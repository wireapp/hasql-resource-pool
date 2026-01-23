{-# LANGUAGE OverloadedRecordDot #-}
module Hasql.Pool
(   Pool
,   PoolSize
,   Settings
,   ConnectionSettings(..)
,   UsageError(..)
,   ConnectionGetter
,   Stats(..)
,   TimeoutSetting(..)
,   TimeUnit(..)
,   errorToDetailedMsg
,   errorIsTransient
,   stats
,   getPoolUsageStat
,   acquire
,   acquireWith
,   release
,   use
,   useWithObserver
,   withResourceOnEither
,   extendedConnectionSettings
)
where

import qualified    Data.Pool                                   as ResourcePool
import qualified    Data.Text                                   as T
import qualified    Data.Pool.Internal                          as Unstable
import              System.Clock                                (Clock(Monotonic), diffTimeSpec, getTime, toNanoSecs)

import              Hasql.Pool.Prelude
import qualified    Hasql.Connection
import qualified    Hasql.Connection.Settings
import qualified    Hasql.Errors
import qualified    Hasql.Session
import              Hasql.Pool.Observer                         (Observed(..), ObserverAction)


-- |
-- A pool of open DB connections.
newtype Pool =
    Pool (ResourcePool.Pool (Either Hasql.Errors.ConnectionError Hasql.Connection.Connection))



type PoolSize         = Int
type ResidenceTimeout = NominalDiffTime

-- |
-- Connection getter action that allows for obtaining Postgres connection settings
-- via external resources such as AWS tokens etc.
type ConnectionGetter = IO (Either Hasql.Errors.ConnectionError Hasql.Connection.Connection)

-- |
-- Settings of the connection pool. Consist of:
--
-- * Pool-size.
--
-- * Timeout.
-- An amount of time for which an unused resource is kept open.
-- The smallest acceptable value is 0.5 seconds.
--
-- * Connection settings.
--
type Settings = (PoolSize, ResidenceTimeout, ConnectionSettings)


data TimeUnit
    =  Microseconds
    |  Milliseconds
    |  Seconds
    |  Minutes
    |  Hours
    |  Days

-- https://www.postgresql.org/docs/18/config-setting.html#CONFIG-SETTING-NAMES-VALUES
instance Show TimeUnit where
    show Microseconds   = "us"
    show Milliseconds   = "ms"
    show Seconds        = "s"
    show Minutes        = "min"
    show Hours          = "h"
    show Days           = "d"


data TimeoutSetting = TimeoutSetting Word16 TimeUnit

instance Show TimeoutSetting where
    show (TimeoutSetting v u) = show v <> show u


-- | Extended connection settings
data ConnectionSettings = ConnectionSettings
    {   host                :: T.Text
    ,   port                :: Word16
    ,   user                :: T.Text
    ,   password            :: T.Text
    ,   dbName              :: T.Text
    ,   connAcqTimeout      :: Word16               -- ^ In seconds: zero, negative, or not specified means wait indefinitely. Doesn't support unit suffixes.
    ,   txIdleTimeout       :: TimeoutSetting       -- ^ Sets explicit `idle_in_transaction_session_timeout`: zero, negative, or not specified means wait indefinitely.
    ,   stmtTimeout         :: TimeoutSetting       -- ^ Sets explicit `statement_timeout`: zero, negative, or not specified means wait indefinitely.
    ,   sslMode             :: T.Text               -- ^ See https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-CONNECT-SSLMODE
    ,   sslRootCert         :: T.Text               -- ^ See https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-CONNECT-SSLROOTCERT
    }

-- | https://www.postgresql.org/docs/18/libpq-connect.html#LIBPQ-CONNECT-CONNECT-TIMEOUT
connectTimeout      = Hasql.Connection.Settings.other "connect_timeout"

-- | https://www.postgresql.org/docs/18/runtime-config-client.html#GUC-TRANSACTION-TIMEOUT
serverOptions = Hasql.Connection.Settings.other "options"

-- | https://www.postgresql.org/docs/18/libpq-connect.html#LIBPQ-CONNECT-SSLMODE
sslmode             = Hasql.Connection.Settings.other "sslmode"

-- | https://www.postgresql.org/docs/18/libpq-connect.html#LIBPQ-CONNECT-SSLROOTCERT
sslrootcert         = Hasql.Connection.Settings.other "sslrootcert"


-- |
-- Given the pool-size, timeout and connection settings
-- create a connection-pool.
acquire :: Settings -> IO Pool
acquire settings@(_, _, cset) =
    acquireWith
        (Hasql.Connection.acquire . extendedConnectionSettings $ cset)
        settings


-- | Produce connection settings suitable for acquiring a connection, from an extended set of parameters covering ssl options.
extendedConnectionSettings :: ConnectionSettings -> Hasql.Connection.Settings.Settings
extendedConnectionSettings cset = 
    foldl' (<>) mempty
        [   Hasql.Connection.Settings.hostAndPort               cset.host cset.port
        ,   Hasql.Connection.Settings.user                      cset.user
        ,   Hasql.Connection.Settings.password                  cset.password
        ,   Hasql.Connection.Settings.dbname                    cset.dbName
        ,   (connectTimeout . T.pack . show)                    cset.connAcqTimeout                
        ,   sslmode                                             cset.sslMode
        ,   sslrootcert                                         cset.sslRootCert
        ,   serverOptions
                (serverOpts
                        -- https://www.postgresql.org/docs/18/runtime-config-client.html#GUC-IDLE-IN-TRANSACTION-SESSION-TIMEOUT
                    [   ("idle_in_transaction_session_timeout", T.pack $ show cset.txIdleTimeout)
                        -- https://www.postgresql.org/docs/18/runtime-config-client.html#GUC-STATEMENT-TIMEOUT
                    ,   ("statement_timeout",                   T.pack $ show cset.stmtTimeout)
                    ]
                )
        ]


serverOpts :: [(T.Text,  T.Text)] -> T.Text
serverOpts = foldl' (\acc (k ,v) -> acc <> " -c " <> k <> "=" <> v) mempty


-- |
-- Similar to 'acquire', allows for finer configuration.
acquireWith :: ConnectionGetter
            -> Settings
            -> IO Pool
acquireWith connGetter (maxSize, sTimeout, _connectionSettings) =
    Pool <$> createPool connGetter releaseConn sTimeout maxSize
    where
        releaseConn = either (const (pure ())) Hasql.Connection.release


createPool :: IO a
           -> (a -> IO ())
           -> NominalDiffTime
           -> PoolSize
           -> IO (ResourcePool.Pool a)
createPool create free idleTime maxResources = ResourcePool.newPool cfg where
    -- defaultPoolConfig create free cacheTTL maxResources = PoolConfig
    cfg = ResourcePool.defaultPoolConfig create free (realToFrac idleTime) maxResources


-- |
-- Release the connection-pool by closing and removing all connections.
release :: Pool -> IO ()
release (Pool pool) =
    ResourcePool.destroyAllResources pool


-- |
-- A union over the connection establishment error and the session error.
data UsageError
    =   ConnectionError Hasql.Errors.ConnectionError
    |   SessionError    Hasql.Errors.SessionError
    deriving (Show, Eq)

-- |
-- Use a connection from the pool to run a session and
-- return the connection to the pool, when finished.
use :: Pool -> Hasql.Session.Session a -> IO (Either UsageError a)
use = useWithObserver Nothing

-- |
-- Same as 'use' but allows for a custom observer action. You can use it for gathering latency metrics.
useWithObserver :: Maybe ObserverAction
                -> Pool
                -> Hasql.Session.Session a
                -> IO (Either UsageError a)
useWithObserver observer (Pool pool) session =
    fmap (either (Left . ConnectionError) (either (Left . SessionError) Right)) $
    withResourceOnEither pool $
    traverse runQuery
    where
        runQuery dbConn = maybe action (runWithObserver action) observer
            where
                action = Hasql.Connection.use dbConn session

        runWithObserver action doObserve = do
            let measure = getTime Monotonic
            start  <- measure
            result <- action
            end    <- measure
            let nsRatio  = 1000000000
                observed = Observed {   latency = toRational (toNanoSecs (end `diffTimeSpec` start) % nsRatio)
                                    }
            doObserve observed >> pure result


withResourceOnEither :: ResourcePool.Pool resource
                     -> (resource -> IO (Either failure success))
                     -> IO (Either failure success)
withResourceOnEither pool act = mask_ $ do
    (resource, localPool) <- ResourcePool.takeResource pool
    failureOrSuccess      <- act resource `onException` ResourcePool.destroyResource pool localPool resource
    case failureOrSuccess of
        Right success -> do
            ResourcePool.putResource localPool resource
            pure $ Right success
        Left failure -> do
            ResourcePool.destroyResource pool localPool resource
            pure $ Left failure


data Stats = Stats
    {   currentUsage  :: !Int
        -- ^ Current number of items.
    ,   available     :: !Int
        -- ^ Total items available for consumption.
    } deriving Show


stats :: Pool -> IO Stats
stats (Pool pool) = currentlyAvailablePerStripe >>= collect where
    -- attributes extraction and counting
    collect xs = pure $ Stats inUse avail where
        inUse = maxResources - avail
        avail = sum xs

    currentlyAvailablePerStripe = traverse id peekAvailable
    peekAvailable               = (fmap stripeAvailability) <$> allStripes    -- array of IO Int
    stripeAvailability ms       = Unstable.available ms                       -- stripe is always initialized with value, as it's from TVar
    allStripes                  = peekStripe <$> Unstable.localPools pool     -- array of IO vals
    peekStripe                  = readTVarIO . Unstable.stripeVar

    -- data from the pool
    maxResources                = Unstable.poolMaxResources . Unstable.poolConfig $ pool
    _quotaPerStripe             = maxResources `quotCeil` _numStripes
    _numStripes                 = length $ Unstable.localPools pool  -- can be 'sizeofSmallArray' but requires 'primitive' as dependency
    quotCeil x y                = let (z, r) = x `quotRem` y in if r == 0 then z else z + 1  -- copied from 'Data.Pool.Internal'


getPoolUsageStat :: Pool -> IO PoolSize
getPoolUsageStat pool = currentUsage <$> stats pool


errorToDetailedMsg = Hasql.Errors.toDetailedText
errorIsTransient   = Hasql.Errors.isTransient
