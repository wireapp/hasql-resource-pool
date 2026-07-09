module Main where

import BasePrelude
import qualified Hasql.Connection as Connection
import qualified Hasql.Pool as Pool
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import Test.Hspec


testSettings :: Pool.Settings
testSettings =
  ( 1,
    1,
    Pool.ConnectionSettings
      { Pool.host = "localhost",
        Pool.port = 5432,
        Pool.user = "postgres",
        Pool.password = "",
        Pool.dbName = "postgres",
        Pool.connAcqTimeout = 1,
        Pool.txIdleTimeout = Pool.TimeoutSetting 0 Pool.Seconds,
        Pool.stmtTimeout = Pool.TimeoutSetting 0 Pool.Seconds,
        Pool.sslMode = "prefer",
        Pool.sslRootCert = ""
      }
  )


successfulSession :: Session.Session Int64
successfulSession =
  Session.statement () successfulStatement
  where
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
    successfulStatement = Statement.preparable "SELECT 1::int8" Encoders.noParams decoder


runSuccessfulSessionWithConnection :: Connection.Connection -> IO (Either Pool.UsageError Int64)
runSuccessfulSessionWithConnection conn =
  fmap (either (Left . Pool.SessionError) Right) $
    Connection.use conn successfulSession


main = hspec $ do
  describe "Hasql.Pool.use" $ do
    it "releases a spot in the pool when there is an error" $ do
      pool <- Pool.acquire testSettings
      let failingStatement = Statement.preparable "SELEC 1" Encoders.noParams Decoders.noResult
          failingSession = Session.statement () failingStatement
      Pool.use pool failingSession `shouldNotReturn` (Right ())

      Pool.use pool successfulSession `shouldReturn` (Right 1)

    it "times out while waiting for an available pool slot" $ do
      connectionStarted <- newEmptyMVar
      releaseConnection <- newEmptyMVar
      let (_, _, connectionSettings) = testSettings
          connectionGetter = do
            putMVar connectionStarted ()
            takeMVar releaseConnection
            Connection.acquire (Pool.extendedConnectionSettings connectionSettings)
      pool <- Pool.acquireWith connectionGetter testSettings
      holderDone <- newEmptyMVar
      _ <- forkIO $ Pool.use pool successfulSession >>= putMVar holderDone

      takeMVar connectionStarted
      Pool.useWithPoolAcquisitionTimeout 1 pool successfulSession `shouldReturn` (Left Pool.AcquisitionTimeoutUsageError)
      putMVar releaseConnection ()
      takeMVar holderDone `shouldReturn` Right 1

    it "borrows a raw connection from the pool" $ do
      pool <- Pool.acquire testSettings
      Pool.withConnectionWithPoolAcquisitionTimeout 1 pool runSuccessfulSessionWithConnection `shouldReturn` (Right 1)

    it "times out while waiting to borrow a raw connection" $ do
      connectionStarted <- newEmptyMVar
      releaseConnection <- newEmptyMVar
      let (_, _, connectionSettings) = testSettings
          connectionGetter = do
            putMVar connectionStarted ()
            takeMVar releaseConnection
            Connection.acquire (Pool.extendedConnectionSettings connectionSettings)
      pool <- Pool.acquireWith connectionGetter testSettings
      holderDone <- newEmptyMVar
      _ <- forkIO $ Pool.withConnectionWithPoolAcquisitionTimeout 1 pool runSuccessfulSessionWithConnection >>= putMVar holderDone

      takeMVar connectionStarted
      Pool.withConnectionWithPoolAcquisitionTimeout 1 pool runSuccessfulSessionWithConnection `shouldReturn` (Left Pool.AcquisitionTimeoutUsageError)
      putMVar releaseConnection ()
      takeMVar holderDone `shouldReturn` Right 1
