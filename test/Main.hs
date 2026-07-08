module Main where

import BasePrelude
import qualified Hasql.Pool as Pool
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import Test.Hspec


main = hspec $ do
  describe "Hasql.Pool.use" $ do
    it "releases a spot in the pool when there is an error" $ do
      pool <-
        Pool.acquire
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
      let failingStatement = Statement.preparable "SELEC 1" Encoders.noParams Decoders.noResult
          failingSession = Session.statement () failingStatement
      Pool.use pool failingSession `shouldNotReturn` (Right ())

      let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
          successfulStatement = Statement.preparable "SELECT 1" Encoders.noParams decoder
          successfulSession = Session.statement () successfulStatement
      Pool.use pool successfulSession `shouldReturn` (Right 1)
