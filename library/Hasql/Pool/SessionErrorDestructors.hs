-- Copied from https://github.com/nikita-volkov/hasql-pool/blob/50b49864afc796fd7df6e08706d3a628c656dcf0/src/library/Hasql/Pool/SessionErrorDestructors.hs
module Hasql.Pool.SessionErrorDestructors where

import Data.Text
import qualified Hasql.Errors as Errors
import Hasql.Pool.Prelude

requiresConnectionDiscard :: Errors.SessionError -> Bool
requiresConnectionDiscard = \case
  Errors.ConnectionSessionError {} -> True
  Errors.MissingTypesSessionError {} -> True
  Errors.ScriptSessionError _ serverError -> isStaleServerError serverError
  Errors.StatementSessionError _ _ _ _ _ statementError -> statementRequiresConnectionDiscard statementError
  -- Driver errors indicate that Hasql or the server left the connection in an
  -- unexpected state. In particular, Hasql closes the libpq connection when
  -- cleanup after an interruption fails, so it must not be reused by the pool.
  Errors.DriverSessionError {} -> True

discardDetails :: Errors.SessionError -> Maybe Text
discardDetails err =
  if requiresConnectionDiscard err
    then Just $ Errors.toMessage err
    else Nothing

statementRequiresConnectionDiscard :: Errors.StatementError -> Bool
statementRequiresConnectionDiscard = \case
  Errors.ServerStatementError serverError -> isStaleServerError serverError
  Errors.UnexpectedColumnTypeStatementError {} -> True
  _ -> False

isStaleServerError :: Errors.ServerError -> Bool
isStaleServerError (Errors.ServerError code _ _ _ _) =
  code == "0A000" || code == "XX000"
