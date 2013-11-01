module Database.Migrate.RethinkDB ( migrate, RethinkDBMigrate(..) ) where


import qualified Data.HashMap.Strict as HM
import           Data.Aeson.Types
import qualified Data.Text as T
import           Control.Applicative
import qualified Database.RethinkDB as R
import           Database.Migrate
import           Control.Monad.State


data RethinkDBMigrate = Handle { handle :: R.RethinkDBHandle } | Query { query :: R.ReQL }

instance Migratable RethinkDBMigrate where
    executeM q h = do
        lift $ R.run' (handle h) (query q)
        return True

    checkT t h = do
        timestamp <- lift $ R.run' (handle h) $ R.get (R.asNumber t) $ R.Table Nothing "migrations" (Just "timestamp")
        case head timestamp of
            R.JSON Null       -> return False
            R.JSON (Object _) -> return True

    insertT t h = do
        rsp <- lift $ R.run' (handle h) $ R.insert (object $ []) $ R.Table Nothing "migrations" (Just "timestamp")
        case head rsp of
            R.JSON (Object rsp) -> do
                let Just ins = HM.lookup (T.pack "inserted") rsp
                case ins of
                    Number inserted ->
                        if inserted >= 1
                          then return True
                          else return False

    createTable = do
        let table = R.Table Nothing "migrations" (Just "timestamp")
        h <- get
        lift $ R.run' (handle h) (R.tableCreate table R.def)
        return True
