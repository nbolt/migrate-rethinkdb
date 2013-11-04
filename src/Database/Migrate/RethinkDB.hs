module Database.Migrate.RethinkDB ( migrate, RethinkDBMigrate(..) ) where


import qualified Data.HashMap.Strict as HM
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V
import           Control.Applicative
import qualified Database.RethinkDB as R
import qualified Database.RethinkDB.Network as R
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
        h <- get
        tables <- lift $ R.run' (handle h) $ R.tableList $ R.rdbDatabase $ handle h
        let table = R.Table Nothing "migrations" (Just "timestamp")
        if elem "migrations" $ concat $ map (\(R.JSON (Array a)) -> map (\(String n) -> T.unpack n) (V.toList a)) tables
          then return True
          else do
            lift $ R.run' (handle h) (R.tableCreate table R.def)
            return True

    createDB = do
        h   <- get
        dbs <- liftIO $ R.run' (handle h) R.dbList
        let dbName = T.unpack $ R.databaseName $ R.rdbDatabase $ handle h
        if elem dbName $ concat $ map (\(R.JSON (Array a)) -> map (\(String n) -> T.unpack n) (V.toList a)) dbs
          then return True
          else do
            lift $ R.run' (handle h) (R.dbCreate dbName)
            return True
