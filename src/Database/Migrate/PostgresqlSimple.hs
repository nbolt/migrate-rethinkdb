module Database.Migrate.PostgresqlSimple ( migrate ) where

import Control.Applicative
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.Migrate

data PostgresqlSimple values where
    PostgresqlSimple :: (ToRow values) => Connection -> Query -> [values] -> PostgresqlSimple values

newtype Migrations = Migrations { timestamp :: Integer }

instance FromRow Migrations where
    fromRow = Migrations <$> field


instance (ToRow values) => Migratable (PostgresqlSimple values) where

    migrate t a@(PostgresqlSimple c q vs) = do
        let m = "migration " ++ show t ++ ": "
        exists <- checkT t a
        if exists
          then return $ Result True $ m ++ "already run" -- do we need this?
          else do
              inserted <- insertT t a
              if not inserted
                then return $ Result False $ m ++ "timestamp insertion failed"
                else do
                    success <- executeM a
                    if success
                      then return $ Result True  $ m ++ "success"
                      else return $ Result False $ m ++ "failed"

    executeM (PostgresqlSimple conn query values) = executeMany conn query values >> return True

    checkT t (PostgresqlSimple conn _ _) = do
        results <- query conn "select * from migrations where timestamp = ?" (Only t) :: IO [Migrations]
        return $ not $ null results

    insertT t (PostgresqlSimple conn _ _) = do
        affected <- execute conn "insert into migrations values (?)" (Only t) 
        if affected == 0
          then return False
          else return True

    createTable (PostgresqlSimple conn _ _) = do
        tables <- query_ conn "select * from pg_tables where schemaname='public'"
        if any exists tables
          then return ()
          else execute_ conn "create table migrations (timestamp bigint)" >> return ()
 
      where
        exists :: (String, String, String, Int, Bool, Bool, Bool) -> Bool
        exists (_, name, _, _, _, _, _) = name == "migrations"
    
