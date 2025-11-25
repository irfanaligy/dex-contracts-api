{-# LANGUAGE QuasiQuotes #-}

module GeniusYield.Api.TokenSale.Utils
  ( whitelistSqlQuery
  , roundDetail
  , TokenRoundInfo (..)
  )
where

import Data.Text qualified as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ (sql)

whitelistSqlQuery :: Query
whitelistSqlQuery = sqlQuery
  where
    sqlQuery :: Query
    sqlQuery =
      [sql|
      select tx.tx_hash, tx.index, u.wallet_stake_key_hash from round as r
      inner join order_sale os on r.round_id = os.round_id
      inner join order_sale_event ose on os.order_sale_id = ose.order_sale_id
      inner join transaction_output tx on ose.event_id = tx.event_id
      inner join "user" u on os.user_id = u.user_id
      inner join user_kyc user_kyc on u.user_id = user_kyc.user_id
        where
          user_kyc.review_result = 'GREEN' and
        ose.event_type = 'OPEN' and
        tx.event_id IS NOT NULL and
        r.script_address=?;
    |]

data TokenRoundInfo = TokenRoundInfo
  { reTotalSupply :: Integer
  , reMaxAllocation :: Integer
  }
  deriving Show

instance FromRow TokenRoundInfo where
  fromRow = TokenRoundInfo <$> field <*> field

roundDetail :: Connection -> T.Text -> IO TokenRoundInfo
roundDetail conn scriptAddr = do
  [res] <- query conn "select base_asset_allocation_amount, order_base_asset_max_allocation from round where script_address=? LIMIT 1;" [scriptAddr]
  return res
