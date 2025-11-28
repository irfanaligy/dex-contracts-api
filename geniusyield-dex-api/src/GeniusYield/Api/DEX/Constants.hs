module GeniusYield.Api.DEX.Constants (
  poRefsMainnet,
  poRefsPreprod,
  poConfigAddrMainnet,
  poConfigAddrPreprod,
  DEXInfo (..),
  twoRefsPreprod,
  twoRefsMainnet,
  dexInfoDefaultMainnet,
  dexInfoDefaultPreprod,
) where

-- import GeniusYield.Api.DEX.TwoWayOrderConfig (TWORef (..), RefTWOCD (..), fetchTwoWayOrderConfig)

import GeniusYield.Api.DEX.PartialOrderConfig (PORef (..), PORefs (..))
import GeniusYield.Api.DEX.TwoWayOrderConfig (TWORef (..))
import GeniusYield.Scripts (GYCompiledScripts, readCompiledScripts)
import GeniusYield.Scripts.DEX.Version (POCVersion (POCVersion1, POCVersion1_1))
import GeniusYield.Types (GYAddress, unsafeAddressFromText)

-- import Control.Monad.Reader (runReaderT)
-- import PlutusLedgerApi.V1 (Address)
-- import PlutusLedgerApi.V1.Scripts (ScriptHash)
-- import PlutusLedgerApi.V1.Value (AssetClass)
-- import Ply (ScriptRole (..), TypedScript)

poRefsMainnet :: PORefs
poRefsMainnet =
  PORefs
    { porV1 =
        PORef
          { porValRef = "062f97b0e64130bc18b4a227299a62d6d59a4ea852a4c90db3de2204a2cd19ea#2",
            porRefNft = "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.4aff78908ef2dce98bfe435fb3fd2529747b1c4564dff5adebedf4e46d0fc63d",
            porMintRef = "062f97b0e64130bc18b4a227299a62d6d59a4ea852a4c90db3de2204a2cd19ea#1"
          },
      porV1_1 =
        PORef
          { porValRef = "c8adf3262d769f5692847501791c0245068ed5b6746e7699d23152e94858ada7#2",
            porRefNft = "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.682fd5d4b0d834a3aa219880fa193869b946ffb80dba5532abca0910c55ad5cd",
            porMintRef = "c8adf3262d769f5692847501791c0245068ed5b6746e7699d23152e94858ada7#1"
          }
    }

poRefsPreprod :: PORefs
poRefsPreprod =
  PORefs
    { porV1 =
        PORef
          { porValRef = "be6f8dc16d4e8d5aad566ff6b5ffefdda574817a60d503e2a0ea95f773175050#2",
            porRefNft = "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.8309f9861928a55d37e84f6594b878941edce5e351f7904c2c63b559bde45c5c",
            porMintRef = "be6f8dc16d4e8d5aad566ff6b5ffefdda574817a60d503e2a0ea95f773175050#1"
          },
      porV1_1 =
        PORef
          { porValRef = "16647d6365020555d905d6e0edcf08b90a567886f875b40b3d7cec1c70482624#2",
            porRefNft = "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.b5121487c7661f202bc1f95cbc16f0fce720a0a9d3dba63a9f128e617f2ddedc",
            porMintRef = "16647d6365020555d905d6e0edcf08b90a567886f875b40b3d7cec1c70482624#1"
          }
    }

twoRefsMainnet :: TWORef
twoRefsMainnet =
  TWORef
    { tworRefNft = "",
      tworMintRef = "",
      tworSpendRef = "",
      tworFillRef = "",
      tworCancelRef = ""
    }

twoRefsPreprod :: TWORef
twoRefsPreprod =
  TWORef
    { tworRefNft = "fae686ea8f21d567841d703dea4d4221c2af071a6f2b433ff07c0af2.dbcf78371df705da17b8c70e93cb3603ba6b9009ec3389fe319ec0bab1f9406d",
      tworMintRef = "e6f7734e1c65da63b4e1185ed57c80c74291bb072c8a1337bd7e5a6a8f0dbc69#1",
      tworSpendRef = "9432d77b5636390d2af241c92bcaec2af910bff034eea73918953a34b50d33e7#0",
      tworFillRef = "62ba0d7454c8cfa8b49ec9bf079eb561a5728ea59e611d91877a403b0d5153fd#0",
      tworCancelRef = "6f0113dfc55a745046fa0701debb1472e01ea6001ad20b94934abea359baaa33#0"
    }

poConfigAddrMainnet :: POCVersion -> GYAddress
poConfigAddrMainnet =
  let v1Addr = unsafeAddressFromText "addr1w9zr09hgj7z6vz3d7wnxw0u4x30arsp5k8avlcm84utptls8uqd0z"
      v1_1Addr = unsafeAddressFromText "addr1wxcqkdhe7qcfkqcnhlvepe7zmevdtsttv8vdfqlxrztaq2gge58rd"
   in \case
        POCVersion1 -> v1Addr
        POCVersion1_1 -> v1_1Addr

poConfigAddrPreprod :: POCVersion -> GYAddress
poConfigAddrPreprod =
  let v1Addr = unsafeAddressFromText "addr_test1wrgvy8fermjrruaf7fnndtmpuw4xx4cnvfqjp5zqu8kscfcvh32qk"
      v1_1Addr = unsafeAddressFromText "addr_test1wqzy2cay2twmcq68ypk4wjyppz6e4vjj4udhvkp7dfjet2quuh3la"
   in \case
        POCVersion1 -> v1Addr
        POCVersion1_1 -> v1_1Addr

data DEXInfo = DEXInfo
  { dexPORefs :: !PORefs,
    dexTWORefs :: !TWORef,
    dexScripts :: !GYCompiledScripts
    -- , dexRefCfg  :: !RefTWOCD
  }

dexInfoDefaultMainnet :: IO DEXInfo
dexInfoDefaultMainnet = do
  gycs <- readCompiledScripts
  -- refCfg <- runReaderT (fetchTwoWayOrderConfig (tworRefNft twoRefsMainnet)) gycs
  return $
    DEXInfo
      { dexPORefs = poRefsMainnet,
        dexTWORefs = twoRefsMainnet,
        dexScripts = gycs
        -- , dexRefCfg  = refCfg
      }

dexInfoDefaultPreprod :: IO DEXInfo
dexInfoDefaultPreprod = do
  gycs <- readCompiledScripts
  -- refCfg <- runReaderT (fetchTwoWayOrderConfig (tworRefNft twoRefsPreprod)) gycs
  return $
    DEXInfo
      { dexPORefs = poRefsPreprod,
        dexTWORefs = twoRefsPreprod,
        dexScripts = gycs
        -- , dexRefCfg  = refCfg
      }
