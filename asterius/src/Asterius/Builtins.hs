{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Asterius.Builtins
  ( BuiltinsOptions(..)
  , getDefaultBuiltinsOptions
  , rtsAsteriusModuleSymbol
  , rtsAsteriusModule
  , fnTypeName
  , fnType
  , globalRegName
  , tsoSymbol
  , tsoInfoSymbol
  , stackSymbol
  , stackInfoSymbol
  , bdescrSymbol
  , capabilitySymbol
  , eagerBlackholeInfoSymbol
  , stopThreadInfoSymbol
  , gcEnter1Symbol
  , gcFunSymbol
  , stgRunSymbol
  , stgReturnSymbol
  , sizeof_bdescr
  , tsoStatics
  , stackStatics
  , bdescrStatics
  , baseRegStatics
  , capabilityStatics
  , stgReturnFunction
  ) where

import Asterius.BuildInfo
import Asterius.Internals
import Asterius.Types
import Control.Exception
import qualified Data.ByteString.Short as SBS
import Data.String
import qualified Data.Vector as V
import Foreign
import Foreign.C
import qualified GHC
import qualified GhcPlugins as GHC
import Prelude hiding (IO)

foreign import capi "Rts.h value BDESCR_SIZE" sizeof_bdescr :: CInt

data BuiltinsOptions = BuiltinsOptions
  { dflags :: GHC.DynFlags
  , stackSize, nurserySize :: Int
  }

getDefaultBuiltinsOptions :: IO BuiltinsOptions
getDefaultBuiltinsOptions =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
  GHC.runGhc (Just ghcLibDir) $ do
    _ <- GHC.getSessionDynFlags >>= GHC.setSessionDynFlags
    dflags <- GHC.getSessionDynFlags
    pure
      BuiltinsOptions
        {dflags = dflags, stackSize = 1024576, nurserySize = 1024576}

rtsAsteriusModuleSymbol :: AsteriusModuleSymbol
rtsAsteriusModuleSymbol =
  AsteriusModuleSymbol
    { unitId = SBS.toShort $ GHC.fs_bs $ GHC.unitIdFS GHC.rtsUnitId
    , moduleName = ["Asterius"]
    }

rtsAsteriusModule :: BuiltinsOptions -> AsteriusModule
rtsAsteriusModule opts =
  mempty
    { staticsMap = [(capabilitySymbol, capabilityStatics opts)]
    , functionMap = [(stgReturnSymbol, stgReturnFunction opts)]
    }

fnTypeName :: SBS.ShortByteString
fnTypeName = "_asterius_FN"

fnType :: FunctionType
fnType = FunctionType {returnType = I64, paramTypes = []}

globalRegName :: UnresolvedGlobalReg -> SBS.ShortByteString
globalRegName gr =
  case gr of
    VanillaReg i -> fromString $ "_asterius_R" <> show i
    FloatReg i -> fromString $ "_asterius_F" <> show i
    DoubleReg i -> fromString $ "_asterius_D" <> show i
    LongReg i -> fromString $ "_asterius_L" <> show i
    Sp -> "_asterius_Sp"
    SpLim -> "_asterius_SpLim"
    Hp -> "_asterius_Hp"
    HpLim -> "_asterius_HpLim"
    CurrentNursery -> "_asterius_CurrentNursery"
    HpAlloc -> "_asterius_HpAlloc"
    BaseReg -> "_asterius_BaseReg"
    _ -> throw $ AssignToImmutableGlobalReg gr

tsoSymbol, tsoInfoSymbol, stackSymbol, stackInfoSymbol, bdescrSymbol, capabilitySymbol, eagerBlackholeInfoSymbol, stopThreadInfoSymbol, gcEnter1Symbol, gcFunSymbol, stgRunSymbol, stgReturnSymbol ::
     AsteriusEntitySymbol
tsoSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_TSO"}

tsoInfoSymbol =
  AsteriusEntitySymbol {entityKind = StaticsEntity, entityName = "stg_TSO_info"}

stackSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_Stack"}

stackInfoSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "stg_STACK_info"}

bdescrSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_bdescr"}

capabilitySymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "_asterius_Capability"}

eagerBlackholeInfoSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "__stg_EAGER_BLACKHOLE_info"}

stopThreadInfoSymbol =
  AsteriusEntitySymbol
    {entityKind = StaticsEntity, entityName = "stg_stop_thread_info"}

gcEnter1Symbol =
  AsteriusEntitySymbol
    {entityKind = FunctionEntity, entityName = "__stg_gc_enter_1"}

gcFunSymbol =
  AsteriusEntitySymbol
    {entityKind = FunctionEntity, entityName = "__stg_gc_fun"}

stgRunSymbol =
  AsteriusEntitySymbol {entityKind = FunctionEntity, entityName = "StgRun"}

stgReturnSymbol =
  AsteriusEntitySymbol {entityKind = FunctionEntity, entityName = "StgReturn"}

asteriusStaticSize :: AsteriusStatic -> Int
asteriusStaticSize s =
  case s of
    Uninitialized l -> l
    Serialized buf -> SBS.length buf
    _ -> 8

layoutStatics :: [(Int, AsteriusStatic)] -> AsteriusStatics
layoutStatics ss = AsteriusStatics {asteriusStatics = snd $ f ss (0, [])}
  where
    f :: [(Int, AsteriusStatic)]
      -> (Int, V.Vector AsteriusStatic)
      -> (Int, V.Vector AsteriusStatic)
    f [] r = r
    f ((x_offset, x_static):xs) (tot_len, tot_l) =
      f
        xs
        ( x_offset + asteriusStaticSize x_static
        , case x_offset - tot_len of
            0 -> tot_l <> [x_static]
            delta -> tot_l <> [Uninitialized delta, x_static])

tsoStatics, stackStatics, bdescrStatics, baseRegStatics, capabilityStatics ::
     BuiltinsOptions -> AsteriusStatics
tsoStatics BuiltinsOptions {..} =
  layoutStatics
    [ (0, UnresolvedStatic tsoInfoSymbol)
    , (GHC.oFFSET_StgTSO_stackobj dflags, UnresolvedStatic stackSymbol)
    , ( GHC.oFFSET_StgTSO_alloc_limit dflags
      , Serialized (encodePrim (maxBound :: Int64)))
    ]

stackStatics BuiltinsOptions {..} =
  layoutStatics
    [ (0, UnresolvedStatic stackInfoSymbol)
    , ( GHC.oFFSET_StgStack_sp dflags
      , UnresolvedOffStatic stackSymbol (GHC.oFFSET_StgStack_stack dflags))
    , (GHC.oFFSET_StgStack_stack dflags, Uninitialized stackSize)
    ]

bdescrStatics BuiltinsOptions {..} =
  layoutStatics
    [ (GHC.oFFSET_bdescr_start dflags, undefined)
    , (GHC.oFFSET_bdescr_free dflags, undefined)
    , (GHC.oFFSET_bdescr_flags dflags, Serialized (encodePrim (0 :: Word16)))
    , (GHC.oFFSET_bdescr_blocks dflags, Serialized (encodePrim (1 :: Word32)))
    ]

baseRegStatics BuiltinsOptions {..} =
  layoutStatics
    [ (GHC.oFFSET_StgRegTable_rCurrentTSO dflags, UnresolvedStatic tsoSymbol)
    , ( GHC.oFFSET_StgRegTable_rCurrentNursery dflags
      , UnresolvedStatic bdescrSymbol)
    ]

capabilityStatics opts@BuiltinsOptions {..} =
  AsteriusStatics
    { asteriusStatics =
        asteriusStatics
          (layoutStatics
             [ ( GHC.oFFSET_stgEagerBlackholeInfo dflags +
                 GHC.oFFSET_Capability_r dflags
               , UnresolvedStatic eagerBlackholeInfoSymbol)
             , ( GHC.oFFSET_stgGCEnter1 dflags + GHC.oFFSET_Capability_r dflags
               , UnresolvedStatic gcEnter1Symbol)
             , ( GHC.oFFSET_stgGCFun dflags + GHC.oFFSET_Capability_r dflags
               , UnresolvedStatic gcFunSymbol)
             ]) <>
        asteriusStatics (baseRegStatics opts)
    }

stgReturnFunction :: BuiltinsOptions -> Function
stgReturnFunction _ =
  Function
    {functionTypeName = fnTypeName, varTypes = [], body = Return $ ConstI64 0}
