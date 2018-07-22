{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Asterius.MemoryTrap
  ( addMemoryTrap
  , addMemoryTrapDeep
  ) where

import Asterius.Builtins
import Asterius.Types
import Data.Data (Data, gmapT)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Type.Reflection

addMemoryTrap :: AsteriusModule -> AsteriusModule
addMemoryTrap m =
  m
    { functionMap =
        HM.mapWithKey
          (\func_sym func ->
             if func_sym `V.elem`
                [ "_get_Sp"
                , "_get_SpLim"
                , "_get_Hp"
                , "_get_HpLim"
                , "__asterius_memory_trap"
                ]
               then func
               else addMemoryTrapDeep func)
          (functionMap m)
    }

addMemoryTrapDeep :: Data a => a -> a
addMemoryTrapDeep t =
  case eqTypeRep (typeOf t) (typeRep :: TypeRep Expression) of
    Just HRefl ->
      case t of
        MemoryTrapped {} -> t
        Load {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} ->
          MemoryTrapped
            { trappedExpression =
                Block
                  { name = ""
                  , bodys =
                      V.fromList $
                      [ UnresolvedSetLocal
                          { unresolvedLocalReg = LoadStoreI64Ptr
                          , value = addMemoryTrapDeep i64_ptr
                          }
                      , UnresolvedSetLocal
                          { unresolvedLocalReg = LoadStoreValue valueType
                          , value =
                              t
                                { ptr =
                                    Unary {unaryOp = WrapInt64, operand0 = p}
                                }
                          }
                      ] <>
                      [ CallImport
                        { target' = "__asterius_load_i64"
                        , operands = cutI64 p <> cutI64 (v valueType)
                        , valueType = None
                        }
                      | valueType == I64
                      ] <>
                      [ Call
                          { target = "__asterius_memory_trap"
                          , operands = [p]
                          , valueType = None
                          }
                      , v valueType
                      ]
                  , valueType = valueType
                  }
            }
        Store {ptr = Unary {unaryOp = WrapInt64, operand0 = i64_ptr}, ..} ->
          MemoryTrapped
            { trappedExpression =
                Block
                  { name = ""
                  , bodys =
                      V.fromList $
                      [ UnresolvedSetLocal
                          { unresolvedLocalReg = LoadStoreI64Ptr
                          , value = addMemoryTrapDeep i64_ptr
                          }
                      , UnresolvedSetLocal
                          { unresolvedLocalReg = LoadStoreValue valueType
                          , value = addMemoryTrapDeep value
                          }
                      , t
                          { ptr = Unary {unaryOp = WrapInt64, operand0 = p}
                          , value = v valueType
                          }
                      ] <>
                      [ CallImport
                        { target' = "__asterius_store_i64"
                        , operands = cutI64 p <> cutI64 (v valueType)
                        , valueType = None
                        }
                      | valueType == I64
                      ] <>
                      [ Call
                          { target = "__asterius_memory_trap"
                          , operands = [p]
                          , valueType = None
                          }
                      ]
                  , valueType = None
                  }
            }
        _ -> go
    _ -> go
  where
    p = UnresolvedGetLocal {unresolvedLocalReg = LoadStoreI64Ptr}
    v vt = UnresolvedGetLocal {unresolvedLocalReg = LoadStoreValue vt}
    go = gmapT addMemoryTrapDeep t
