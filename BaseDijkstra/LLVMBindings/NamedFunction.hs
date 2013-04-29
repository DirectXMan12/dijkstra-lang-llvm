module BaseDijkstra.LLVMBindings.NamedFunction (getNamedFunctionFromModule) where

import Foreign.C.Types
import Foreign.C.String
import LLVM.FFI.Core hiding (Module)
import LLVM.Core

getNamedFunctionFromModule :: Module -> String -> IO ValueRef
getNamedFunctionFromModule mod fn =
  withCString fn $ \nameStr -> getNamedFunction (fromModule mod) nameStr
