module BaseDijkstra.LLVMBindings.ReadAssembly (readAssemblyFromString) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import LLVM.FFI.AssemblyReader
import LLVM.Core hiding (alloca)
import Foreign.Storable (peek)
import Debug.Trace

readAssemblyFromString :: String -> IO Module
readAssemblyFromString code =
  let codelen = (fromIntegral $ length code) :: CInt
  in
    withCString code $ \namePtr ->
      alloca $ \errStr -> do
        res <- getModuleFromAssembly namePtr codelen errStr
        return $ Module res
