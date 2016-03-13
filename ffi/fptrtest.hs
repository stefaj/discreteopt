{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import 	Foreign.Ptr (Ptr, FunPtr)
import 	Foreign.C
import Control.Monad(liftM)

add x y = x + y
nothing x y = 0
mult x y = x * y
          
foreign import ccall "wrapper" 
	c_createFuncPtr :: (CInt -> CInt -> CInt) -> IO (FunPtr (CInt -> CInt -> CInt))

foreign import ccall "addArgs" c_addArgs 
	:: FunPtr (CInt -> CInt -> CInt) -> CInt -> CInt -> CInt

f :: Char -> Int
f x = x + 1

main = do
	ptr <- c_createFuncPtr mult
	let ans = c_addArgs ptr 5 1
	putStr $ show ans



	
