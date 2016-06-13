{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Foldable            (for_)
import           Data.Proxy
import           Data.Text
import qualified System.Directory         as Dir
import           System.Environment       (getArgs)
import           System.FilePath          ((</>))
import           Servant.Matlab
import qualified Servant.JS               as JS
import qualified Servant.Matlab           as Matlab
import qualified Servant.Matlab.Functions as Matlab

import Tagging.API


------------------------------------------------------------------------------
matlabLibrary :: [(Text,Text)]
matlabLibrary = []
--  Matlab.matlabForAPI
--  (Proxy :: Proxy TaggingAPI)
--  Matlab.matlabFunctions

jsLibrary :: Text
jsLibrary = undefined
  -- JS.jsForAPI (Proxy :: Proxy TaggingAPI)
  -- JS.vanillaJS

main :: IO ()
main =
  getArgs >>= \case
    [d] -> do
      let matlabDir = d </> "tagging-matlab"
      Dir.createDirectoryIfMissing True (d </> "tagging-matlab")
      for_ matlabLibrary $ \(fn,f) -> writeFile (matlabDir </> unpack fn) (unpack f)
      writeFile (d </> "tagging.js") (unpack jsLibrary)
    _ -> putStrLn "Call with one argument: directory to build all clients"
