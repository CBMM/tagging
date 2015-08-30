{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE LambdaCase   #-}


module Tagging.Crud where

import           Data.Bool
import           Data.Char (toLower)
import           Data.Functor
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import qualified Data.IntMap as I
import           Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import Data.Traversable
import GHC.Int
import Reflex.Dom
import Reflex.Dom.Xhr
import Reflex.Dynamic.TH

import Tagging.User
import Tagging.Stimulus
import Tagging.Response

class (A.FromJSON v, A.ToJSON v) => Crud v where

  resourceName :: Proxy v -> String

  resourceWidget :: MonadWidget t m => v -> Bool -> m (Dynamic t v)

  getEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t (Maybe v))
  getEntity p ks = getAndDecode (fmap toApi ks)
      where toApi k = ("api/" <> resourceName p <> "/" <> show k)

  getAllEntities :: MonadWidget t m => Proxy v -> Event t () -> m (Event t (I.IntMap v))
  getAllEntities p triggers = do
    mJson <- getAndDecode (triggers $> "api/" <> map toLower (resourceName p))
    --return $ (Map.fromList . toList) <$> mJson
    return $ fforMaybe mJson id

  postEntity :: MonadWidget t m => Event t v ->  m (Event t Int64)

  putEntity :: MonadWidget t m => Event t (Int64,v) -> m (Event t ())

  deleteEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t ())



-----------------------------------------------------------------------------
crudTableWidget :: forall t m v.(MonadWidget t m, Crud v)
  => Proxy v
  -> Dynamic t (v -> Bool)
  -> m (Event t ())
crudTableWidget p dynValidate = mdo
  pl <- getPostBuild
  let updateEvents = leftmost [pl, () <$ tableEvents]
  --let updateEvents = () <$ tableEvents
  vMap <- holdDyn mempty =<< getAllEntities p updateEvents
  tableEvents <- elClass "table" "crud-table" $ do
    dynText =<< mapDyn (show . length) vMap
    --forDyn vMap (\vs -> text (show $ length vs))
    return never
    --newRowEvents      <- newRowWidget p

    --existingRowsEvents <- listViewWithKey vMap crudRowWidget
    -- existingRowEvents <- forDyn vMap $ \vs -> do
    --                        let rs <- map crudRowWidget' (Map.toList vs)  :: m [Event t ()]
    --                        return rs
    --existingRowsEvents <- forDyn vMap $ (\vs -> crudRowWidget' (head $ Map.toList vs))
    --return $ updated existingRowsEvents
    --vList <- mapDyn Map.toList vMap :: MonadWidget t m => Dynamic t [(Int64,v)]
    --vRows <- mapDyn (mapM crudRowWidget) vList

    --let existingRowsEvents = leftmost vRows
    --existingRowsEvents <- updated <$> mapDyn (mapM crudRowWidget . Map.toList) vMap
    --return (newRowEvents <> existingRowEvents)
    --return existingRowsEvents
  return $ () <$ tableEvents


-----------------------------------------------------------------------------
-- crudRowWidget :: (MonadWidget t m, Crud v)
--               => Int64
--               -> Dynamic t v
--               -> m (Event t ())
-- crudRowWidget k v = do
--   el "tr" $ do
--     text (show k)
--     dynVal <- resourceWidget v False
--     return $ () <$ updated dynVal


crudRowWidget' :: (MonadWidget t m, Crud v)
               => (Int64, v)
               -> m (Event t ())
crudRowWidget' (k, v) = do
  el "tr" $ do
    text (show k)
    dynVal <- resourceWidget v False
    return $ () <$ updated dynVal

instance Crud StimulusResource where
  resourceName _ = "StimulusResource"
  resourceWidget v0 b = do
    let attrs = s "disabled" =: s (bool "false" "true" b)
    f1 <- el "td" $
          _textInput_value <$> textInput
           def { _textInputConfig_initialValue = T.unpack (srName v0)}
    f2 <- el "td" $
          _textInput_value <$> textInput
           def {_textInputConfig_initialValue = T.unpack (srUrlSuffix v0)}
    f3 <- el "td" $
          _textInput_value <$> textInput
           def { _textInputConfig_initialValue = T.unpack (srMimeType v0)}
    $( qDyn [| StimulusResource
                        (T.pack $(unqDyn [|f1|]))
                        (T.pack $(unqDyn [|f2|]))
                        (T.pack $(unqDyn [|f3|])) |] )


instance Crud TaggingUser where
  resourceName _ = "TaggingUser"
  resourceWidget  _ = undefined

s :: String -> String
s = id
