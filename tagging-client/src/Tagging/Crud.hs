{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeFamilies   #-}


module Tagging.Crud where

import           Data.Functor
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable
import           Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import GHC.Int
import Reflex.Dom
import Reflex.Dom.Xhr
import Reflex.Dynamic.TH

import Tagging.User
import Tagging.Stimulus
import Tagging.Response

class (A.FromJSON v, A.ToJSON v) => Crud v where

  resourceName :: Proxy v -> String

  inputWidget :: MonadWidget t m => Proxy v -> m (Dynamic t v) -- TODO: Proxy is needed?

  getEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t (Maybe v))
  getEntity p ks = getAndDecode (fmap toApi ks)
      where toApi k = ("api/" <> resourceName p <> "/" <> show k)

  getAllEntities :: MonadWidget t m => Proxy v -> Event t () -> m (Event t (Map Int64 v))
  getAllEntities p triggers = do
    mJson <- getAndDecode (triggers $> "api/" <> resourceName p)
    return $ (Map.fromList . toList) <$> mJson

  postEntity :: MonadWidget t m => Event t v ->  m (Event t Int64)

  putEntity :: MonadWidget t m => Event t (Int64,v) -> m (Event t ())

  deleteEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t ())



-----------------------------------------------------------------------------
crudTableWidget :: forall t m v.(MonadWidget t m, Crud v)
  => Proxy v
  -> Dynamic t (v -> Bool)
  -> m (Event t ())
crudTableWidget p dynValidate = mdo
  updateEvents <- crudEvents p tableEvents
  vMap <- holdDyn mempty =<< getAllEntities p (() <$ updateEvents)
  tableEvents <- elClass "table" "crud-table" $ do
    --newRowEvents      <- newRowWidget p
    existingRowsEvents <- listViewWithKey vMap crudRowWidget
    --return (newRowEvents <> existingRowEvents)
    return existingRowsEvents
  return tableEvents


-----------------------------------------------------------------------------
crudRowWidget :: (MonadWidget t m, Crud v)
              => Int64
              -> Dynamic t v
              -> m (Event t (CrudRowCmds v))
crudRowWidget k dynVal = do
  text "RowWidget" >> return (never)


instance Crud StimulusResource where
  resourceName _ = "StimulusResource"
  inputWidget  _ = do
    text "F1:"
    f1 <- _textInput_value <$> textInput def
    text "F2:"
    f2 <- _textInput_value <$> textInput def
    text "F3:"
    f3 <- _textInput_value <$> textInput def
    $( qDyn [| StimulusResource
                        (T.pack $(unqDyn [|f1|]))
                        (T.pack $(unqDyn [|f2|]))
                        (T.pack $(unqDyn [|f3|])) |] )


instance Crud TaggingUser where
  resourceName _ = "TaggingUser"
  inputWidget  _ = undefined
