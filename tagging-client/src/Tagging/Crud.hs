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

  resourceWidget :: MonadWidget t m  => v -> Bool -> m (Dynamic t v)
  inputWidget :: MonadWidget t m => v -> m (Dynamic t v)
  outputWidget :: MonadWidget t m => Dynamic t v ->  m ()

  getEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t (Maybe v))
  getEntity p ks = getAndDecode (fmap toApi ks)
      where toApi k = ("api/" <> resourceName p <> "/" <> show k)

  getAllEntities :: MonadWidget t m => Proxy v -> Event t () -> m (Event t (Map Int64 v))
  getAllEntities p triggers = do
    mJson <- getAndDecode (triggers $> "api/" <> map toLower (resourceName p))
    return $ fmap (Map.mapKeys fromIntegral . Map.fromList . I.toList) $ fforMaybe mJson id

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
  let xhrGetEvents = leftmost [() <$ pl] -- , () <$ tableEvents]
  --let updateEvents = leftmost ((id <$ pl): fmap Map.elems tableEvents)
  vMapServer <- (fmap . fmap) const $ getAllEntities p xhrGetEvents
  --let updateEvents = () <$ tableEvents
  vMap <- foldDyn ($) mempty (tableEvents <> vMapServer)
  tableEvents <- elClass "table" "crud-table" $ do
    eventsMap <- listViewWithKey vMap crudRowWidget
    -- Turn the row events (Event t (Map k MapEndo)) into (Event t MapEndo)
    return $ fmap (flip (foldr ($)) .  Map.elems) eventsMap
    --dynText =<< mapDyn (show . length) vMap
    --forDyn vMap (\vs -> text (show $ length vs))
    --return never
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


---------------------------------------------------------------------------
crudRowWidget :: forall t m v.(MonadWidget t m, Crud v)
              => Int64
              -> Dynamic t v
              -> m (Event t (Map.Map Int64 v -> Map.Map Int64 v))
crudRowWidget k dynVal = do
  el "tr" $ mdo
    text (show k)
    -- dynEditing <- holdDyn False (leftmost [ True  <$ editButton
    --                                       , False <$ saveClicks
    --                                       ])
    editSaveToggle <- widgetHold showWidget [ editWidget <$ editButton
                                            , showWidget <$ saveButton
                                            ]
    dynDynM <- combineDyn resourceWidget dynVal dynEditing :: MonadWidget t m => m (Dynamic t (Dynamic t (m v)))
    let dynM = joinDyn dynDynM
    --dynM <- outputWidget dynVal
    --evts <- dyn dynM

    --saveAttrs <- forDyn dynEditing $ \b -> s "style" =: s ("display:" <> bool "none" "normal" b)
    --editAttrs <- forDyn dynEditing $ \b -> s "style" =: s ("display:" <> bool "normal" "none" b)

    --saveClicks <- elDynAttr "div" saveAttrs $ button "S"
    saveButton <- button "Save"
    let saveClicks = saveButton
    --saveButton <- fmap fst $ elDynAttr' "button" saveAttrs $ text "Save"
    --let saveClicks = (domEvent Click saveButton)
    let vAtClick = tagDyn dynM saveClicks
        fAtClick = fmap (\v m -> Map.insert k v m) vAtClick
    --saveButton <- dyn =<< forDyn dynEditing (bool (return never) (button "S"))
    -- saveButton <- dyn =<< forDyn dynEditing $ \case
    --   False -> return never
    --   True  -> button "S"
    editButton <- fmap (domEvent Click . fst) $ elDynAttr' "button" editAttrs $ text "Edit"
    delButton  <- el "td" $ button "Delete"
    return $ leftmost [(Map.delete k) <$ delButton
                      --,fAtClick
                      ]


crudRowWidget' :: (MonadWidget t m, Crud v)
               => (Int64, v)
               -> m (Event t (Map.Map Int64 v -> Map.Map Int64 v))
crudRowWidget' (k, v) = do
  el "tr" $ do
    text (show k)
    dynVal <- resourceWidget v False
    return $ (Map.delete k) <$ updated dynVal

-----------------------------------------------------------------------------
instance Crud StimulusResource where
  resourceName _ = "StimulusResource"
  resourceWidget v0 b = do
    let attrs = if b then mempty else (s "disabled" =: s "true")
    f1 <- el "td" $
          _textInput_value <$> textInput
           def { _textInputConfig_initialValue = T.unpack (srName v0)
               , _textInputConfig_attributes   = constDyn attrs}
    f2 <- el "td" $
          _textInput_value <$> textInput
           def {_textInputConfig_initialValue = T.unpack (srUrlSuffix v0)
               , _textInputConfig_attributes  = constDyn attrs}
    f3 <- el "td" $
          _textInput_value <$> textInput
           def { _textInputConfig_initialValue = T.unpack (srMimeType v0)
               , _textInputConfig_attributes   = constDyn attrs}
    $( qDyn [| StimulusResource
                        (T.pack $(unqDyn [|f1|]))
                        (T.pack $(unqDyn [|f2|]))
                        (T.pack $(unqDyn [|f3|])) |] )
  inputWidget = flip resourceWidget False
  outputWidget dynVal = do
    el "td" =<< forDyn dynVal (T.unpack . srName)
    el "td" =<< forDyn dynVal (T.unpack . srUrlSuffix)
    el "td" =<< forDyn dynVal (T.unpack . srMimeType)


instance Crud TaggingUser where
  resourceName _ = "TaggingUser"
  resourceWidget  _ = undefined

s :: String -> String
s = id
