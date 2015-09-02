{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE StandaloneDeriving   #-}


module Tagging.Crud where

import           Control.Error
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

  resourceWidget :: MonadWidget t m  => (Dynamic t v) -> (Dynamic t Bool) -> m (Dynamic t (Maybe v))
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

  putEntity :: MonadWidget t m => Proxy v -> Event t (Int64,v) -> m (Event t ())
  putEntity p kvEvent = do
    let req = ffor kvEvent (\(k,v) ->
                XhrRequest "PUT"
                ("api/" <> resourceName p <> "/" <> show k <> "/")
                (def { _xhrRequestConfig_headers  = s "Content-Type" =: s "application/json"
                     , _xhrRequestConfig_sendData = Just (BL.unpack $ A.encode v)}))
    fmap (() <$) $ performRequestAsync req

  deleteEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t ())
  deleteEntity p delEvent =
    let req = ffor delEvent $ \k ->
                XhrRequest "DELETE"
                ("api/" <> resourceName p <> "/" <> show k) def
    in  fmap (() <$) $ performRequestAsync req



-----------------------------------------------------------------------------
crudTableWidget :: forall t m v.(MonadWidget t m, Crud v, Show v)
  => Proxy v
  -> Dynamic t (v -> Bool)
  -> m (Event t ())
crudTableWidget p dynValidate = mdo
  pl <- getPostBuild
  let xhrGetEvents = leftmost [() <$ pl] -- , () <$ tableEvents]
  vMapServer <- (fmap . fmap) const $ getAllEntities p xhrGetEvents
  vMap <- foldDyn ($) mempty (tableEvents <> vMapServer)
  tableEvents <- elClass "table" "crud-table" $ do
    eventsMap <- listViewWithKey vMap (crudRowWidget p)
    return $ fmap (flip (foldr ($)) . Map.elems) eventsMap
  return $ () <$ tableEvents


---------------------------------------------------------------------------
crudRowWidget :: forall t m v.(MonadWidget t m, Crud v)
              => Proxy v
              -> Int64
              -> Dynamic t v
              -> m (Event t (Map.Map Int64 v -> Map.Map Int64 v))
crudRowWidget p k dynVal = do
  el "tr" $ mdo
    text (show k)

    dynEditing   <- holdDyn False $ leftmost [False <$ saveClicks,  True <$ editButton]

    dynM <- resourceWidget dynVal dynEditing

    saveAttrs <- forDyn dynEditing $ \b ->
      s "style" =: s ("display:" <> bool "none" "normal" b)
    editAttrs <- forDyn dynEditing $ \b ->
      s "style" =: s ("display:" <> bool "normal" "none" b)

    saveButton <- fmap fst $ elDynAttr' "button" saveAttrs $ text "Save"

    _ <- putEntity p (fmap (k,) (fmapMaybe id (tag (current dynM) saveClicks)))
    _ <- deleteEntity p (k <$ delButton)

    let saveClicks = domEvent Click saveButton
    let vAtClick = fmapMaybe id $ tagDyn dynM saveClicks
        fAtClick = fmap (\v m -> Map.insert k v m) vAtClick

    editButton <- fmap (domEvent Click . fst) $ elDynAttr' "button" editAttrs $ text "Edit"
    delButton  <- el "td" $ button "Delete"
    return $ leftmost [(Map.delete k) <$ delButton
                      ,fAtClick
                      ]


-----------------------------------------------------------------------------
instance Crud StimulusResource where
  resourceName _ = "stimulusresource"
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = (tag (current dynVal) pb)
    attrs <- forDyn dynB $ \b ->
                if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (T.unpack . srName) attrs
    f2 <- crudPieceField pbV (T.unpack . srUrlSuffix) attrs
    f3 <- crudPieceField pbV (T.unpack . srMimeType) attrs
    $( qDyn [| Just $ StimulusResource
                        (T.pack $(unqDyn [|f1|]))
                        (T.pack $(unqDyn [|f2|]))
                        (T.pack $(unqDyn [|f3|])) |] )

instance Crud TaggingUser where
  resourceName _ = "tagginguser"
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (show . tuId) attrs
    f2 <- crudPieceField pbV (maybe "" T.unpack . tuStudentID) attrs
    f3 <- crudPieceField pbV (maybe "" T.unpack . tuRealName) attrs
    f4 <- crudPieceField pbV (maybe "" show . tuCurrentStimulus) attrs
    f5 <- crudPieceField pbV (show . tuRoles) attrs
    $(qDyn [| TaggingUser
              <$> readMay $(unqDyn [|f1|])
              <*> pure (let f2' = $(unqDyn [|f2|])
                        in  if null f2' then Nothing else Just (T.pack f2'))
              <*> pure (let f3' = $(unqDyn [|f3|])
                        in  if null f3' then Nothing else Just (T.pack f3'))
              <*> pure (readMay $(unqDyn [|f4|]))
              <*> readMay $(unqDyn [|f5|])
            |])

crudPieceField :: MonadWidget t m
               => Event t v
               -> (v -> String)
               -> Dynamic t (Map.Map String String)
               -> m (Dynamic t String)
crudPieceField pbV proj attrs = el "td" $ do
  _textInput_value <$> textInput
    (TextInputConfig { _textInputConfig_setValue =
                                 fmap (\v -> proj $ v) pbV
                     , _textInputConfig_attributes   = attrs
                     , _textInputConfig_inputType    = "text"
                     , _textInputConfig_initialValue = "empty"})


instance Eq StimulusResource where
  StimulusResource a b c == StimulusResource d e f =
    a == d && b == e && c ==f

deriving instance Show TaggingUser

s :: String -> String
s = id
