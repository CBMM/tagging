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
import           Control.Monad (join)
import           Data.Char (isNumber)
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
import qualified Data.UUID.Types as U
import Database.Groundhog
import           Database.Groundhog.Core -- (Key, PersistValue(PersistInt64))
import GHC.Int
import Reflex.Dom
import Reflex.Dom.Xhr
import Reflex.Dynamic.TH

import Tagging.User
import Tagging.Stimulus
import Tagging.Response
import Utils
import qualified Utils


------------------------------------------------------------------------------
class (A.FromJSON v, A.ToJSON v) => Crud v where

  resourceName :: Proxy v -> String

  resourceWidget  :: MonadWidget t m
                  => (Dynamic t v) -- ^ External updates to value
                  -> (Dynamic t Bool) -- ^ Editing toggle
                  -> m (Dynamic t (Either String v))

  resourceHeaders :: Proxy v -> [String]

  -- inputWidget :: MonadWidget t m => v -> m (Dynamic t v)
  -- outputWidget :: MonadWidget t m => Dynamic t v ->  m ()


getEntity :: (MonadWidget t m, Crud v, A.FromJSON v) => Proxy v -> Event t Int64 -> m (Event t (Maybe v))
getEntity p ks = getAndDecode (fmap toApi ks)
      where toApi k = ("api/" <> resourceName p <> "/" <> show k)

getAllEntities :: (MonadWidget t m, Crud v, A.FromJSON v) => Proxy v -> Event t () -> m (Event t (Map Int64 v))
getAllEntities p triggers = do
    mJson <- getAndDecode (triggers $> "api/" <> resourceName p)
    return $ fmap (Map.mapKeys fromIntegral . Map.fromList . I.toList) $ fforMaybe mJson id

-- TODO
-- postEntity :: MonadWidget t m => Event t v ->  m (Event t Int64)

putEntity :: (MonadWidget t m, Crud v, A.ToJSON v) => Proxy v -> Event t (Int64,v) -> m (Event t ())
putEntity p kvEvent = do
    let req = ffor kvEvent (\(k,v) ->
                XhrRequest "PUT"
                ("api/" <> resourceName p <> "/" <> show k <> "/")
                (def { _xhrRequestConfig_headers  = s "Content-Type" =: s "application/json"
                     , _xhrRequestConfig_sendData = Just (BL.unpack $ A.encode v)}))
    fmap (() <$) $ performRequestAsync req

deleteEntity :: (MonadWidget t m, Crud v) => Proxy v -> Event t Int64 -> m (Event t ())
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
    _ <- el "tr" $ (el "th" (return ()) >>
                    mapM (el "th" . text) (resourceHeaders p))
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
    el "td" $ text (show k)

    dynEditing   <- holdDyn False $ leftmost [False <$ saveClicks,  True <$ editButton]

    dynM <- resourceWidget dynVal dynEditing

    saveAttrs <- combineDyn (\b mV ->
      s "style" =: s ("display:" <> bool "none" "normal" b)
      <> (bool (s "disabled" =: s "disabled") mempty (isRight mV)))
      dynEditing dynM

    editAttrs <- forDyn dynEditing $ \b ->
      s "style" =: s ("display:" <> bool "normal" "none" b)

    saveButton <- fmap fst $ elDynAttr' "button" saveAttrs $ text "Save"

    _ <- putEntity p (fmap (k,) (fmapMaybe hush (tag (current dynM) saveClicks)))
    _ <- deleteEntity p (k <$ delButton)

    let saveClicks = domEvent Click saveButton
    let vAtClick = fmapMaybe hush $ tagDyn dynM saveClicks
        fAtClick = fmap (\v m -> Map.insert k v m) vAtClick

    editButton <- fmap (domEvent Click . fst) $ elDynAttr' "button" editAttrs $ text "Edit"
    delButton  <- el "td" $ button "Delete"
    return $ leftmost [(Map.delete k) <$ delButton
                      ,fAtClick
                      ]


tuKey :: Key TaggingUser BackendSpecific -> Int64
tuKey  (TaggingUserKey      (PersistInt64 k)) = k

ssKey :: Key StimulusSequence BackendSpecific -> Int64
ssKey  (StimulusSequenceKey (PersistInt64 k)) = k

ssiKey :: Key StimSeqItem BackendSpecific -> Int64
ssiKey (StimSeqItemKey      (PersistInt64 k)) = k


------------------------------------------------------------------------------
instance Crud TaggingUser where
  resourceName _ = "tagginguser"
  resourceHeaders _ = ["Tagging Id","Student Id","Name","Roles"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (show . tuId)                     attrs
    f2 <- crudPieceField pbV (maybe "" T.unpack . tuStudentID) attrs
    f3 <- crudPieceField pbV (maybe "" T.unpack . tuRealName)  attrs
    f4 <- crudPieceField pbV (show . tuRoles)                  attrs
    $(qDyn [| TaggingUser
              <$> readEither "No id parse" $(unqDyn [|f1|])
              <*> pure (let f2' = $(unqDyn [|f2|])
                        in  if null f2' then Nothing else Just (T.pack f2'))
              <*> pure (let f3' = $(unqDyn [|f3|])
                        in  if null f3' then Nothing else Just (T.pack f3'))
              <*> readEither "No Roles parse" $(unqDyn [|f4|])
            |])


------------------------------------------------------------------------------
instance Crud Assignment where
  resourceName _ = "assignment"
  resourceHeaders _ = ["Tagging Id","Sequence","Index","Start Index"
                      ,"End Index","FinishURL"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV
          (show . tuKey . aUser)
          attrs
    f2 <- crudPieceField pbV
          (show . ssKey . aSequence)
          attrs
    f3 <- crudPieceField pbV (maybe "" show . aIndex) attrs
    f4 <- crudPieceField pbV (show . aStart) attrs
    f5 <- crudPieceField pbV (show . aEnd) attrs
    f6 <- crudPieceField pbV (maybe "" T.unpack . aFinished) attrs
    $(qDyn [| Assignment
              <$> fmap Utils.intToKey (readEither "No user id parse"
                                       $(unqDyn [|f1|]))
              <*> fmap Utils.intToKey (readEither "No sequence id parse"
                                       $(unqDyn [|f2|]))
              <*> (let s = $(unqDyn [|f3|])
                  in bool (Just <$> readEither "No index parse" s) (Right Nothing) (null s)) --    $(unqDyn [|f3|])
              <*> readEither "No start index parse" $(unqDyn [|f4|])
              <*> readEither "No end index parse"   $(unqDyn [|f5|])
              <*> (let s = $(unqDyn [|f6|])
                  in Right $ Just $ bool (T.pack s) "" (null s))
                     -- readEither "No url parse"         $(unqDyn [|f6|]) -- TODO actual url parse check
            |])



safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs


readEither :: Read a => String -> String -> Either String a
readEither n a = note n (readMay a)

------------------------------------------------------------------------------
instance Crud StimulusSequence where
  resourceName _ = "stimulussequence"
  resourceHeaders _ = ["Name","UUID","Metadata","Description"
                      , "Base Url","Sampling Method"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (T.unpack . ssName) attrs
    f2 <- crudPieceField pbV (show     . ssUuid) attrs
    f3 <- crudPieceField pbV (BL.unpack . A.encode . ssMetaData) attrs
    f4 <- crudPieceField pbV (T.unpack . ssDescription) attrs
    f5 <- crudPieceField pbV (T.unpack . ssBaseUrl) attrs
    f6 <- crudPieceField pbV (show      . ssSampling) attrs
    $(qDyn [| StimulusSequence
              <$> pure (T.pack $(unqDyn [|f1|]))
              <*> note "No UUID parse"
                  (U.fromText (T.pack $(unqDyn [|f2|])))
              <*> note "No metadata parse (should be valid JSON)"
                  ((A.decode . BL.pack) $(unqDyn [|f3|]))
              <*> pure (T.pack $(unqDyn [|f4|]))
              <*> pure (T.pack $(unqDyn [|f5|]))
              <*> readEither "No sampling parse" $(unqDyn[|f6|])
            |])

------------------------------------------------------------------------------
instance Crud StimSeqItem where
  resourceName _ = "stimseqitem"
  resourceHeaders _ = ["Parent Sequence","Stimulus Resource","Next Item"
                      ,"List Index", "Response Type"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (show . ssiStimulus) attrs
    f2 <- crudPieceField pbV (show . ssKey . ssiStimulusSequence) attrs
    f3 <- crudPieceField pbV (show . ssiIndex) attrs
    $(qDyn [| StimSeqItem
              <$> readEither
                  "No stimulus parse (should be JSON)" $(unqDyn [|f1|])
              <*> fmap Utils.intToKey (readEither
                  "No parend id parse" $(unqDyn [|f2|]))
              <*> readEither
                  "No sequence index parse" $(unqDyn [|f3|])
            |])

instance Crud StimulusRequest where
  resourceName _ = "stimulusrequest"
  resourceHeaders _ = ["User","Sequence","Index","Time"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (show . sreqUser) attrs
    f2 <- crudPieceField pbV (show . ssKey . sreqSequence) attrs
    f3 <- crudPieceField pbV (show . sreqIndex) attrs
    f4 <- crudPieceField pbV (show . sreqTime) attrs
    $(qDyn [| StimulusRequest
              <$> readEither "No user id parse" $(unqDyn [|f1|])
              <*> fmap Utils.intToKey (readEither "No sequence key parse" $(unqDyn [|f2|]))
              <*> readEither "No index parse" $(unqDyn [|f3|])
              <*> readEither "No parse for timestamp" $(unqDyn [|f4|])
            |])


instance Crud StimulusResponse where
  resourceName _ = "stimulusresponse"
  resourceHeaders _ = ["User","Sequence","Index","Delivery"
                      ,"Recepit","Type","Payload"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (show . srUser) attrs
    f2 <- crudPieceField pbV (show . ssKey . srSequence) attrs
    f3 <- crudPieceField pbV (show . srIndex) attrs
    f4 <- crudPieceField pbV (show . srDeliveredTime) attrs
    f5 <- crudPieceField pbV (show . srRespondedTime) attrs
    f6 <- crudPieceField pbV (show . srResponseType) attrs
    f7 <- crudPieceField pbV (show . srResponseData) attrs
    $(qDyn [| StimulusResponse
              <$> readEither "No user id parse" $(unqDyn [|f1|])
              <*> fmap Utils.intToKey (readEither "No sequence parse" $(unqDyn [|f2|]))
              <*> readEither "No index parse" $(unqDyn [|f3|])
              <*> readEither "No delivery timestamp parse" $(unqDyn [|f4|])
              <*> readEither "No receipt timestamp parse" $(unqDyn [|f5|])
              <*> pure (T.pack  $(unqDyn [|f6|]))
              <*> note "No response data parse (should be JSON)"
                  ((A.decode . BL.pack)  $(unqDyn [|f7|]))
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

validatingCrudPieceField :: MonadWidget t m
                         => Event t v -- ^ External set string
                         -> (v -> String)
                         -> Dynamic t (Map.Map String String)
                         -> (String -> Either String a)
                         -> m (Dynamic t (Either String a))
validatingCrudPieceField pbV proj attrs validate = el "td" $ mdo
  tInput <- textInput (TextInputConfig
                        { _textInputConfig_setValue =
                            fmap (\v -> proj $ v) pbV
                        , _textInputConfig_attributes   = fullAttrs
                        , _textInputConfig_inputType    = "text"
                        , _textInputConfig_initialValue = "empty"})
  parseRes <- mapDyn validate  (_textInput_value tInput)
  extraAttrs <- forDyn parseRes $ \case
    Left s  -> "background" =: "red"
    Right _ -> mempty
  fullAttrs <- combineDyn (<>) attrs extraAttrs
  return parseRes


s :: String -> String
s = id
