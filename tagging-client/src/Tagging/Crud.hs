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
import qualified Data.UUID as U
import           Database.Groundhog.Core (PersistValue(PersistInt64))
import GHC.Int
import Reflex.Dom
import Reflex.Dom.Xhr
import Reflex.Dynamic.TH

import Tagging.User
import Tagging.Stimulus
import Tagging.Response
import Utils

class (A.FromJSON v, A.ToJSON v) => Crud v where

  resourceName :: Proxy v -> String

  resourceWidget  :: MonadWidget t m  
                  => (Dynamic t v) 
                  -> (Dynamic t Bool) 
                  -> m (Dynamic t (Either String v))

  resourceHeaders :: Proxy v -> [String]

  inputWidget :: MonadWidget t m => v -> m (Dynamic t v)
  outputWidget :: MonadWidget t m => Dynamic t v ->  m ()

  getEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t (Maybe v))
  getEntity p ks = getAndDecode (fmap toApi ks)
      where toApi k = ("api/" <> resourceName p <> "/" <> show k)

  getAllEntities :: MonadWidget t m => Proxy v -> Event t () -> m (Event t (Map Int64 v))
  getAllEntities p triggers = do
    mJson <- getAndDecode (triggers $> "api/" <> resourceName p)
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


instance Crud TaggingUser where
  resourceName _ = "tagginguser"
  resourceHeaders _ = ["Tagging Id","Student IdTest","Name","Current Stimulus","Roles"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    -- f1 <- crudPieceField pbV (show . tuId) attrs
    f1 <- validatingCrudPieceField pbV (show . tuId) attrs (readEither "No id parse")
    f2 <- crudPieceField pbV (maybe "" T.unpack . tuStudentID) attrs
    f3 <- crudPieceField pbV (maybe "" T.unpack . tuRealName) attrs
    -- f4 <- crudPieceField pbV (printPosString . tuCurrentStimulus) attrs
    f4 <- validatingCrudPieceField pbV (printPosString . tuCurrentStimulus) attrs parsePosString
    f5 <- crudPieceField pbV (show . tuRoles) attrs
    $(qDyn [| TaggingUser
              -- <$> readEither "No Id parse" $(unqDyn [|f1|])
              <$> $(unqDyn [|f1|])
              <*> pure (let f2' = $(unqDyn [|f2|])
                        in  if null f2' then Nothing else Just (T.pack f2'))
              <*> pure (let f3' = $(unqDyn [|f3|])
                        in  if null f3' then Nothing else Just (T.pack f3'))
              -- <*> parsePosString $(unqDyn [|f4|])
              <*> $(unqDyn [|f4|])
              <*> readEither "No Roles parse" $(unqDyn [|f5|])
            |])


parsePosString :: String -> Either String (Maybe PositionInfo)
parsePosString "" = Right Nothing
parsePosString s  = let (x,y) = break (== ':') s
                        mPI   = PositionInfo 
                                <$> fmap intToKey (readMay x) 
                                <*> (readMay =<< safeTail y)
                    in case mPI of
                      Nothing -> Left "No Index parse (either \"\" or \"m:n\""
                      Just p  -> Right (Just p)

printPosString :: Maybe PositionInfo -> String
printPosString Nothing = ""
printPosString (Just (PositionInfo (StimulusSequenceKey (PersistInt64 x)) y)) =
  show x ++ ":" ++ show y

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

readEither :: Read a => String -> String -> Either String a
readEither n a = note n (readMay a)

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
              <*> note "No UUID parse" (U.fromText (T.pack $(unqDyn [|f2|])))
              <*> note "No metadata parse (should be valid JSON)"
                  ((A.decode . BL.pack) $(unqDyn [|f3|]))
              <*> pure (T.pack $(unqDyn [|f4|]))
              <*> pure (T.pack $(unqDyn [|f5|]))
              <*> readEither "No sampling parse" $(unqDyn[|f6|])
            |])

instance Crud StimSeqItem where
  resourceName _ = "stimseqitem"
  resourceHeaders _ = ["Parent Sequence","Stimulus Resource","Next Item", "List Index", "Response Type"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (show . ssiStimulus) attrs
    f2 <- crudPieceField pbV (show . ssiStimulusSequence) attrs
    f3 <- crudPieceField pbV (show . ssiIndex) attrs
    $(qDyn [| StimSeqItem
              <$> readEither "No stimulus parse (should be JSON)" $(unqDyn [|f1|])
              <*> readEither "No parend id parse" $(unqDyn [|f2|])
              <*> readEither "No sequence index parse" $(unqDyn [|f3|])
            |])


instance Crud StimulusRequest where
  resourceName _ = "stimulusrequest"
  resourceHeaders _ = ["User","StimSeqItem","Sequence","Index","Time"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (show . sreqUser) attrs
    f2 <- crudPieceField pbV (printPosString . Just . sreqStimSeqItem) attrs
    f3 <- crudPieceField pbV (show . sreqSequence) attrs
    f4 <- crudPieceField pbV (show . sreqIndex) attrs
    f5 <- crudPieceField pbV (show . sreqTime) attrs
    $(qDyn [| StimulusRequest
              <$> readEither "No user id parse" $(unqDyn [|f1|])
              <*> (join . fmap (note "No parse"))
                    (parsePosString $(unqDyn [|f2|]))
              <*> readEither "No sequence key parse" $(unqDyn [|f3|])
              <*> readEither "No index parse" $(unqDyn [|f4|])
              <*> readEither "No parse for timestamp" $(unqDyn [|f5|])
            |])

instance Crud StimulusResponse where
  resourceName _ = "stimulusresponse"
  resourceHeaders _ = ["User","StimSeqItem","Delivery"
                      ,"Recepit","Type","Payload"]
  resourceWidget dynVal dynB = do
    pb <- getPostBuild
    let pbV = tag (current dynVal) pb
    attrs <- forDyn dynB $ \b ->
      if b then mempty else (s "disabled" =: s "disabled")
    f1 <- crudPieceField pbV (show . srUser) attrs
    f2 <- crudPieceField pbV (show . srStim) attrs
    f3 <- crudPieceField pbV (show . srSequence) attrs
    f4 <- crudPieceField pbV (show . srIndex) attrs
    f5 <- crudPieceField pbV (show . srDeliveredTime) attrs
    f6 <- crudPieceField pbV (show . srRespondedTime) attrs
    f7 <- crudPieceField pbV (show . srResponseType) attrs
    f8 <- crudPieceField pbV (show . srResponseData) attrs
    $(qDyn [| StimulusResponse
              <$> readEither "No user id parse" $(unqDyn [|f1|])
              <*> note "No stimulus parse" ((A.decode . BL.pack) $(unqDyn [|f2|]))
              <*> readEither "No sequence parse" $(unqDyn [|f3|])
              <*> readEither "No index parse" $(unqDyn [|f4|])
              <*> readEither "No delivery timestamp parse" $(unqDyn [|f5|])
              <*> readEither "No receipt timestamp parse" $(unqDyn [|f6|])
              <*> pure (T.pack  $(unqDyn [|f7|]))
              <*> note "No response data parse (should be JSON)"
                  ((A.decode . BL.pack)  $(unqDyn [|f8|]))
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
                         => Event t v
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
