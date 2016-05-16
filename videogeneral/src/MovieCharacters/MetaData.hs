{-#  LANGUAGE DeriveGeneric       #-}
{-#  LANGUAGE LambdaCase          #-}
{-#  LANGUAGE RecursiveDo         #-}
{-#  LANGUAGE RankNTypes          #-}
{-#  LANGUAGE TupleSections       #-}
{-#  LANGUAGE ScopedTypeVariables #-}

module MovieCharacters.MetaData where

import           Control.Applicative (liftA2)
-- import           Control.Arrow (first, second)
import           Data.Bool (bool)
import           Data.Default (Default)
import qualified Data.Map as Map
-- import           Data.Maybe (fromMaybe)
-- import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
-- import           Data.Traversable
import           GHC.Generics (Generic)
import           Text.Read (readMaybe)
import           Reflex.Dom
-- import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dom.Contrib.Widgets.ButtonGroup (radioGroup)


data MetaData = MetaData
  { version                   :: Int
  , characters                :: [Character]
  , clipGeneralQuestions      :: [QuestionSpec]
  , characterClipAttributes   :: [QuestionSpec]
  , characterStableAttributes :: [QuestionSpec]
  } deriving (Eq, Ord, Show, Generic)

data Character = Character
  { characterId          :: Text
  , characterDisplayName :: Text
  , characterPicUrl      :: Text
  } deriving (Eq, Ord, Show, Generic)

data QuestionSpec = QuestionSpec
  { questionOptional :: Bool
  , questionName     :: Text
  , questionLabel    :: Text
  , question         :: Question
  } deriving (Eq, Ord, Show, Generic)

data Question = QButtonsChoice  [Choice]
              | QRadioChoice    [Choice]
              | QDropdownChoice [Choice]
              | QTextEntry
              deriving (Eq, Ord, Show, Generic)

data Choice = Choice
  { choiceId :: Text
  , choiceLabel :: Text
  } deriving (Eq, Ord, Show, Generic)


data MetaDataWidgetConfig t = MetaDataWidgetConfig
  { metaDataWidgetConfig_initialValue :: MetaData
  , metaDataWidgetConfig_setValue     :: Event t MetaData
  }

instance Default MetaData where
  def = MetaData 0 [] [] [] []

instance Reflex t => Default (MetaDataWidgetConfig t) where
  def = MetaDataWidgetConfig (MetaData 0 [] [] [] []) never


------------------------------------------------------------------------------
questionMetaData :: MonadWidget t m
                 => m (Dynamic t (Either String QuestionSpec))
questionMetaData = divClass "question-metadata" $ do

  rec isOptional <- divClass "optional" $ do
        elAttr "label" ("for" =: "question-metadata") (text "Optional")
        mapDyn Right =<< value <$> checkbox False def

      qId <- mapDyn valdId =<< (divClass "id" $ do
        elAttr "label" ("for" =: "id") (text "ID")
        value <$> textInput def { _textInputConfig_attributes =
                                  constDyn ("placeholder" =: "question-id")})

      qLabel <- mapDyn valdLabel =<< (divClass "qLabel" $ do
        elAttr "label" ("for" =: "label") (text "Question Label")
        value <$> textInput def)

      radiosId <- mapDyn (either (const "radiogroup-")
                          (\x -> "radiogroup-" ++ x) . fmap T.unpack) qId
      let radioBtns = [(0 :: Int, "Buttons"),(1, "Radio Buttons")
                      ,(2,"Dropdown"),(3,"Textbox")]
      qType <- mapDyn (note "Invalid radio choice") =<<
        value <$> radioGroup radiosId (constDyn radioBtns) def

      choices <- validatingList "Choices" oneChoiceWidget

      qChoices <- combineDyn (liftA2 choicesForQ) qType choices

  liftA4 QuestionSpec `mapDyn` isOptional `apDyn` qId
                      `apDyn`  qLabel     `apDyn` qChoices

  where valdId x = if (null x) || (' ' `elem` x)
                   then Left $ "Question ID must not not be "
                                ++ "empty or contain spaces"
                   else Right (T.pack x)
        valdLabel x = if null x
                      then Left "Question label must not be empty"
                      else Right (T.pack x)
        oneChoiceWidget = validatingChoiceWidget
        choicesForQ typeInd cs = case typeInd of
          0 -> QButtonsChoice cs
          1 -> QRadioChoice cs
          2 -> QDropdownChoice cs
          3 -> QTextEntry




-------------------------------------------------------------------------------
metaDataWidget :: MonadWidget t m
               => MetaDataWidgetConfig t
               -> m (Dynamic t (Either String MetaData))
metaDataWidget cfg = do
  v :: Dynamic t (Either String Int)          <- versionWidget
  chars        <- charactersWidget
  clipQs       <- clipGeneralQWidget
  charClipQs   <- characterClipQWidget
  charStableQs <- characterStableQWidget
  liftA5 MetaData `mapDyn` v `apDyn` chars `apDyn` clipQs
                  `apDyn` charClipQs `apDyn` charStableQs

  where versionWidget = validatingSingleField "Version" "1" (note "Need an integer" . readMaybe)
        clipGeneralQWidget     = validatingList "clip-questions" questionMetaData
        characterClipQWidget   = validatingList "clip-character-questions"questionMetaData
        characterStableQWidget = validatingList "character-stabale-questions" questionMetaData


validatingSingleField :: MonadWidget t m
                      => String -- ^ Label
                      -> String -- ^ Placeholder
                      -> (String -> Either String a) -- ^ Validation
                      -> m (Dynamic t (Either String a))
validatingSingleField label placeholder validate = do
  rec res <- elDynAttr "div" singleFieldAttrs $ do
        elAttr "label" ("for" =: label) $ text label
        txt <- divClass "input-group" $ do
          txt <- fmap value $ textInput def
          dynText =<< forDyn res (either id (const ""))
          return txt
        mapDyn validate txt
      singleFieldAttrs <- forDyn res $
        either (const $ "class" =: "validatnig-single-field invalid")
               (const $ "class" =: "validating-single-field valid")
  return res

validatingChoiceWidget :: MonadWidget t m
                       => m (Dynamic t (Either String Choice))
validatingChoiceWidget = do
  cId <- mapDyn (\s -> bool (Right $ T.pack s) (Left "Invalid id") (null s || elem ' ' s)) =<<
    value <$> textInput def { _textInputConfig_attributes = constDyn ("placeholder" =: "choice-id")}
  cLb <- mapDyn (\s -> bool (Right $ T.pack s) (Left "Invalid label") (null s)) =<<
    value  <$> textInput def { _textInputConfig_attributes = constDyn ("placeholder" =: "Choice Label")}
  liftA2 Choice `mapDyn` cId `apDyn` cLb

charactersWidget  :: MonadWidget t m => m (Dynamic t (Either String [Character]))
charactersWidget = validatingList "Characters" oneCharacterWidget

oneCharacterWidget :: MonadWidget t m => m (Dynamic t (Either String Character))
oneCharacterWidget = do
  i <- mapDyn vald =<< (value <$>  textInput def { _textInputConfig_attributes =
                       constDyn ("placeholder" =: "person_a")})
  n <- mapDyn vald =<< (value <$> textInput def { _textInputConfig_attributes =
                       constDyn ("placeholder" =: "Albert")})
  u <- mapDyn vald =<< (value <$> textInput def { _textInputConfig_attributes =
                  constDyn ("placeholder" =: "http://example.com/face_a.jpg")})
  liftA3 Character `mapDyn` i `apDyn` n `apDyn` u
  where vald :: String -> Either String Text
        vald x       = if null x then Left "Empty" else Right (T.pack x)


validatingList :: forall t m r.MonadWidget t m
               => String -- ^ Label
               -> m (Dynamic t (Either String r))
               -> m (Dynamic t (Either String [r]))
validatingList label oneItem = do

  adds <- button "Add"
  let oneListEntry :: Int -> () -> Event t () -> m (Dynamic t (Either String r), Event t (Map.Map Int (Maybe ())))
      oneListEntry k v e = divClass "validating-list-item" $ do
        r <- oneItem
        x <- button "(x)"
        let closes = (k =: Nothing) <$ x
        return (r, closes)

  let m0 = mempty :: Map.Map Int ()
  rec xsAndDels <- listWithKeyShallowDiff m0 (leftmost [addEvents, delEvents]) oneListEntry
      let x = xsAndDels :: Dynamic t (Map.Map Int (Dynamic t (Either String r), Event t (Map.Map Int (Maybe ()))))
          addEvents :: Event t (Map.Map Int (Maybe ())) = ffor (tag (current xsAndDels) adds) $ \m ->
            maybe 0 (succ . fst . fst) (Map.maxViewWithKey m) =: Just ()

      delEvents :: Event t (Map.Map Int (Maybe ())) <- do
        dels :: Dynamic t (Event t (Map.Map Int (Maybe ()))) <- mapDyn (leftmost . Map.elems . Map.map snd) xsAndDels
        return (switchPromptlyDyn dels)

  dels <- mapDyn (Map.map snd) xsAndDels

  r :: Dynamic t (Map.Map Int (Dynamic t (Either String r))) <- mapDyn (Map.map fst) xsAndDels

  let rs = joinDynThroughMap r
  forDyn rs $ fmap Map.elems . sequence





note :: String -> Maybe a -> Either String a
note e Nothing = Left e
note _ (Just a) = Right a

apDyn :: MonadWidget t m => m (Dynamic t (a -> b)) -> Dynamic t a -> m (Dynamic t b)
apDyn mf a = do
  f <- mf
  combineDyn ($) f a

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

liftA5 :: Applicative f
       => (a -> b -> c -> d -> e -> r)
       -> f a -> f b -> f c -> f d -> f e
       -> f r
liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e

