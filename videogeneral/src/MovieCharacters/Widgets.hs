{-#  LANGUAGE DeriveGeneric       #-}
{-#  LANGUAGE LambdaCase          #-}
{-#  LANGUAGE RecursiveDo         #-}
{-#  LANGUAGE RankNTypes          #-}
{-#  LANGUAGE TupleSections       #-}
{-#  LANGUAGE ScopedTypeVariables #-}

module MovieCharacters.Widgets where

import Control.Monad (join)
import Control.Arrow (second)
import Data.Default (Default, def)
import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.Dom.Contrib.Widgets.ButtonGroup

import  MovieCharacters.MetaData


-------------------------------------------------------------------------------
data CharacterSelectionBankConfig t = CharacterSelectionBankConfig
  { _characterSelectionBankConfig_initialCharacters :: [Character]
  , _characterSelectionBankConfig_setCharacters     :: Event t [Character]
  }

instance Reflex t => Default (CharacterSelectionBankConfig t) where
  def = CharacterSelectionBankConfig [] never

-------------------------------------------------------------------------------
data CharacterSelectionBank t = CharacterSelectionBank
  { _characterSelectionBank_characters       :: Dynamic t (Map Int Character)
  , _characterSelectionBank_selClicks        :: Event t (Map Int Character)
  , _characterSelectionBank_stablePropClicks :: Event t Character
  }



data CharacterClickType = CharClickSelect | CharClickStable
  deriving (Eq)

-------------------------------------------------------------------------------
characterSelectionBank :: forall t m.MonadWidget t m
                       => CharacterSelectionBankConfig t
                       -> m (CharacterSelectionBank t)
characterSelectionBank (CharacterSelectionBankConfig initCs setCs) =
  divClass "character-selection-bank" $ do

    chars <- holdDyn initCs setCs >>= mapDyn (Map.fromList . zip [0..])
    rec clicks <- listViewWithKey chars (characterBankCharacter searchString)
        searchString :: Dynamic t String <- divClass "search-box" $ do
          text "Search"
          value <$> textArea def

    let selClicks   = ffor clicks
          (Map.map (snd . snd) . Map.filter ( (== CharClickSelect) . fst))
        propsClicks = fforMaybe clicks
          ( fmap fst
          . Map.minView
          . Map.map (snd . snd)
          . Map.filter ( (== CharClickStable) . fst))
    return $ CharacterSelectionBank chars selClicks propsClicks


-------------------------------------------------------------------------------
characterBankCharacter
  :: MonadWidget t m => Dynamic t String -> Int -> Dynamic t Character
  -> m (Event t (CharacterClickType, (Int, Character)))
characterBankCharacter search ind c = divClass "single-character-icon" $ do
  imgAttrs <- forDyn c $ ("src" =:) . T.unpack . characterPicUrl
  img <- fst <$> elDynAttr' "img" imgAttrs (return ())
  el "span" $ dynText =<< mapDyn (T.unpack . characterDisplayName) c
  propsBtn <- fmap fst $ elAttr' "div" ("class" =: "stable-props-button") $ do
    elAttr "span" ("class" =: "glyphicon glyphicon-info"
                <> "aria-hidden" =: "true") (return ())
  -- TODO: hook up the search filtering
  return $ leftmost [fmap ((CharClickSelect,) . (ind,))
                     (tag (current c) (domEvent Click img))
                    ,fmap ((CharClickStable,) . (ind,))
                     (tag (current c) (domEvent Click propsBtn))]


data CharacterSelectedSetConfig t = CharacterSelectedSetConfig
  { _characterSelectedSetConfig_setSelection    :: Event t (Map Int (Maybe Character))
  , _characterSelectedSetConfig_initalSelection :: Map.Map Int Character
  , _characterSelectedSetConfig_intialFocus     :: Maybe Character
  , _characterSelectedSetConfig_setFocus        :: Event t (Maybe Character)
  }

data CharacterSelectedSet t = CharacterSelectedSet
  { _characterSelectedSet_focus :: Dynamic t (Maybe Character)
  , _characterSelectedSet_characters :: Dynamic t (Map Int Character)
  }


-------------------------------------------------------------------------------
characterSelectedSet
  :: forall t m.MonadWidget t m => CharacterSelectedSetConfig t
  -> m (CharacterSelectedSet t)
characterSelectedSet (CharacterSelectedSetConfig sel sel0 foc0 foc) = do

  charsAndClicks <- listWithKeyShallowDiff sel0 sel iconAndFocuses

  iconFocuses <- fmap switchPromptlyDyn $
    mapDyn (leftmost . map snd . Map.elems) charsAndClicks

  let insertFocuses = ffor sel $ (join . fmap fst) <$> Map.minView

  internalFocus <- holdDyn Nothing (leftmost [insertFocuses, iconFocuses])
  characters <- mapDyn (Map.map fst) charsAndClicks
  return $ CharacterSelectedSet internalFocus characters

  where iconAndFocuses :: Int -> Character -> Event t Character
                       -> m (Character, Event t (Maybe Character))
        iconAndFocuses key c e = divClass "single-character-icon" $ do
          let imgAttrs = "src" =: T.unpack (characterPicUrl c)
          -- imgAttrs <- forDyn c ("src" =:) . T.unpack . characterPicUrl
          img <- fst <$> elAttr' "img" imgAttrs (return ())
          return $ (c, Just c <$ domEvent Click img)


-------------------------------------------------------------------------------
questionWidget :: forall t m.MonadWidget t m
               => QuestionSpec
               -> m (Dynamic t [Either String (String, Maybe String)])
questionWidget (QuestionSpec optional name label q) = divClass "q-group" $ do

  elAttr "label" ("for" =: T.unpack name) (text $ T.unpack label)

  divClass "input-group" $ do
    rec qRes <- qContainer name validResult $ case q of

          QButtonsChoice cs -> do
            let btns = constDyn $ (\(Choice cID cLabel) ->
                                    (T.unpack cID, T.unpack cLabel)) <$> cs
            fmap value $ bootstrapButtonGroup btns def
              { _widgetConfig_attributes =
                constDyn ("class" =: "btn-group btn-group-xs"
                       <> "role" =: "group")}

          QRadioChoice cs -> do
            let btns = constDyn $ (\(Choice cID cLabel) ->
                                    (T.unpack cID, T.unpack cLabel)) <$> cs
                radioName = "radiogroup-" ++ T.unpack name
            fmap value $ radioGroup (constDyn radioName) btns def

        let validate :: Maybe String -> Either String (Maybe String)
            validate = if optional
                       then Right
                       else maybe (Left $ "Required: " ++ T.unpack label)
                                  (Right . Just)
        validResult <- mapDyn validate qRes
        return (validResult :: Dynamic t (Either String (Maybe String)))

    mapDyn ((:[]) . fmap (T.unpack name,)) validResult

  where qContainer n validAnswer m = do
          qAttrs <- forDyn validAnswer $ \case
            Right _ -> "class" =: "question"
            Left  _ -> "class" =: "question invalid"
          elDynAttr "div" qAttrs m

-------------------------------------------------------------------------------
questionGroup
  :: forall t m. MonadWidget t m
  => [QuestionSpec]
  -> m (Dynamic t (Either String [(String, Maybe String)]))
questionGroup qs = divClass "character-questions" $ do
  dyns <- mconcatDyn =<< traverse questionWidget qs
  mapDyn sequence dyns



-------------------------------------------------------------------------------
characterClipQuestionsWidget
  :: MonadWidget t m
  => [QuestionSpec]
  -> m (Dynamic t (Either String [(String, Maybe String)]))
characterClipQuestionsWidget qs = divClass "character-questions" $
  questionGroup qs

