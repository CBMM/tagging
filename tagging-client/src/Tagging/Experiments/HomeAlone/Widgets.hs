{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeFamilies              #-}

module Tagging.Experiments.HomeAlone.Widgets where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as A
import           Data.Default
import           GHC.Int
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Time
import           Reflex.Dom.Xhr

import           Tagging.Response
import           Tagging.Stimulus
import           Tagging.User
import           Experiments.HomeAlonePersonAndDirection

-- TODO: 'get' a new stim by posting the answer to this one,
--       rather than two different calls
-- TODO: Drop the Maybe part of Maybe HeadDirection in ChacartecAtDir;
--       All characters have direction now that "Noone" is not a character

------------------------------------------------------------------------------
-- | Widget collecting all of the selectable characters. Uses the List
--   Of currently-selected characters for fading the already-selected faces
--   Returns event stream of face-clicks. Upstream should interpret a Name
--   event as a singal to TOGGLE whether that character is selected
--   A search box is included for locating characters by name fragment
choiceBankWidget
  :: MonadWidget t m
  => [CharacterName]           -- ^ List of all characters
  -> Dynamic t [CharacterName] -- ^ List of currently selected characters
  -> m (Event t CharacterName)
choiceBankWidget allChars dynSelChars = mdo

  -- A face is gray (status: Nothing) because either:
  --  1. The text-box is non-empty and the face is a non-match
  --  2. The face is already selected
  -- Otherwise status: Just (name colored with search string)
  nameAndStatus <- combineDyn (\(sText :: String) selChars ->
    let low      = T.toLower
        isIn a b = low (T.pack a) `T.isInfixOf` low b
        notSel   = (`notElem` selChars)
        frag a b = T.unpack a `fragmentWith` T.unpack b
    in Map.fromList $ ffor allChars
                      (\c -> if   sText `isIn` c && notSel c
                             then (c, Just (c `frag` T.pack sText))
                             else (c, Nothing))
    ) searchText' dynSelChars

  ns <- elClass "div" "bank-container" $ oneFromMap <$>
          listViewWithKey nameAndStatus choiceBankSingleChoice

  searchText' <- elClass "div" "search-div" $ mdo
    tBox <- textInput (def {_textInputConfig_setValue = clearEvents})
    (e, _) <- elAttr' "div" ("class" =: "search-clear-button") $ return ()
    let clearEvents = fmap (const "") $ domEvent Click e
    return $ _textInput_value tBox

  return ns


------------------------------------------------------------------------------
-- | Utility function for pulling one (arbitrary) event out of a Map of events
oneFromMap :: Reflex t => Event t (Map.Map k a) -> Event t a
oneFromMap = fmapMaybe (listToMaybe . Map.elems)


------------------------------------------------------------------------------
-- | Auxiliary funciton for choiceBankWidget, drawing one choice bank entry
choiceBankSingleChoice
  :: MonadWidget t m
  => CharacterName
  -> Dynamic t (Maybe (String,String,String))
  -> m (Event t CharacterName)
choiceBankSingleChoice n dynStatus = do
    divAttrs <- forDyn dynStatus $ \s ->
           "class"   =: "choice-bank-choice"
        <> "style" =: bool "opacity:1" "opacity:0.25" (isNothing s)

    dynText <- forDyn dynStatus $ \case
      Nothing                 -> text (T.unpack n)
      Just (pre,matched,post) -> do
        text pre
        elClass "span" "choice-bank-text-match" (text matched)
        text post

    (d,_) <- elDynAttr' "div" divAttrs $ do
      elAttr "img" ("src" =: nameToFile (T.unpack n)) (return ())
      elClass "div" "choice-bank-choice-text" $ dyn dynText
    return (n <$ domEvent Click d)


-- -----------------------------------------------------------------------------
-- pageWidget :: forall t m .MonadWidget t m => TaggingUser -> m ()
-- pageWidget TaggingUser{..} = mdo
--
--   -- This div holds vids, user's answer, and submit button
--   elClass "div" "main-div" $ mdo
--
--     pb <- getPostBuild
--     getAfterSubmit <- delay 0.1 submits
--     let getStim = pb <> getAfterSubmit
--
--     stims <- fmapMaybe id <$> getAndDecode ("/api/posinfo" <$ getStim)
--
--     elClass "div" "question-div" $
--           movieWidget stims
--
--     qWidget  <- questionWidget stims bankEvents
--
--     buttonAttrs <- forDyn qWidget
--                    (bool ("invalid" =: "invalid") mempty . isValid)
--     buttonText  <- forDyn qWidget (bool "Submit" "Noone" . null)
--     btn <- fmap fst $ elDynAttr' "button" buttonAttrs $ dynText buttonText
--     let submits = domEvent Click btn
--
--     performRequestAsync $ tag (current submitReq) submits
--     submitReq <- forDyn qWidget $ \cs ->
--       XhrRequest "POST" "/api/response" $
--       XhrRequestConfig ("Content-Type" =: "application/json") Nothing Nothing
--         Nothing (Just . BSL.unpack $ A.encode
--                  (ResponsePayload (T.decodeUtf8 . BSL.toStrict $ A.encode cs)))
--
--     return ()
--
--   -- Another div for the choice-bank
--   bankEvents <- optionBankWidget
--
--
--
--   return ()
--
--
-- -----------------------------------------------------------------------------
-- isValid :: Answer HomeAloneExperiment -> Bool
-- isValid = all (isJust . cadDir)
--
-- -----------------------------------------------------------------------------
-- data HACommand =  AnswerAdd      CharacterAtDir
--                |  AnswerDel      CharacterAtDir
--                |  AnswerTogl     CharacterAtDir
--                |  AnswerClear
--                |  HighlightDir   (Maybe HeadDirection)
--   deriving (Eq, Show)
--
-- type UIState = (Answer HomeAloneExperiment, Maybe HeadDirection)
--
-- -----------------------------------------------------------------------------
-- doCommand :: HACommand -> UIState -> UIState
-- doCommand (AnswerAdd x)    (a,h) = (bool (a ++ [x]) a (x `elem` a), h)
-- doCommand (AnswerDel x)    (a,h) = (a L.\\ [x],    h)
-- doCommand AnswerClear      (_,h) = ([],            h)
-- doCommand (HighlightDir d) (a,_) = (a,             d)
-- doCommand (AnswerTogl x)   (a,h)
--   | x `elem` a = doCommand (AnswerDel x) (a,h)
--   | otherwise  = doCommand (AnswerAdd x) (a,h)
--
--
-- -----------------------------------------------------------------------------
-- questionWidget :: forall t m. MonadWidget t m
--                => Event t PositionInfo
--                -> Event t HACommand
--                -> m (Dynamic t (Answer HomeAloneExperiment))
-- questionWidget p cmds = mdo
--
--   pb <- getPostBuild :: m (Event t ())
--
--   res <- foldDyn doCommand ([], Nothing) (leftmost [cmds, ansRes])
--   answer <- mapDyn fst res
--
--   ansRes <- answerDisplay answer
--   return answer
--
--
-- -----------------------------------------------------------------------------
-- answerDisplay :: forall t m. MonadWidget t m
--                => Dynamic t [CharacterAtDir]
--                -> m (Event t HACommand)
-- answerDisplay dynXs =
--   elClass "div" "response" $ do
--     charsMap <- mapDyn (Map.fromList . zip [0..]) dynXs
--     eventMap <- listViewWithKey charsMap answerDisplayOne
--     let eventList = fmapMaybe (listToMaybe . Map.elems) eventMap
--     return $ eventList
--
-- answerDisplayOne :: MonadWidget t m
--                  => Int
--                  -> Dynamic t CharacterAtDir
--                  -> m (Event t HACommand)
-- answerDisplayOne k dynChar = do
--         let baseUrl = "http://web.mit.edu/greghale/Public/hapics/" :: String
--         picAttr <- forDyn (dynChar) $ \(CharacterAtDir cName mDir) ->
--           ("src" =: nameToFile (T.unpack cName))
--           <> ("class" =: "answer-face-face")
--         dirAttr <- forDyn dynChar $ \(CharacterAtDir cName mDir) ->
--           maybe ("display" =: "none")
--                        (\d -> ("src" =: (baseUrl ++ show d ++ ".png")
--                               <> ("class" =: "answer-face-dir"))) mDir
--         elClass "div" "answer-face" $ mdo
--           elDynAttr "img" picAttr $ return ()
--           elDynAttr "img" dirAttr $ return ()
--           btnAttr <- holdDyn ("class" =: "fa fa-times") $
--                      leftmost [ domEvent Mouseenter btn $>
--                                   ("class" =: "fa fa-times-circle")
--                               , domEvent Mouseleave btn $>
--                                   ("class" =: "fa fa-times")
--                               ]
--           btn <- fmap fst $ elDynAttr' "span" btnAttr $
--                  return ()
--           return (AnswerDel <$> tag (current dynChar) (domEvent Click btn))
--
--
-- -----------------------------------------------------------------------------
-- movieWidget :: MonadWidget t m => Event t PositionInfo -> m ()
-- movieWidget pEvent = do
--
--   let movieSrc   = \PositionInfo{..} ->
--                      ssBaseUrl (snd piStimulusSequence) <> "/"
--                      <> srUrlSuffix (snd piStimulusResource)
--       movieAttrs = \p -> "src"  =: movieSrc p
--                       <> "type" =: srMimeType (snd $ piStimulusResource p)
--
--   movieAttrs <- widgetHold (text "waiting")
--     (ffor pEvent $ \p ->
--       elAttr "video" ("width" =: "320"
--                       <> "height" =: "240"
--                       <> "controls" =: "controls") $ do
--       elAttr "source" (Map.map T.unpack $ movieAttrs p)
--         (return ())
--     )
--
--   return ()
--
--
-- -----------------------------------------------------------------------------
-- -- A listing of all possible faces, filtered by text typed so far
-- -- Returns: stream of clicked directional characters,
-- --          and head direction button mouse-enter/leave events
-- optionBankWidget :: MonadWidget t m => m (Event t HACommand)
-- optionBankWidget = elClass "div" "bank" $ mdo
--   cmdMap        <- elClass "div" "bank-container" $
--                    listViewWithKey
--                        (constDyn choicesMap)
--                        (oneChoiceWidget searchBox)
--
--   searchBox  <- fmap _textInput_value $ elClass "div" "search-div" $ do
--     elClass "label" "search-label" $ text "Search:"
--     elClass "div"   "search-input" $ textInput def
--
--   return $ fmapMaybe id (fmap (listToMaybe . Map.elems) cmdMap)
--
--
-- -----------------------------------------------------------------------------
-- -- Dom components for a single face with name (no direction info)
-- oneChoiceWidget :: MonadWidget t m
--                 => Dynamic t String
--                 -> String
--                 -> Dynamic t String
--                 -> m (Event t HACommand)
-- oneChoiceWidget searchString n dynPath = elClass "div" "bank-item" $ do
--   divAttrs <- combineDyn
--               (\s p -> let isIn  = T.toLower (T.pack s)
--                                    `T.isInfixOf`
--                                    T.toLower (T.pack n)
--                            style = "opacity: " <> bool "0.4" "1.0" isIn
--                        in "style" =: style
--                           <> "class" =: "one-choice")
--                searchString dynPath
--   elDynAttr "div" divAttrs $ mdo
--     imgAttrs <- mapDyn (("class" =: "head-pic" <>) . ("src" =:)) dynPath
--     headDir  <- holdDyn Nothing moveEvents
--     headDirIndicator headDir
--     elDynAttr "img" imgAttrs $ return ()
--     dynSearchAct <- searchText searchString n
--     dyn dynSearchAct
--     (clickEvents, moveEvents) <- headDirButtons (T.pack n)
--     return clickEvents
--
-- -----------------------------------------------------------------------------
-- headDirButtons :: MonadWidget t m
--                => CharacterName
--                -> m (Event t HACommand, Event t (Maybe HeadDirection))
-- headDirButtons n = elClass "div" "head-dir-button-container" $ do
--   bs <- forM [HDLeft .. HDRight] $ \d -> do
--     b <- elAttr' "div" ("class" =: "head-dir-button") $ return ()
--     let cAtD = \a b -> AnswerTogl $ CharacterAtDir a (Just b)
--     let moveEvents =
--           leftmost [ (Just d) <$ domEvent Mouseenter (fst b)
--                    , Nothing  <$ domEvent Mouseleave (fst b)
--                    ]
--         clickEvents =  cAtD n d  <$ domEvent Click      (fst b)
--     return (clickEvents, moveEvents)
--   return (leftmost $ map fst bs, leftmost $ map snd bs)
--
--
-- -----------------------------------------------------------------------------
-- headDirIndicator :: MonadWidget t m
--                  => Dynamic t (Maybe HeadDirection)
--                  -> m ()
-- headDirIndicator hd = elClass "div" "head-dir-indicator" $ do
--   let picSrc :: Maybe HeadDirection -> String
--       picSrc h = "http://web.mit.edu/greghale/Public/hapics/"
--                  <> maybe "HDWhite" show h
--                  <> ".png"
--       picDisp  = bool mempty ("hidden" =: "") . isNothing
--   atTrs <- forDyn hd $ \h -> ("src" =: picSrc h) <> (picDisp h)
--   elDynAttr "img" atTrs $ return ()
--
--


-----------------------------------------------------------------------------
-- Utility that renders a string according to a search query that may hit
fragmentWith :: String -> String -> (String, String, String)
fragmentWith source query =

  let qSource = T.pack source
      qText   = (T.toLower . T.pack) query
  in  if (T.null qText)
      then (source, "", "")
      else let breakPoint = T.length . fst . T.breakOn qText . T.toLower
                            $ qSource
               (p0,pTemp) = T.splitAt breakPoint qSource
               (p1,p2)    = T.splitAt (T.length qText) pTemp
           in  (T.unpack p0, T.unpack p1, T.unpack p2)


-----------------------------------------------------------------------------
-- Utility that renders a string according to a search query that may hit
searchText :: MonadWidget t m
           => Dynamic t String
           -> String
           -> m (Dynamic t (m ()))
searchText query source = do

  let qSource = T.pack source
  qText <- mapDyn (T.toLower . T.pack) query
  dynAction <-
    forDyn qText $ \q ->
      case not (T.null q) && T.toLower q `T.isInfixOf` T.toLower qSource of
        False -> el "h2" $ text source
        True  ->
          let breakPoint = T.length . fst . T.breakOn q . T.toLower $ qSource
              (p0,pTemp) = T.splitAt breakPoint qSource
              (p1,p2)    = T.splitAt (T.length q) pTemp
          in el "h2" $ do
            text (T.unpack p0)
            elClass "span" "text-found" $ text (T.unpack p1)
            text (T.unpack p2)
  return dynAction


-----------------------------------------------------------------------------
-- Listing of names and paths to their pics (hard-coded for now. TODO serve)
choices :: [CharacterName]
choices= ["Kevin McC" ,"Tracy McC" ,"Sondra McC" ,"Rod McC" ,"Rob McC"
         ,"Buzz McC" ,"Peter McC" ,"Other" ,"Other (Major)" ,"Other (Minor)"
         ,"Not Sure" ,"Mrs. Stone" ,"Mr. Hector" ,"Mr. Duncan"
         ,"Megan McC" ,"Marv Merch" ,"Linnie McC" ,"Leslie McC" ,"Kate McC"
         ,"Jeff McC" ,"Harry Lyme" ,"Fuller McC" ,"Frank McC" ,"Cedric"
         ,"Buzz McC" ,"Brooke McC" ,"Bird Lady"
         ]

choicesMap :: Map.Map String String
choicesMap = Map.fromList $
             map (\n -> (T.unpack n, nameToFile (T.unpack n))) choices

nameToFile :: String -> String
nameToFile = ("http://web.mit.edu/greghale/Public/hapics/" <>)
             . (<> ".png")
             . filter (`notElem` ("() ." :: String))
