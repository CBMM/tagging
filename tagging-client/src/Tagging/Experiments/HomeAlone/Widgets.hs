{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ExistentialQuantification #-}

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


-----------------------------------------------------------------------------
pageWidget :: forall t m .MonadWidget t m => TaggingUser -> m ()
pageWidget TaggingUser{..} = mdo

  pb <- getPostBuild
  getAfterSubmit <- delay 0.1 submits
  let getStim = pb <> getAfterSubmit

  stims <- fmapMaybe id <$> getAndDecode ("/api/posinfo" <$ getStim)

  elClass "div" "question-div" $
        movieWidget stims

  qWidget  <- questionWidget stims bankEvents


  searchBox  <- _textInput_value <$> textInput def
  bankEvents <- optionBankWidget searchBox

  buttonAttrs <- forDyn qWidget
                 (bool ("invalid" =: "invalid") mempty . isValid)
  buttonText  <- forDyn qWidget (bool "Submit" "Noone" . null)
  btn <- fmap fst $ elDynAttr' "button" buttonAttrs $ dynText buttonText
  let submits = domEvent Click btn

  return ()


-----------------------------------------------------------------------------
isValid :: Answer HomeAloneExperiment -> Bool
isValid = all (isJust . cadDir)

-----------------------------------------------------------------------------
data HACommand =  AnswerAdd      CharacterAtDir
               |  AnswerDel      CharacterAtDir
               |  AnswerTogl     CharacterAtDir
               |  HighlightDir   (Maybe HeadDirection)
  deriving (Eq, Show)

type UIState = (Answer HomeAloneExperiment, Maybe HeadDirection)

-----------------------------------------------------------------------------
doCommand :: HACommand -> UIState -> UIState
doCommand (AnswerAdd x)    (a,h) = (bool (a ++ [x]) a (x `elem` a), h)
doCommand (AnswerDel x)    (a,h) = (a L.\\ [x],    h)
doCommand (HighlightDir d) (a,_) = (a,             d)
doCommand (AnswerTogl x)   (a,h)
  | x `elem` a = doCommand (AnswerDel x) (a,h)
  | otherwise  = doCommand (AnswerAdd x) (a,h)

-----------------------------------------------------------------------------
questionWidget :: forall t m. MonadWidget t m
               => Event t PositionInfo
               -> Event t HACommand
               -> m (Dynamic t (Answer HomeAloneExperiment))
questionWidget p cmds = mdo

  pb <- getPostBuild :: m (Event t ())

  res <- foldDyn doCommand ([], Nothing) (leftmost [cmds, ansRes])
  headDirIndicator =<< forDyn res snd
  answer <- mapDyn fst res

  ansRes <- answerDisplay answer
  --let a = ansRes :: Int
  return answer


answerDisplay :: forall t m. MonadWidget t m
               => Dynamic t [CharacterAtDir]
               -> m (Event t HACommand)
answerDisplay dynXs =
  elClass "div" "response" $ do
    charsMap <- mapDyn (Map.fromList . zip [0..]) dynXs
    eventMap <- listViewWithKey charsMap answerDisplayOne
    let eventList = fmapMaybe (listToMaybe . Map.elems) eventMap
    return $ eventList

answerDisplayOne :: MonadWidget t m => Int -> Dynamic t CharacterAtDir -> m (Event t HACommand)
answerDisplayOne k dynChar = do
        let baseUrl = "http://web.mit.edu/greghale/Public/hapics/" :: String
        picAttr <- forDyn (dynChar) $ \(CharacterAtDir cName mDir) ->
          ("src" =: nameToFile (T.unpack cName))
          <> ("class" =: "answer-face-face")
        dirAttr <- forDyn dynChar $ \(CharacterAtDir cName mDir) ->
          maybe ("display" =: "none")
                       (\d -> ("src" =: (baseUrl ++ show d ++ ".png")
                              <> ("class" =: "answer-face-dir"))) mDir
        elClass "div" "answer-face" $ do
          elDynAttr "img" picAttr $ return ()
          elDynAttr "img" dirAttr $ return ()
          btn <- fmap fst $ elAttr' "button" ("class" =: "del-button") $ text "x"
          return (AnswerDel <$> tag (current dynChar) (domEvent Click btn))


-----------------------------------------------------------------------------
movieWidget :: MonadWidget t m => Event t PositionInfo -> m ()
movieWidget pEvent = do

  let movieSrc   = \PositionInfo{..} ->
                     ssBaseUrl (snd piStimulusSequence) <> "/"
                     <> srUrlSuffix (snd piStimulusResource)
      movieAttrs = \p -> "src"  =: movieSrc p
                      <> "type" =: srMimeType (snd $ piStimulusResource p)

  movieAttrs <- widgetHold (text "waiting")
    (ffor pEvent $ \p ->
      elAttr "video" ("width" =: "320"
                      <> "height" =: "240"
                      <> "controls" =: "controls") $ do
      elAttr "source" (Map.map T.unpack $ movieAttrs p)
        (return ())
    )

  return ()


-----------------------------------------------------------------------------
-- A listing of all possible faces, filtered by text typed so far
-- Returns: stream of clicked directional characters,
--          and head direction button mouse-enter/leave events
optionBankWidget :: MonadWidget t m
                 => Dynamic t String
                 -> m (Event t HACommand)
optionBankWidget searchString = elClass "div" "bank-container" $ do
  cmdMap        <- listViewWithKey
                       (constDyn choicesMap)
                       (oneChoiceWidget searchString)
  return $ fmapMaybe id (fmap (listToMaybe . Map.elems) cmdMap)


-----------------------------------------------------------------------------
-- Dom components for a single face with name (no direction info)
oneChoiceWidget :: MonadWidget t m
                => Dynamic t String
                -> String
                -> Dynamic t String
                -> m (Event t HACommand)
oneChoiceWidget searchString n dynPath = elClass "div" "bank-item" $ do
  divAttrs <- combineDyn
              (\s p -> let isIn  = T.toLower (T.pack s)
                                   `T.isInfixOf`
                                   T.toLower (T.pack n)
                           style = bool "opacity: 0.4" "opacity: 1.0" isIn
                       in "src" =: p
                          <> "style" =: style
                          <> "class" =: "one-choice")
               searchString dynPath
  elDynAttr "div" divAttrs $ do
    imgAttrs <- mapDyn ("src" =:) dynPath

    elDynAttr "img" imgAttrs $ return ()
    dynSearchAct <- searchText searchString n
    dyn dynSearchAct
    mouseEvents <- headDirButtons (T.pack n)
    return mouseEvents

-----------------------------------------------------------------------------
headDirButtons :: MonadWidget t m => CharacterName -> m (Event t HACommand)
headDirButtons n = elClass "div" "head-dir-button-container" $ do
  bs <- forM [HDLeft .. HDRight] $ \d -> do
    b <- elAttr' "div" ("class" =: "head-dir-button") $ return ()
    let cAtD = \a b -> AnswerTogl $ CharacterAtDir a (Just b)
    return $ leftmost [ HighlightDir (Just d) <$ domEvent Mouseenter (fst b)
                      , HighlightDir Nothing  <$ domEvent Mouseleave (fst b)
                      , cAtD n d              <$ domEvent Click      (fst b)
                      ]
  return $ leftmost bs


-----------------------------------------------------------------------------
headDirIndicator :: MonadWidget t m
                 => Dynamic t (Maybe HeadDirection)
                 -> m ()
headDirIndicator hd = elClass "div" "head-dir-indicator" $ do
  let picSrc :: Maybe HeadDirection -> String
      picSrc h = "http://web.mit.edu/greghale/Public/hapics/"
                 <> maybe "HAWhite" show h
                 <> ".png"
  atTrs <- forDyn hd $ \h -> "src" =: picSrc h
  elDynAttr "img" atTrs $ return ()


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
choicesMap :: Map.Map String String
choicesMap = Map.fromList $ map (\n -> (n, nameToFile n))
             ["Kevin McC" ,"Tracy McC" ,"Sondra McC" ,"Rod McC" ,"Rob McC"
             ,"Buzz McC" ,"Peter McC" ,"Other" ,"Other (Major)" ,"Other (Minor)"
             ,"Not Sure" ,"Mrs. Stone" ,"Mr. Hector" ,"Mr. Duncan"
             ,"Megan McC" ,"Marv Merch" ,"Linnie McC" ,"Leslie McC" ,"Kate McC"
             ,"Jeff McC" ,"Harry Lyme" ,"Fuller McC" ,"Frank McC" ,"Cedric"
             ,"Buzz McC" ,"Brooke McC" ,"Bird Lady"
             -- ,"Nobody"
             ]

nameToFile :: String -> String
nameToFile = ("http://web.mit.edu/greghale/Public/hapics/" <>)
             . (<> ".png")
             . filter (`notElem` ("() ." :: String))
