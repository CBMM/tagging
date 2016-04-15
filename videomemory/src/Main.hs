{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens
import           Control.Monad              (mzero, zipWithM)
import           Data.Aeson
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as A
import           Data.Bool
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char                  (toLower)
import           Data.Either
import           Data.Foldable
import           Data.Maybe                 (catMaybes)
import           Data.Monoid
import qualified Data.Text                  as T
import           Data.Traversable           (forM)
import qualified Data.Vector                as Vector
import           GHC.Generics
import           MediaElement
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Tagging.Response
import           Tagging.Stimulus
import           Tagging.User


------------------------------------------------------------------------------
data ClipResponse = ClipResponse
  { _crRemember :: Bool
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON   ClipResponse
instance FromJSON ClipResponse


------------------------------------------------------------------------------
data Survey = Survey
  { _sName                :: String
  , _sHasSeenTestEpisode  :: Bool
  , _sHasSeenMoreEpisodes :: Bool
  , _sVideoTestWritten    :: String
  , _sVideoTestSpoken     :: String
  } deriving (Eq, Show, Generic)

instance ToJSON   Survey
instance FromJSON Survey


------------------------------------------------------------------------------
data Response = RClip   ClipResponse
              | RSurvey Survey
              | RQuiz   MemoryQuiz
  deriving (Eq, Show, Generic)

instance ToJSON   Response
instance FromJSON Response

data MemoryQuiz = MemoryQuiz
  { _mqCharacterName   :: String
  , _mqCharacterChoice :: String
  , _mqGenre           :: String
  , _mqHasBadGuys      :: Bool
  , _mqWhoBadGuys      :: String
  , _mqLocation        :: String
  , _mqLocationChoice  :: String
  , _mqLandmark        :: String
  , _mqFamousRole      :: String
  , _mqPresidentRace   :: String
  , _mqBlackAndWhite   :: Bool
  } deriving (Eq, Show, Generic)

makeLenses ''MemoryQuiz

instance A.ToJSON MemoryQuiz where
  toJSON = A.genericToJSON A.defaultOptions
    { A.fieldLabelModifier = drop 3 }

instance A.FromJSON MemoryQuiz where
  parseJSON = A.genericParseJSON A.defaultOptions
    { A.fieldLabelModifier = drop 3 }

quiz0 = MemoryQuiz "" "" "" True "" "" "" "" "" "" True

------------------------------------------------------------------------------
memoryQuiz :: MonadWidget t m => m (Event t MemoryQuiz)
memoryQuiz = questionSequence qs
 where
  qs = [memoryQuestion (bootstrapLabeledInput
                        "What was the name of the main character?"
                        "mqCharacterName" vNotNull) (set mqCharacterName)
       , memoryQuestion (radioMultichoice
                         "What was the name of the main character?"
                         "mqCharacterChoice"
                         (map (\x -> (x,x))
                          ["Mike Stronson","John Novak","Jack Bauer"
                          ,"President Barn","Jennifer Anniston"]) someChoice)
         (set mqCharacterChoice)
       , memoryQuestion
         (radioMultichoice
          "The genre of this movie would be best described as" "mqGenre"
          (map (\x -> (x,x)) ["Action","Comedy","Horror","Documentary"
                             ,"Sports"]) someChoice) (set mqGenre)
       , memoryQuestion (radioMultichoice
                         "Were there any bad guys in the movie?"
                         "mqHasBadGuys" [(False,"No"), (True,"Yes")]
                         someChoice) (set mqHasBadGuys)
       , memoryQuestion (bootstrapLabeledInput
                         "Who were the bad guys in the movie?" "mqWhoBadGuys"
                         vNotNull) (set mqWhoBadGuys)
       , memoryQuestion (bootstrapLabeledInput
                         "Name one location where the action takes place."
                         "mqLocation" vNotNull) (set mqLocation)
       , memoryQuestion (radioMultichoice
                         "Which of the following cities is shown in the movie?"
                         "mqLocationChoice"
                         (map (\x->(x,x)) ["Washington DC","London","Paris",
                                           "San Francisco","New York"])
                         someChoice) (set mqLocationChoice)
       , memoryQuestion
         (radioMultichoice
          "Which of the following landmarks is shown in the movie?"
          "mqLandmark" (map (\x->(x,x))
                        ["Eiffel Tower","Golden Gate Bridge", "White House",
                         "Big Ben Tower" ,"Times Square"]) someChoice)
         (set mqLandmark)
       , memoryQuestion
         (radioMultichoice
          "Which of th efollowing roles is depicted in the movie?"
          "mqFamousRole"
          (map (\x->(x,x)) ["Head coach of the Yankees team"
                           ,"A wealthy entrepeneur" ,"A musician"
                           ,"The president of the United States"
                           ,"Ambassador to England"]) someChoice)
         (set mqFamousRole)
       , memoryQuestion
         (radioMultichoice
          ("The president of the United States in the " ++
           "movie is represented by an actor who is " ++
           "best described as:") "mqPresidentRace"
          (map (\x->(x,x)) ["African American","Asian","Latino" ,"Caucasian"
                           ,"Native American"]) someChoice)
         (set mqPresidentRace)
       , memoryQuestion (radioMultichoice "Was the movie in black and white?"
                         "mqBlackAndWhite" [(False,"No"),(True,"Yes")]
                         someChoice) (set mqBlackAndWhite)
       ]

------------------------------------------------------------------------------
questionSequence :: forall t m.MonadWidget t m
                 => [m (Event t (MemoryQuiz -> MemoryQuiz))]
                 -> m (Event t MemoryQuiz)
questionSequence questions = mdo
  pb <- getPostBuild
  qWidgets <- zipListWithEvent
    (\i ma -> (i :: m (Event t (MemoryQuiz -> MemoryQuiz))))
    questions (leftmost [() <$ pb, () <$ qUpdates])
  qUpdates' <- widgetHold (return never) qWidgets
  let qUpdates = switchPromptlyDyn qUpdates'

  quiz :: Dynamic t MemoryQuiz <- foldDyn ($) quiz0 qUpdates
  nUpdates <- mapDyn succ =<< count (updated quiz)

  return (gate (fmap (>= length questions) (current nUpdates)) (updated quiz))


someChoice :: Maybe a -> Either String a
someChoice (Just x) = Right x
someChoice Nothing =
    Left "Please select one option, it's Ok if you have to guess."

choiceEq :: Eq a => a -> Maybe a -> Either String a
choiceEq _ Nothing = Left "Please choose an option."
choiceEq target (Just x)
    | target == x = Right x
    | otherwise   = Left "Sorry, that was incorrect"

vNotNull :: String -> Either String String
vNotNull "" = Left "Please try to guess"
vNotNull x  = Right x


------------------------------------------------------------------------------
memoryQuestion :: MonadWidget t m
               => (Event t () -> m (Dynamic t (Either String a)))
                  -- ^ Function from 'try-validate' events to dyn response
               -> (a -> MemoryQuiz -> MemoryQuiz)
                  -- ^ Quiz update function
               -> m (Event t (MemoryQuiz -> MemoryQuiz))
memoryQuestion question fUpdate = mdo
  r    <- mapDyn hush =<< question next
  next <- bootstrapButton "chevron-right"
  let quizUpdates = fmapMaybe (fmap fUpdate) (tag (current r) next)
  return quizUpdates


data TaskPhase = TaskPhaseSurvey
               | TaskPhaseMemoryQuiz
               | TaskPhaseTrial Int
               | TaskPhaseDone


------------------------------------------------------------------------------
trialSequence :: MonadWidget t m
              => (TaskPhase -> m (Event t Response))
              -> Progress
              -> m ()
trialSequence phaseTask (Progress nFinished nTrials) = do
  pb <- getPostBuild
  let ts = drop nFinished $
           [TaskPhaseSurvey, TaskPhaseMemoryQuiz]
           ++ map TaskPhaseTrial (enumFromTo 1 nTrials)
           ++ [TaskPhaseDone]
  rec trials    <- zipListWithEvent const
                                    (map phaseTask ts)
                                    (leftmost [() <$ responseAck, pb])
      responses <- switchPromptlyDyn <$> widgetHold (never <$ text "Loading") trials

      responseAck <- performRequestAsync $ ffor responses $ \(r :: Response) ->
        xhrRequest "POST" "/api/response?advance"
        (def { _xhrRequestConfig_headers  = "Content-Type" =: "application/json"
             , _xhrRequestConfig_sendData =
               Just (BSL.unpack $ A.encode (ResponsePayload (A.toJSON r)))})

  return ()

pleaseLogin :: MonadWidget t m => m ()
pleaseLogin = do
  text "There seems to be a problem. Perhaps you are not logged in?"
  el "br" fin
  elAttr "a" ("href" =: "/login") $ text "Login"

------------------------------------------------------------------------------
run :: forall t m.MonadWidget t m => m ()
run = elClass "div" "content" $ mdo
  pb <- getPostBuild
  progress <- getAndDecode ("/api/progress" <$ pb)

  widgetHold (return ()) (fmap (maybe pleaseLogin (trialSequence makeTrial)) progress)
  return ()

makeTrial :: MonadWidget t m => TaskPhase -> m (Event t Response)
makeTrial TaskPhaseSurvey     = text "survey" >> (fmap . fmap) RSurvey modal -- TODO Rename this
makeTrial TaskPhaseMemoryQuiz = text "quiz"   >> (fmap . fmap) RQuiz   memoryQuiz
makeTrial (TaskPhaseTrial n)  = mdo
  pb <- getPostBuild
  pos <- fmapMaybe id <$> getAndDecode ("/api/fullposinfo" <$ pb)
  responses <- elClass "div" "interaction" $
    widgetHold (text "Waiting ..." >> return never) (fmap videoQuestion pos)
  return $ (switchPromptlyDyn responses)



-- ------------------------------------------------------------------------------
-- run' :: forall t m.MonadWidget t m => m ()
-- run' = elClass "div" "content" $ mdo
--   pb <- getPostBuild

--   progress :: Event t Progress <- fmapMaybe id <$> getAndDecode ("/api/progress" <$ pb)

--   -- Show the modal survey until the server acks receipt of survey data
--   survEntry    <- switchPromptlyDyn <$> widgetHold modal (return never <$ survReceived)

--   -- memQuiz <- fmap switchPromptlyDyn $ widgetHold (return never) (undefined)

--   survReceived <- performRequestAsync $ ffor survEntry $ \(s :: Survey) ->
--     xhrRequest "POST" "/api/response"
--     (def { _xhrRequestConfig_sendData =
--            Just (BSL.unpack $ A.encode (ResponsePayload (A.toJSON (RSurvey s))))}) -- TODO finish

--   -- quizReceived <- performRequestAsync $ ffor memQuiz $ \(m :: MemoryQuiz) ->
--   --   xhrRequest "POST" "/api/response"
--   --   (def { _xhrRequestConfig_sendData =
--   --          Just (BSL.unpack $ A.encode (ResponsePayload (A.toJSON (RQuiz m))))})

--   let stimRequestTriggers = leftmost [pb, () <$ submitSuccess]
--   posTry <- getAndDecode ("/api/fullposinfo" <$ stimRequestTriggers)

--   let pos :: Event t (Assignment, StimulusSequence, StimSeqItem) = fmapMaybe id posTry

--   errorText (text "Failed to load position")
--             (True <$ ffilter (== Nothing) posTry)

--   responses :: Dynamic t (Event t Response) <- elClass "div" "interaction" $
--     widgetHold (text "Waiting..." >> return never) (fmap videoQuestion pos)

--   submitSuccess <- performRequestAsync
--     (ffor (switchPromptlyDyn responses) $ \(r :: Response) ->
--       XhrRequest "POST" "/api/response?advance" $
--       XhrRequestConfig ("Content-Type" =: "application/json")
--       Nothing Nothing Nothing (Just . BSL.unpack $ A.encode
--                                (ResponsePayload (A.toJSON r))))

--   return ()


------------------------------------------------------------------------------
videoQuestion :: forall t m.MonadWidget t m
              => (Assignment, StimulusSequence, StimSeqItem)
              -> m (Event t Response)
videoQuestion (asgn, stimseq, ssi) = do
  elClass "div" "videoandquestion" $ mdo

    let srcs = case ssiStimulus ssi of
                 A.Array fileNames -> catMaybes $
                   ffor (Vector.toList fileNames) $ \case
                     A.String fn -> Just $ T.unpack fn
                     _           -> Nothing
                 A.String fn -> [T.unpack fn]
    vid <- divClass "video-container" $ videoWidget
           (map (\s -> (T.unpack (ssBaseUrl stimseq)
                        <> "/" <> s, mimeOf s)) srcs)
           (def {_videoWidgetConfig_play = _videoWidget_canplaythrough vid
                ,_videoWidgetConfig_attributes = vidAttrs
                })

    btnsVis <- holdDyn False (True <$ _videoWidget_ended vid)
    vidAttrs  <- forDyn btnsVis $ bool mempty ("style" =: "opacity:0")
    btnsAttrs <- forDyn btnsVis $ (("class" =: "yesno") <>) . bool ("style" =: "opacity:0.25" <> "disabled" =: "true") mempty
    -- vid <- elAttr' "video" ("height"   =: "640"
    --                      <> "width"    =: "480"
    --                      <> "controls" =: "false") $ do
      -- case ssiStimulus ssi of
      --   (A.Array fileNames) -> do
      --     forM fileNames $ \(A.String fn) -> do
      --         elAttr "source" ("src" =: (T.unpack (ssBaseUrl stimseq) <> "/" <> T.unpack fn)
      --                    <> "type" =: mimeOf (T.unpack fn)) fin
      --     text "Sorry, this browser does not support our videos"
      --   _ -> text "Stimulus decoding error"
    rememb <- elDynAttr "div" btnsAttrs $ do
       text "Have you seen this clip?"
       ys <- fmap (True  <$) (button "Yes")
       ns <- fmap (False <$) (button "No")
       return $ leftmost [ys,ns]
    remembered <- holdDyn Nothing $ fmap Just rememb
    return $ fmap (RClip . ClipResponse) $ fmapMaybe id (updated remembered)


------------------------------------------------------------------------------
welcome :: MonadWidget t m => m (Event t ())
welcome = elClass "div" "welcome" $ do
  text $ unwords ["Thank you for participating in this experiment."
                 ,"Please watch the following video clips,"
                 ,"and after each one, indicate whether you saw this"
                 , "clip during the first phase of the experiment."]
  el "br" fin
  text $ "If you feel uncomfortable, you may stop the experiment at any time."
  text $ "Logging in later will bring you to your last spot in the experiment."
  button "Continue"


------------------------------------------------------------------------------
-- | An externally-prodded validating label & text-input
bootstrapLabeledInput :: forall t m a.MonadWidget t m
                      => String
                      -> String
                      -> (String -> Either String a)
                      -> Event t ()
                      -> m (Dynamic t (Either String a))
bootstrapLabeledInput label idattr validate eval =
  elClass "div" "input-group" $ mdo
    elAttr "label" ("for" =: idattr) $ text label
    attrs <- forDyn isOk $ \ok ->
      "type"  =: "text" <>
      "id"    =: idattr <>
      "class" =: bool "form-control invalid" "form-control valid" ok
    mapDyn (bool mempty ("class" =: "valid")) isOk

    v <- mapDyn validate =<<
         (fmap value $ textInput (def & attributes .~ attrs))
    isOk :: Dynamic t Bool <- mapDyn isRight v
    return v


------------------------------------------------------------------------------
-- | An externally-prodded validating label & radio-button group
radioMultichoice :: (MonadWidget t m, Eq a, Ord a, Show a)
                 => String
                 -> String
                 -> [(a, String)]
                 -> (Maybe a -> Either String a)
                 -> Event t ()
                 -> m (Dynamic t (Either String a))
radioMultichoice label idattr choices validate eval =
  elClass "div" "input-group" $ mdo
    grpAttr <- holdDyn True (tag (fmap isRight $ current r) eval) >>= \isOk ->
      forDyn isOk (\b -> "class" =:
                         bool "radio-group invalid" "radio-group valid" b)
    r <- elDynAttr "div" grpAttr $ mdo
      elAttr "label" ("for" =: idattr) (text label)
      rMaybe <- value <$> radioGroup
           (constDyn idattr)
           (constDyn choices)
           (def & widgetConfig_attributes .~ constDyn ("id" =: idattr))
      mapDyn validate rMaybe
    return r

bootstrapButton :: MonadWidget t m => String -> m (Event t ())
bootstrapButton glyphShortname = (domEvent Click . fst) <$>
  elAttr' "span" ("class" =: (prfx <> glyphShortname)) (return ())
  where prfx = "glyphicon glyphicon-"

modal :: MonadWidget t m => m (Event t Survey)
modal = do
  elClass "div" "modal-background" $ survey

------------------------------------------------------------------------------
-- | Survey questions, emits a Survey when form is filled out right
survey :: MonadWidget t m => m (Event t Survey)
survey = elClass "form" "survey" $ mdo
  pb <- getPostBuild
  el "h1" $ text "Welcome!"
  surv <- elClass "div" "qs-and-vid" $ do

    surv <- elClass "div" "questions" $ do
      name  <- bootstrapLabeledInput "Name" "user-real-name" validateName sends

      seen1 <- radioMultichoice
               "Have you seen 24, Season 5 Episode 1?"
               "seen1" [(True,"Yes"),(False,"No")] validateSeenEp1 sends

      seenN <- radioMultichoice
               "Have you seen any other episodes of 24?"
               "seenN" [(True,"Yes"),(False,"No")] validateSeenEpN sends

      vidVis <- bootstrapLabeledInput
                "Written word" "written-word" validateWritten sends
      vidHer <- bootstrapLabeledInput
                "Spoken word" "spoken-word" validateSpoken sends
      $(qDyn [| Survey <$> $(unqDyn [|name|]) <*> $(unqDyn [|seen1  |])
                                              <*> $(unqDyn [|seenN  |])
                                              <*> $(unqDyn [|vidVis |])
                                              <*> $(unqDyn [|vidHer |])
            |])

    elClass "div" "survey-video" $ mdo
      let vidUrlBase = "https://s3.amazonaws.com/gk24/avtest."
      v <- videoWidget [(vidUrlBase <> "mp4" , "video/mp4")
                       ,(vidUrlBase <> "ogg" , "video/ogg")
                       ,(vidUrlBase <> "webm", "video/webm")
                  ]
        (def & videoWidgetConfig_play .~ leftmost [pb, replay])
      replayAttrs <- holdDyn ("style" =: "display:none")
                     (mempty <$ v ^. videoWidget_ended)
      replay <- elDynAttr "div" replayAttrs $ bootstrapButton "repeat"
      el "p" $ text
        "To test your video and audio, please tell us what you see and hear."

    return surv
  return ()

  elClass "div" "error-text" $
    dynText =<< holdDyn "" (leftmost [lefts survs, "" <$ rights survs])
  sends <- button "Ok"

  let survs = tag (current surv) sends
  return $ rights survs

  where
    validateName "" = Left "Please enter your full name"
    validateName n  = Right n

    validateSeenEp1 (Just False) = Left
      "This test is only for subjects who saw 24 Season 5, Episode 1"
    validateSeenEp1 Nothing      = Left
      "Please indicate whether or not you saw Episode 1"
    validateSeenEp1 (Just True)  = Right True

    validateSeenEpN (Just True)  = Left
      "This test is for subjects who have only seen 24 Season 5 Episode 1"
    validateSeenEpN Nothing      = Left
      "Please indicate whether or not you saw any other episodes of 24"
    validateSeenEpN (Just False) = Right False

    validateVidLook   n
      | map toLower n == "brain" = Right "brain"
      | otherwise                = Left
        "Sorry, that isn't the work shown in the video"
    validateVidListen n
      | map toLower n == "mind" = Right "mind"
      | otherwise               = Left
        "Sorry, that isn't the word spoken in the video"
    validateWritten n
      | map toLower n == "hand" = Right "hand"
      | otherwise = Left
        "That is not the word written in the video"
    validateSpoken n
      | map toLower n == "hello" = Right "hello"
      | otherwise = Left
        "That is not the word spoken in the video"
    rights = fmapMaybe $ \case
      Right x -> Just x
      Left _  -> Nothing
    lefts = fmapMaybe $ \case
      Left x -> Just x
      Right _ -> Nothing



------------------------------------------------------------------------------
bootstrapLink =
     "rel" =: "stylesheet"
  <> "type" =: "text/css"
  <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"

------------------------------------------------------------------------------
main' :: IO ()
main' = mainWidget run

-- This is just a test of VideoWidget
main :: IO ()
main = mainWidget $ do
  play <- button "play"
  pause <- button "pause"
  reset <- button "reset"
  muted <- toggle False =<< button "Toggle mute"
  el "br" (return ())
  w <- videoWidget [("file:///home/greghale/Downloads/test4.mp4","video/mp4")]
       (def & videoWidgetConfig_play .~ play
            & videoWidgetConfig_pause .~ pause
            & videoWidgetConfig_setMuted .~ updated muted
            & videoWidgetConfig_setCurrentTime .~ (0 <$ reset))
  el "br" (return ())
  -- display =<< count (w ^. videoWidget_ended)
  text "test"
  -- display (w ^. videoWidget_currentTime)

  return ()


------------------------------------------------------------------------------
-- | Convenience
fin :: MonadWidget t m => m ()
fin = return ()


------------------------------------------------------------------------------
-- | Throwaway error-reporting widget
errorText :: MonadWidget t m => m () -> Event t Bool -> m ()
errorText msg setShowing = mdo
  showing <- holdDyn False $ leftmost [setShowing, False <$ closeClick]
  divAttr <- forDyn showing $ \s -> "class"   =: "error-text"
                                 <> "style" =: bool "display:none" "" s
  closeClick <- elDynAttr "div" divAttr $ do
    msg
    (e,_) <- elAttr' "span" ("class" =: "glyphicon glyphicon-remove") fin
    return $ domEvent Click e
  fin


------------------------------------------------------------------------------
-- | Mime type string for various video file extensions
mimeOf :: String -> String
mimeOf fn = case extension fn of
  "ogg" -> "video/ogg"
  "mp4" -> "video/mp4"
  "webm" -> "video/webm"
  where extension = reverse . takeWhile (/= '.') . reverse


------------------------------------------------------------------------------
hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing
