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
{-# LANGUAGE TupleSections             #-}

module Tagging.Experiments.HomeAlone.Widgets where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor
import qualified Data.List                  as L
import qualified Data.Map                   as Map
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy.Encoding    as TL
import           Data.Time
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson                 as A
import           Data.Default
import           GHC.Int
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dom.Time
import           Reflex.Dom.Xhr
------------------------------------------------------------------------------
import           Tagging.Response
import           Tagging.Stimulus
import           Tagging.User
import           Experiments.HomeAlonePersonAndDirection
------------------------------------------------------------------------------

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
    let clearEvents = const "" <$> domEvent Click e
    return $ _textInput_value tBox

  return ns


------------------------------------------------------------------------------
-- | Auxiliary funciton for choiceBankWidget, drawing one choice bank entry
choiceBankSingleChoice
  :: MonadWidget t m
  => CharacterName
  -> Dynamic t (Maybe (String,String,String))
  -> m (Event t CharacterName)
choiceBankSingleChoice n dynStatus = do
    divAttrs <- forDyn dynStatus $ \s ->
           "class" =: "choice-bank-choice"
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


-----------------------------------------------------------------------------
-- | Show movie clip
movieWidget :: MonadWidget t m
            => Event t (Assignment, StimulusSequence, StimSeqItem)
            -- ^ Location within stimulus sequence
            -> m ()
movieWidget pEvent = do

  let movieSrcs (_, StimulusSequence{..}, StimSeqItem{..}) =
        case ssiStimulus of
          A.Array fileNames -> ffor fileNames $ \(A.String fn) ->
               "src"  =: (T.unpack ssBaseUrl <> "/" <> T.unpack fn)
            <> "type" =: nameToMime (T.unpack fn)

  widgetHold (text "waiting")
                (ffor pEvent $ \p ->
                  elAttr "video" ("controls" =: "controls") $
                   forM_ (movieSrcs p) $ \attrs ->
                     elAttr "source" attrs (return ())
                )

  return ()


type StableProps t = Map.Map CharacterName (Dynamic t StableProperties)

-----------------------------------------------------------------------------
-- Options for updating stable properties of a character
-- Returns a Event t (), one () for each click on the update button
stablePropsWidget :: forall t m. MonadWidget t m
                  => [CharacterName] -- ^ List of characters
                  -> Event t (Maybe CharacterName)
                     -- ^ Currently selected character name
                  -> m (Event t ())
stablePropsWidget characterNames selName =

  elClass "div" "stable-props-div" $ mdo

  nameDyn <- holdDyn Nothing selName

  let bgroup :: (MonadWidget t m, Eq a, Show a)
             => String 
             -> Dynamic t [(a,String)]
             -> m (Dynamic t (Maybe a))
      bgroup lbl choices = el "tr" $ do
        el "td" $ text lbl
        wid <- el "td" $
          bootstrapButtonGroup choices
          WidgetConfig { _widgetConfig_setValue     = never
                       , _widgetConfig_attributes   = constDyn ("class" =: "btn-group btn-group-xs")
                       , _widgetConfig_initialValue = Nothing
                       }
        holdDyn Nothing (_hwidget_change wid)


  stableProps <- fmap Map.fromList $ forM characterNames $ \c -> do

    singleCharacterProps <- forDyn nameDyn $ \selName ->
      bool ("style" =: "display:none;") mempty (Just c == selName)

    elDynAttr "div" singleCharacterProps $
     el "table" $ do

      dynGend <- bgroup "Gender" (constDyn [(FemaleGender,"Female")
                                           ,(MaleGender,"Male")
                                           ,(OtherGender,"Other")])

      dynFeel <- bgroup "Type" (constDyn [(GoodGuy,"Goodguy")
                                           ,(BadGuy,"Badguy")])

      dynFam <- bgroup "Famous" (constDyn [(False,"No")
                                          ,(True,"True")])


      stableProps <- $(qDyn [| StableProperties c
                               ($(unqDyn [| dynGend |]))
                               ($(unqDyn [| dynFeel |]))
                               ($(unqDyn [| dynFam  |]))
                            |]) :: m (Dynamic t StableProperties)

      return (c,stableProps)

  buttonProps <- forDyn nameDyn $
                  maybe ("style" =: "display:none;") (const mempty)
  submitClicks <- elDynAttr "div" buttonProps $
                  button "Submit"

  xs <- forM (Map.elems stableProps) $ \dynMayProps ->
               combineDyn (\n props -> bool [] [props]
                                        (Just (_spCharacterName props) == n))
               nameDyn dynMayProps
  x <- mapDyn listToMaybe =<< mconcatDyn xs

  let okToRequest = fmapMaybe id (tagDyn x submitClicks)
  let propReqs    = ffor okToRequest $ \(sp :: StableProperties) ->
        XhrRequest "POST" "/api/response" $
        XhrRequestConfig ("Content-Type" =: "application/json") Nothing Nothing
          Nothing (Just . BSL.unpack $ A.encode
            (ResponsePayload (A.toJSON (Sporadic sp))))
  stableResponses <- performRequestAsync propReqs

  return (() <$ stableResponses)


-----------------------------------------------------------------------------
type ClipPropsMap t = Map.Map CharacterName
                     (Dynamic t (Maybe ClipProperties))


-----------------------------------------------------------------------------
clipPropsWidget :: forall t m. MonadWidget t m
                => [CharacterName] -- ^ List of characters
                -> Event  t (Maybe CharacterName)
                   -- ^ Updates to the singley selected character
                -> Event t ()
                   -- ^ External commands that reset the property listing
                -> m (ClipPropsMap t)
                   -- ^ Returning current set of properties
clipPropsWidget characterNames selName resetEvents = mdo

  nameDyn <- holdDyn Nothing selName

  let bgroup :: (MonadWidget t m, Eq a, Show a)
             => String
             -> Dynamic t [(a,String)]
             -> m (Dynamic t (Maybe a))
      bgroup lbl choices = el "tr" $ do
        el "td" $ text lbl
        wid <- el "td" $
          bootstrapButtonGroup choices
          WidgetConfig { _widgetConfig_setValue     = Nothing <$ resetEvents
                       , _widgetConfig_attributes   = constDyn ("class" =: "btn-group btn-group-xs")
                       , _widgetConfig_initialValue = Nothing
                       }
        holdDyn Nothing (_hwidget_change wid)

  chars <- fmap Map.fromList $ forM characterNames $ \c -> do
    singleCharacterProps <- forDyn nameDyn $ \selName ->
      bool ("style" =: "display:none;") mempty (Just c == selName)
    elDynAttr "div" singleCharacterProps $ el "table" $ do
     dynHeadDir <- bgroup "Head"
<<<<<<< HEAD
                   (constDyn [(HDLeft, "Left")   ,(HDFront, "Front")
                             ,(HDRight, "Right") ,(HDBack,  "Back")
                             ,(HDBody, "Body") ,(HDOffscreen, "Hidden")]
                   )

     -- dynHeadDir :: Dynamic t (Maybe HeadInfo) <- el "tr" $ do
     --   el "td" $ text "Head Direction"
     --   wid <- el "td" $ bootstrapButtonGroup
     --             (constDyn [(HDLeft, "Left")
     --                       ,(HDFront, "Front")
     --                       ,(HDRight, "Right")
     --                       ,(HDBack,  "Back")
     --                       ,(HDBody, "Body")
     --                       ,(HDOffscreen, "Offscreen")])
     --             def { _widgetConfig_setValue = Nothing <$ resetEvents
     --                 , _widgetConfig_attributes = "class" =: "btn-group-xs"
     --                 }
     --   holdDyn Nothing (_hwidget_change wid)

     dynTalking <- bgroup "Interacting"
                   (constDyn [(False,"No") ,(True,"Yes")])

     -- dynTalking <- el "tr" $ do
     --  el "td" $ text "Interacting"
     --  talkingDropdown <- el "td" $
     --    dropdown Nothing
     --      (constDyn $ Map.fromList [ (Nothing, "")
     --                               , (Just True,"Yes")
     --                               , (Just False, "No")]
     --      ) (DropdownConfig (Nothing <$ resetEvents) (constDyn mempty))

     --  holdDyn Nothing (_dropdown_change talkingDropdown)
=======
                   (constDyn [(HDLeft,  "Left")  ,(HDFront,     "Front")
                             ,(HDRight, "Right") ,(HDBack,      "Back")
                             ,(HDBody,  "Body")  ,(HDOffscreen, "Hidden")])
     dynInteracting <- bgroup "Interacting"
                   (constDyn [(InteractNone, "No")
                             ,(InteractPos,  "Positive")
                             ,(InteractNeg,  "Negative")
                             ,(InteractNeut, "Neutral")])
>>>>>>> unknowncharacters
     dynPain <- bgroup "In Pain"
                   (constDyn [(False, "No"), (True, "Yes")])
     dynMentalizing <- bgroup "Mentalizing"
                       (constDyn [(False, "No"), (True, "Yes")])
     clipProps <- $(qDyn [| ClipProperties c
                            <$>      $(unqDyn [| dynHeadDir     |])
                            <*>      $(unqDyn [| dynInteracting |])
                            <*> pure $(unqDyn [| dynPain        |])
                            <*> pure $(unqDyn [| dynMentalizing |])
                         |])
     return (c, clipProps)


  return (chars)


data SelectionWidget t = SelectionWidget
  { swAdditions :: Event t CharacterName
  , swDeletions :: Event t CharacterName
  , swSends     :: Event t ()
  }

-----------------------------------------------------------------------------
-- | View currently selected characters, detecting clicks
--   Provide a 'submit' button for submitting the per-clip properties
--   Returning event stream of tuples:
--   (Select character clicks, remove character clicks, send-button clicks)
selectionsWidget :: MonadWidget t m
                 => Dynamic t [CharacterName]
                    -- ^ Listing of selected characters' names
                 -> Dynamic t (Maybe CharacterName)
                 -> Dynamic t Bool
                    -- ^ Flag for whether send button should be enabled
                 -> m (SelectionWidget t, Dynamic t Int)
selectionsWidget selChars selChar okToSend =
 elClass "div" "selections-container" $ do

  -- This map just gets the character list into a map shape for compatability
  -- with listViewWithKey, putting characters from the multi-selection in
  -- the key positions, and flagging whether that character is the single
  -- selection in the value position
  charMap <- combineDyn (\chrs chr -> Map.fromList $
                                      map (\c -> (c, Just c == chr)) chrs)
             selChars selChar
  clickMap <- listViewWithKey charMap $ \n dynMatch -> do
    selChoiceAttrs <- forDyn dynMatch $ \b ->
         "class" =: bool "selection-choice" "selection-choice selected" b
    (e,btn) <- elDynAttr "div" selChoiceAttrs $ mdo
      (e,_) <- elAttr' "img" (("src" :: String) =: nameToFile (T.unpack n)) $ return ()
      btnAttr <- holdDyn ("class" =: "fa fa-times") $
                   leftmost [ domEvent Mouseenter btn $>
                                  ("class" =: "fa fa-times-circle")
                            , domEvent Mouseleave btn $>
                                  ("class" =: "fa fa-times")
                            ]
      (btn,_) <- elDynAttr' "span" btnAttr $ return ()
      elClass "div" "selection-choice-text" $ text (T.unpack n)
      return (e,btn)
    return $ leftmost [ (n,True)  <$ domEvent Click e
                      , (n,False) <$ domEvent Click btn]
  let clks = oneFromMap clickMap

  sendAttrs <- forDyn okToSend $ \b ->
       "type" =: "button"
       <> bool ("disabled" =: "disabled") mempty b

  sendClicks <- el "div" $ do
    (e,_) <- elDynAttr' "button" sendAttrs $ text "Send"
    return (domEvent Click e)

  nOtherChars <- elAttr "div" ("class" =: "others-and-send") $ do
    text "Others"
    dropdown 0
      (constDyn $ Map.fromList [((0::Int),"0"),(1,"1"),(2,"2"),(3,"3+")]) 
      (DropdownConfig (0 <$ sendClicks) 
                      (constDyn $ "class" =: "n-characters"))

  return $ (SelectionWidget (fmap fst (ffilter snd clks))
                            (fmap fst (ffilter (not . snd) clks))
                            sendClicks,
            value nOtherChars)


-----------------------------------------------------------------------------
-- Utility that renders a string according to a search query that may hit
fragmentWith :: String -> String -> (String, String, String)
fragmentWith source query =

  let qSource = T.pack source
      qText   = (T.toLower . T.pack) query
  in  if T.null qText
      then (source, "", "")
      else let breakPoint = T.length . fst . T.breakOn qText . T.toLower
                            $ qSource
               (p0,pTemp) = T.splitAt breakPoint qSource
               (p1,p2)    = T.splitAt (T.length qText) pTemp
           in  (T.unpack p0, T.unpack p1, T.unpack p2)


-------------------------------------------------------------------------------
-- Utility that renders a string according to a search query that may hit
searchText :: MonadWidget t m
           => Dynamic t String -- ^ String to search for
           -> String           -- ^ String to search in
           -> m (Dynamic t (m ()))
searchText query source = do

  let qSource = T.pack source
  qText <- mapDyn (T.toLower . T.pack) query
  forDyn qText $ \q ->
      if not (T.null q) && T.toLower q `T.isInfixOf` T.toLower qSource
      then let breakPoint = T.length . fst . T.breakOn q . T.toLower $ qSource
               (p0,pTemp) = T.splitAt breakPoint qSource
               (p1,p2)    = T.splitAt (T.length q) pTemp
           in el "h2" $ do
                text (T.unpack p0)
                elClass "span" "text-found" $ text (T.unpack p1)
                text (T.unpack p2)

      else el "h2" $ text source


-----------------------------------------------------------------------------
-- Listing of names and paths to their pics (hard-coded for now. TODO serve)
choices :: [CharacterName]
choices= ["Kevin McC" ,"Tracy McC" ,"Sondra McC" ,"Rod McC" ,"Rob McC"
         ,"Buzz McC" ,"Peter McC"
         ,"Mrs. Stone" ,"Mr. Hector" ,"Mr. Duncan"
         ,"Megan McC" ,"Marv Merch" ,"Linnie McC" ,"Leslie McC" ,"Kate McC"
         ,"Jeff McC" ,"Harry Lyme" ,"Fuller McC" ,"Frank McC" ,"Cedric"
         ,"Brooke McC" ,"Bird Lady", "Other"
         ]

choicesMap :: Map.Map String String
choicesMap = Map.fromList $
             map (\n -> (T.unpack n, nameToFile (T.unpack n))) choices

nameToFile :: String -> String
nameToFile = ("http://web.mit.edu/greghale/Public/hapics/" <>)
             . (<> ".png")
             . filter (`notElem` ("() ." :: String))

nameToMime :: String -> String
nameToMime fn = case extension fn of
  "ogg" -> "video/ogg"
  "mp4" -> "video/mp4"
  where extension = reverse . takeWhile (/= '.') . reverse


------------------------------------------------------------------------------
-- | Utility function for pulling one (arbitrary) event out of a Map of events
oneFromMap :: Reflex t => Event t (Map.Map k a) -> Event t a
oneFromMap = fmapMaybe (listToMaybe . Map.elems)
