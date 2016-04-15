{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- Specification:
-- https://www.w3.org/TR/html5/embedded-content-0.html#htmlmediaelement
-- https://www.w3.org/TR/html5/embedded-content-0.html#mediaevents

module MediaElement where

import Control.Lens
import Data.Default
import Data.Foldable
import Data.Traversable
import Data.Map
import Data.Monoid
import GHCJS.DOM.EventM
import qualified GHCJS.DOM.HTMLMediaElement as Media
import Reflex
import Reflex.Dom

------------------------------------------------------------------------------
-- | Try to replicate as much of the functionality at
--   https://www.w3.org/2010/05/video/mediaevents.html as possible
data VideoWidgetConfig t = VideoWidgetConfig
  { _videoWidgetConfig_attributes :: Dynamic t (Map String String)
  , _videoWidgetConfig_sourceAttributes :: (String,String) -> Dynamic t (Map String String)
  , _videoWidgetConfig_load :: Event t ()
  , _videoWidgetConfig_play :: Event t ()
  , _videoWidgetConfig_pause :: Event t ()
  , _videoWidgetConfig_setCurrentTime :: Event t Double
  , _videoWidgetConfig_setPlaybackRate :: Event t Double
  , _videoWidgetConfig_setVolume :: Event t Double
  , _videoWidgetConfig_setMuted :: Event t Bool
  }

makeLenses ''VideoWidgetConfig

instance Reflex t => Default (VideoWidgetConfig t) where
  def = VideoWidgetConfig defAttr f never never never never never never never
    where defAttr = constDyn mempty
          f (srcUrl, srcMime) = constDyn ("src" =: srcUrl <> "type" =: srcMime)

data VideoWidget t = VideoWidget
  { _videoWidget_videoEl :: El t
  , _videoWidget_sourceEls :: [El t]
  , _videoWidget_currentTime :: Dynamic t Double
  , _videoWidget_playbackRate :: Dynamic t Double
  , _videoWidget_volume :: Dynamic t Double
  , _videoWidget_muted :: Dynamic t Bool
  -- , _videoWidget_loadstart :: Event t () -- TODO: This seems not to come from the MediaElement
  , _videoWidget_canplaythrough :: Event t ()
  , _videoWidget_ended :: Event t ()
  }

makeLenses ''VideoWidget

------------------------------------------------------------------------------
-- | Create a video playing widget with access to video controls, events, and
--   state
videoWidget :: MonadWidget t m
            => [(String, String)] -- ^ Pairs of source url and mime-type
            -> VideoWidgetConfig t
            -> m (VideoWidget t)
videoWidget srcs cfg = do
  (vidEl, vidSrcs) <- elDynAttr' "video" (_videoWidgetConfig_attributes cfg) $
    forM srcs $ \src ->
      fmap fst $ elDynAttr' "source"
                            (_videoWidgetConfig_sourceAttributes cfg src)
                            (return ())

  let e = Media.castToHTMLMediaElement (_el_element vidEl)

  -- Property getters
  -- TODO: What are the correct initial values?

#ifdef ghcjs_HOST_OS
  curTime <- holdDyn 0 =<<
             wrapDomEvent e (`on` Media.timeUpdate) (Media.getCurrentTime e)
  curTime <- undefined
  vidEnded <- wrapDomEvent e (`on` Media.ended) (return ())
  vidMuted <- holdDyn False =<<
              wrapDomEvent e (`on` Media.volumeChange) (Media.getMuted e)
  vidVolume <- holdDyn 1 =<<
               wrapDomEvent e (`on` Media.volumeChange) (Media.getVolume e)
  canPlThr  <- wrapDomEvent e (`on` Media.canPlayThrough) (return ())
#endif

  -- loadStart <- wrapDomEvent e (`on` Media.loadStart) (return ())
  -- Pushers & Setters
  performEvent_ $ Media.load e <$ _videoWidgetConfig_load cfg
  performEvent_ $ Media.play e <$ _videoWidgetConfig_play cfg
  performEvent_ $ Media.pause e <$ _videoWidgetConfig_pause cfg
  performEvent_ $ Media.setCurrentTime e
                  <$> _videoWidgetConfig_setCurrentTime cfg
  performEvent_ $ Media.setPlaybackRate e
                  <$> _videoWidgetConfig_setPlaybackRate cfg
  performEvent_ $ Media.setVolume e
                  <$> _videoWidgetConfig_setVolume cfg
  performEvent_ $ Media.setMuted e
                  <$> _videoWidgetConfig_setMuted cfg

#ifdef ghcjs_HOST_OS
  return $ VideoWidget vidEl vidSrcs curTime undefined
                       vidVolume vidMuted canPlThr vidEnded
#else
  return $ VideoWidget
    { _videoWidget_videoEl = vidEl
    , _videoWidget_sourceEls = vidSrcs
    , _videoWidget_currentTime = error "_videoWidget_currentTime: can only be used with GHCJS"
    , _videoWidget_playbackRate = error "_videoWidget_playbackRate: can only be used with GHCJS"
    , _videoWidget_volume = error "_videoWidget_volume: can only be used with GHCJS"
    , _videoWidget_muted = error "_videoWidget_error: can only be used with GHCJS"
    , _videoWidget_canplaythrough = error "_videoWidget_canplaythrough: can only be used with GHCJS"
    , _videoWidget_ended = error "_videoWidget_ended: can only be used with GHCJS"
    }
#endif

