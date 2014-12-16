{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, JavaScriptFFI, CPP #-}

module JavaScript.Canvas ( Context
                         , Canvas
                         , Image
                         , TextAlign(..)
                         , TextBaseline(..)
                         , LineCap(..)
                         , LineJoin(..)
                         , getContext
                         , save
                         , restore
                         , scale
                         , rotate
                         , translate
                         , transform
                         , setTransform
                         , fill
                         , fillRule
                         , stroke
                         , beginPath
                         , closePath
                         , clip
                         , moveTo
                         , lineTo
                         , quadraticCurveTo
                         , bezierCurveTo
                         , arc
                         , arcTo
                         , rect
                         , isPointInPath
                         , fillStyle
                         , strokeStyle
                         , globalAlpha
                         , lineJoin
                         , lineCap
                         , lineWidth
                         , setLineDash
                         , lineDashOffset
                         , miterLimit
                         , fillText
                         , strokeText
                         , font
                         , measureText
                         , textAlign
                         , textBaseline
                         , fillRect
                         , strokeRect
                         , clearRect
                         , drawImage
                         ) where

import Prelude hiding (Left, Right)

import Control.Applicative
import Control.Monad
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

data Canvas_
data Context_
type Canvas = JSRef Canvas_
type Context = JSRef Context_

data Image_
type Image = JSRef Image_

data TextAlign = Start
               | End
               | Left
               | Right
               | Center
             deriving (Eq, Show, Enum)

data TextBaseline = Top 
                  | Hanging 
                  | Middle
                  | Alphabetic
                  | Ideographic
                  | Bottom
                deriving (Eq, Show, Enum)

data LineJoin = LineJoinBevel
              | LineJoinRound
              | LineJoinMiter
            deriving (Eq, Show, Enum)

data LineCap = LineCapButt
             | LineCapRound
             | LineCapSquare deriving (Eq, Show, Enum)

getContext :: Canvas -> IO Context
getContext = js_getContext

save :: Context -> IO ()
save = js_save
{-# INLINE save #-}

restore :: Context -> IO ()
restore = js_restore
{-# INLINE restore #-}

transform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
transform = js_transform
{-# INLINE transform #-}

setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
setTransform = js_setTransform
{-# INLINE setTransform #-}

scale :: Double -> Double -> Context -> IO ()
scale = js_scale
{-# INLINE scale #-}

translate :: Double -> Double -> Context -> IO ()
translate = js_translate
{-# INLINE translate #-}

rotate :: Double -> Context -> IO ()
rotate = js_rotate
{-# INLINE rotate #-}

fill :: Context -> IO ()
fill = js_fill
{-# INLINE fill #-}

fillRule :: Text -> Context -> IO ()
fillRule rule = js_fill_rule (toJSString rule)
{-# INLINE fillRule #-}

stroke :: Context -> IO ()
stroke = js_stroke
{-# INLINE stroke #-}

beginPath :: Context -> IO ()
beginPath = js_beginPath
{-# INLINE beginPath #-}

closePath :: Context -> IO ()
closePath = js_closePath
{-# INLINE closePath #-}

clip :: Context -> IO ()
clip = js_clip
{-# INLINE clip #-}

moveTo :: Double -> Double -> Context -> IO ()
moveTo = js_moveTo
{-# INLINE moveTo #-}

lineTo :: Double -> Double -> Context -> IO ()
lineTo = js_lineTo
{-# INLINE lineTo #-}

quadraticCurveTo :: Double -> Double -> Double -> Double -> Context -> IO ()
quadraticCurveTo = js_quadraticCurveTo
{-# INLINE quadraticCurveTo #-}

bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
bezierCurveTo = js_bezierCurveTo
{-# INLINE bezierCurveTo #-}

arc :: Double -> Double -> Double -> Double -> Double -> Bool -> Context -> IO ()
arc a b c d e bl ctx = js_arc a b c d e (toJSBool bl) ctx
{-# INLINE arc #-}

arcTo :: Double -> Double -> Double -> Double -> Double -> Context -> IO ()
arcTo = js_arcTo
{-# INLINE arcTo #-}

rect :: Double -> Double -> Double -> Double -> Context -> IO ()
rect = js_rect
{-# INLINE rect #-}

isPointInPath :: Double -> Double -> Context -> IO ()
isPointInPath = js_isPointInPath
{-# INLINE isPointInPath #-}

fillStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
fillStyle = js_fillStyle
{-# INLINE fillStyle #-}

strokeStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
strokeStyle = js_strokeStyle
{-# INLINE strokeStyle #-}

globalAlpha :: Double -> Context -> IO ()
globalAlpha = js_globalAlpha
{-# INLINE globalAlpha #-}

lineJoin :: LineJoin -> Context -> IO ()
lineJoin lj ctx = js_lineJoin (fromEnum lj) ctx
{-# INLINE lineJoin #-}

lineCap :: LineCap -> Context -> IO ()
lineCap lc ctx = js_lineCap (fromEnum lc) ctx
{-# INLINE lineCap #-}

miterLimit :: Double -> Context -> IO ()
miterLimit = js_miterLimit
{-# INLINE miterLimit #-}

setLineDash :: (Num a, ToJSRef a) => [a] -> Context -> IO ()
setLineDash lst ctx = do
     arr <- toArray =<< mapM (return . castRef <=< toJSRef) lst
     js_setLineDash arr ctx
{-# INLINE setLineDash #-}

lineDashOffset :: Double -> Context -> IO ()
lineDashOffset = js_lineDashOffset
{-# INLINE lineDashOffset #-}

textAlign :: TextAlign -> Context -> IO ()
textAlign align ctx = case align of
     Start  -> js_textAlign (toJSString "start") ctx
     End    -> js_textAlign (toJSString "end") ctx
     Left   -> js_textAlign (toJSString "left") ctx
     Right  -> js_textAlign (toJSString "right") ctx
     Center -> js_textAlign (toJSString "center") ctx
{-# INLINE textAlign #-}

textBaseline :: TextBaseline -> Context -> IO ()
textBaseline baseline ctx = case baseline of 
     Top         -> js_textBaseline (toJSString "top") ctx
     Hanging     -> js_textBaseline (toJSString "hanging") ctx
     Middle      -> js_textBaseline (toJSString "middle") ctx
     Alphabetic  -> js_textBaseline (toJSString "alphabetic") ctx
     Ideographic -> js_textBaseline (toJSString "ideographic") ctx
     Bottom      -> js_textBaseline (toJSString "bottom") ctx
{-# INLINE textBaseline #-}

lineWidth :: Double -> Context -> IO ()
lineWidth = js_lineWidth
{-# INLINE lineWidth #-}

fillText :: Text -> Double -> Double -> Context -> IO ()
fillText t x y ctx = js_fillText (toJSString t) x y ctx
{-# INLINE fillText #-}

strokeText :: Text -> Double -> Double -> Context -> IO ()
strokeText t x y ctx = js_strokeText (toJSString t) x y ctx
{-# INLINE strokeText #-}

font :: Text -> Context -> IO ()
font f ctx = js_font (toJSString f) ctx
{-# INLINE font #-}

measureText :: Text -> Context -> IO Double
measureText t ctx = js_measureText (toJSString t) ctx
                    >>= getProp "width"
                    >>= liftM fromJust . fromJSRef
{-# INLINE measureText #-}

fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()
fillRect = js_fillRect
{-# INLINE fillRect #-}

clearRect :: Double -> Double -> Double -> Double -> Context -> IO ()
clearRect = js_clearRect
{-# INLINE clearRect #-}

strokeRect :: Double -> Double -> Double -> Double -> Context -> IO ()
strokeRect = js_strokeRect
{-# INLINE strokeRect #-}

drawImage :: Image -> Int -> Int -> Int -> Int -> Context -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$1.getContext('2d')" js_getContext  ::                     Canvas  -> IO Context

foreign import javascript unsafe "$1.save()"           js_save        ::                     Context -> IO ()
foreign import javascript unsafe "$1.restore()"        js_restore     ::                     Context -> IO ()
foreign import javascript unsafe "$7.transform($1,$2,$3,$4,$5,$6)"
                 js_transform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$7.setTransform($1,$2,$3,$4,$5,$6)"
              js_setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$3.scale($1,$2)"     js_scale       :: Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$3.translate($1,$2)" js_translate   :: Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$2.rotate($1)"       js_rotate      :: Double           -> Context -> IO ()
foreign import javascript unsafe "$1.fill()"           js_fill        ::                     Context -> IO ()
foreign import javascript unsafe "$2.fill($1)"         js_fill_rule   ::         JSString -> Context -> IO ()
foreign import javascript unsafe "$1.stroke()"         js_stroke      ::                     Context -> IO ()
foreign import javascript unsafe "$1.beginPath()"      js_beginPath   ::                     Context -> IO ()
foreign import javascript unsafe "$1.closePath()"      js_closePath   ::                     Context -> IO ()
foreign import javascript unsafe "$1.clip()"           js_clip        ::                     Context -> IO ()
foreign import javascript unsafe "$3.moveTo($1,$2)"    js_moveTo      :: Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$3.lineTo($1,$2)"    js_lineTo      :: Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$5.quadraticCurveTo($1,$2,$3,$4)"
                              js_quadraticCurveTo :: Double -> Double -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$7.bezierCurveTo($1,$2,$3,$4,$5,$6)"
             js_bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$7.arc($1,$2,$3,$4,$5,$6)"
                       js_arc :: Double -> Double -> Double -> Double -> Double -> JSBool -> Context -> IO ()
foreign import javascript unsafe "$6.arcTo($1,$2,$3,$4,$5)"
                               js_arcTo :: Double -> Double -> Double -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$5.rect($1,$2,$3,$4)"
                                          js_rect :: Double -> Double -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$3.isPointInPath($1,$2)"
                                                     js_isPointInPath :: Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$5.fillStyle = 'rgba(' + $1 + ',' + $2 + ',' + $3 + ',' + $4 + ')'"
                                              js_fillStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
foreign import javascript unsafe "$5.strokeStyle = 'rgba(' + $1 + ',' + $2 + ',' + $3 + ',' + $4 + ')'"
                                            js_strokeStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
foreign import javascript unsafe "$2.globalAlpha = $1" js_globalAlpha :: Double           -> Context -> IO ()
foreign import javascript unsafe "$2.lineJoin = ($1===0)?'bevel':(($1===1)?'round':'miter')"
                                                          js_lineJoin :: Int              -> Context -> IO ()
foreign import javascript unsafe "$2.lineCap = ($1===0)?'butt':(($1===1)?'round':'square')"
                                                           js_lineCap :: Int              -> Context -> IO ()
foreign import javascript unsafe "$2.miterLimit = $1"             js_miterLimit :: Double -> Context -> IO ()
foreign import javascript unsafe "h$ghcjs_setLineDash($1,$2)"  js_setLineDash :: JSArray JSNumber -> Context -> IO ()
foreign import javascript unsafe "h$ghcjs_lineDashOffset($1,$2)"     js_lineDashOffset :: Double -> Context -> IO ()
foreign import javascript unsafe "$2.font = $1"                       js_font :: JSString -> Context -> IO ()
foreign import javascript unsafe "$2.textAlign = $1"             js_textAlign :: JSString -> Context -> IO ()
foreign import javascript unsafe "$2.textBaseline = $1"       js_textBaseline :: JSString -> Context -> IO ()
foreign import javascript unsafe "$2.lineWidth = $1"     js_lineWidth :: Double           -> Context -> IO ()
foreign import javascript unsafe "$4.fillText($1,$2,$3)"
                                              js_fillText :: JSString -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$4.strokeText($1,$2,$3)"
                                            js_strokeText :: JSString -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$2.measureText($1)"
                                            js_measureText :: JSString                    -> Context -> IO (JSRef a)                                                            
foreign import javascript unsafe "$5.fillRect($1,$2,$3,$4)"
                                      js_fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$5.clearRect($1,$2,$3,$4)"
                                     js_clearRect :: Double -> Double -> Double -> Double -> Context -> IO ()
foreign import javascript unsafe "$5.strokeRect($1,$2,$3,$4)"
                                    js_strokeRect :: Double -> Double -> Double -> Double -> Context -> IO ()

foreign import javascript unsafe "$6.drawImage($1,$2,$3,$4,$5)"
                                    js_drawImage :: Image -> Int -> Int -> Int -> Int -> Context -> IO () 

#else

#include "nonGhcjsStubs.txt"

#endif
