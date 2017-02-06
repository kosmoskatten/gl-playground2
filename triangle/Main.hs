module Main where

import           Control.Monad    (when)
import           Data.Maybe       (fromJust, isNothing)
import qualified Graphics.GL      as GL
import           Graphics.UI.GLFW (OpenGLProfile (..), StickyKeysInputMode (..),
                                   WindowHint (..))
import qualified Graphics.UI.GLFW as GLFW
import           RenderLoop       (simpleRenderLoop)
import           System.Exit      (exitFailure)

main :: IO ()
main = do
    initSuccess <- GLFW.init
    when (not initSuccess) $ do
        putStrLn "GLFW initialization failed"
        exitFailure

    GLFW.windowHint $ WindowHint'Samples 4
    GLFW.windowHint $ WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core

    mWindow <- GLFW.createWindow 1024 728  "OpenGL" Nothing Nothing
    when (isNothing mWindow) $ do
        putStrLn "Failed to open GLFW window"
        GLFW.terminate
        exitFailure

    let window = fromJust mWindow
    GLFW.makeContextCurrent (Just window)

    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    GL.glClearColor 0 0 0.4 0

    simpleRenderLoop window $ \_ ->
        GL.glClear GL.GL_COLOR_BUFFER_BIT

    GLFW.terminate
