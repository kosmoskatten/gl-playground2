module Main where

import           Control.Monad             (when)
import           Data.Either               (isLeft)
import           Data.Maybe                (fromJust, isNothing)
import           Graphics.Rendering.OpenGL (ClearBuffer (..), Color4 (..),
                                            GLfloat, ShaderType (..), ($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          (OpenGLProfile (..),
                                            StickyKeysInputMode (..),
                                            WindowHint (..))
import qualified Graphics.UI.GLFW          as GLFW
import           RenderLoop                (simpleRenderLoop)
import           ShaderLoader              (loadShaders)
import           System.Exit               (exitFailure)

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

    result <- loadShaders [ (VertexShader, "triangle/triangle.vert")
                          , (FragmentShader, "triangle/triangle.frag")
                          ]

    when (isLeft result) $ do
        let Left err = result
        putStrLn err
        exitFailure

    GLFW.setStickyKeysInputMode window StickyKeysInputMode'Enabled

    GL.clearColor $= Color4 0 0 0.4 (0 :: GLfloat)

    simpleRenderLoop window $ \_ ->
        GL.clear [ColorBuffer]

    GLFW.terminate
