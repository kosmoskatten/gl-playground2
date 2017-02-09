module Main where

import           Control.Monad             (when)
import           Data.Either               (isLeft)
import           Data.Maybe                (fromJust, isNothing)
import           Foreign
import           Graphics.Rendering.OpenGL (AttribLocation (..),
                                            BufferTarget (..), BufferUsage (..),
                                            Capability (..), ClearBuffer (..),
                                            Color4 (..), DataType (..), GLfloat,
                                            IntegerHandling (..),
                                            PrimitiveMode (..), ShaderType (..),
                                            Vertex3 (..),
                                            VertexArrayDescriptor (..),
                                            VertexArrayObject, ($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLFW          (OpenGLProfile (..),
                                            StickyKeysInputMode (..),
                                            WindowHint (..))
import qualified Graphics.UI.GLFW          as GLFW
import           RenderLoop                (simpleRenderLoop)
import           ShaderLoader              (loadShaders)
import           System.Exit               (exitFailure)

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr

vertices :: [Vertex3 GLfloat]
vertices =
    [ Vertex3   0.0    0.5  0.0
    , Vertex3 (-0.5) (-0.5) 0.0
    , Vertex3   0.5  (-0.5) 0.0
    ]

initVAO :: [Vertex3 GLfloat] -> IO VertexArrayObject
initVAO verts = do
    vao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just vao

    vbo <- GL.genObjectName
    GL.bindBuffer ArrayBuffer $= Just vbo
    withArray verts $ \ptr -> do
        let size = fromIntegral (length verts * sizeOf (head verts))
        GL.bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    let position = AttribLocation 0
    GL.vertexAttribArray position $= Enabled
    GL.vertexAttribPointer position $=
            (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))

    GL.bindVertexArrayObject $= Nothing
    return vao

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

    eProgram <- loadShaders [ (VertexShader, "triangle/triangle.vert")
                            , (FragmentShader, "triangle/triangle.frag")]
    when (isLeft eProgram) $ do
        putStrLn "Cannot load shaders"
        GLFW.terminate
        exitFailure

    let Right program = eProgram

    vao <- initVAO vertices

    GL.clearColor $= Color4 0 0 0.4 (0 :: GLfloat)

    simpleRenderLoop window $ \_ -> do
        GL.clear [ColorBuffer]

        GL.currentProgram $= Just program
        GL.bindVertexArrayObject $= Just vao
        GL.drawArrays Triangles 0 3


    GLFW.terminate
