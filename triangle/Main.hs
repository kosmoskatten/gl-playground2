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


    GL.clearColor $= Color4 0 0 0.4 (0 :: GLfloat)

    vao <- GL.genObjectName
    GL.bindVertexArrayObject $= Just vao

    eProgram <- loadShaders [(VertexShader, "triangle/triangle.vert")
                            , (FragmentShader, "triangle/triangle.frag")]
    when (isLeft eProgram) $ do
        putStrLn "Cannot load shaders"
        GLFW.terminate
        exitFailure

    let Right program = eProgram

    let triangle = [ Vertex3 (-1) (-1) 0
                   , Vertex3 1 (-1) 0
                   , Vertex3 0 1 0
                   ] :: [Vertex3 GLfloat]
        numVertices = length triangle
        vertexSize  = sizeOf (head triangle)

    vbo <- GL.genObjectName
    GL.bindBuffer ArrayBuffer $= Just vbo
    withArray triangle $ \ptr -> do
        let size = fromIntegral (numVertices * vertexSize)
        GL.bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    simpleRenderLoop window $ \_ -> do
        GL.clear [ColorBuffer]

        GL.currentProgram $= Just program

        let position = AttribLocation 0
            firstIndex = 0
        GL.vertexAttribArray position $= Enabled
        GL.vertexAttribPointer position $=
            (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset (firstIndex * vertexSize)))
        GL.drawArrays Triangles 0 3
        GL.vertexAttribArray position $= Disabled

    GLFW.terminate
