module ShaderLoader
    ( loadShaders
    ) where

import           Control.Exception         (SomeException, try)
import qualified Data.ByteString.Char8     as BS
import           Graphics.Rendering.OpenGL (Program, Shader, ShaderType (..),
                                            get, ($=))
import qualified Graphics.Rendering.OpenGL as GL

loadShaders :: [(ShaderType, FilePath)] -> IO (Either String Program)
loadShaders specs = do
    result <- sequence <$> (mapM compile specs)
    either (return . Left) link result

compile :: (ShaderType, FilePath) -> IO (Either String Shader)
compile (shaderType, file) = do
    bs <- tryReadFile file
    either (return . Left) tryCompile bs
    where
        tryCompile :: BS.ByteString -> IO (Either String Shader)
        tryCompile code = do
            shader <- GL.createShader shaderType
            GL.shaderSourceBS shader $= code
            GL.compileShader shader
            status <- get $ GL.compileStatus shader
            if status
                then return $ Right shader
                else do
                    info <- get $ GL.shaderInfoLog shader
                    return $ Left info

link :: [Shader] -> IO (Either String Program)
link shaders = do
    program <- GL.createProgram
    mapM_ (GL.attachShader program) shaders
    GL.linkProgram program
    status <- get $ GL.linkStatus program
    if status
        then return $ Right program
        else do
            info <- get $ GL.programInfoLog program
            return $ Left info

tryReadFile :: FilePath -> IO (Either String BS.ByteString)
tryReadFile file = do
    res <- try' $ BS.readFile file
    return $ either (Left . show) Right res
    where
        try' :: IO a -> IO (Either SomeException a)
        try' = try
