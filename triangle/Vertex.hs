module Vertex where

import           Foreign
import           Graphics.Rendering.OpenGL (TexCoord2 (..), Vertex3 (..))

data VertexTexture a = VertexTexture
    { position :: !(Vertex3 a)
    , texCoord :: !(TexCoord2 a)
    } deriving (Show)

instance Storable a => Storable (VertexTexture a) where
    sizeOf v = (sizeOf $ position v) + (sizeOf $ texCoord v)
    alignment v =
        let Vertex3 x _ _ = position v
        in sizeOf x
    peek = error "Reading from stored memory is not implemented"
    poke ptr v = do
        let p1 = castPtr ptr
            p2 = castPtr $ plusPtr ptr (sizeOf $ position v)
        poke p1 $ position v
        poke p2 $ texCoord v
