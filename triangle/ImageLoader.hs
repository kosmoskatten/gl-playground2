module ImageLoader
    ( loadImageRGB8
    , loadImageRGBA8
    , savePngImage
    ) where

import           Codec.Picture

loadImageRGB8 :: FilePath -> IO (Either String (Image PixelRGB8))
loadImageRGB8 file = (fmap convertRGB8) <$> readImage file

loadImageRGBA8 :: FilePath -> IO (Either String (Image PixelRGBA8))
loadImageRGBA8 file = (fmap convertRGBA8) <$> readImage file
