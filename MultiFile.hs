{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module MultiFile where

import ClassyPrelude.Conduit
import Data.Functor.Identity (runIdentity)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as S
import Control.Monad (unless)
import Data.Conduit.List (sinkNull)
import qualified Data.Conduit.Text as CT
import Filesystem.Path.CurrentOS (encode, directory, fromText)
import Filesystem (createTree)
import Control.Monad.Trans.Resource (runExceptionT)
import Data.Conduit.Filesystem (sinkFile)
import Data.Text.Encoding (encodeUtf8)

unpackMultiFile
    :: MonadResource m
    => FilePath -- ^ output folder
    -> (Text -> Text) -- ^ fix each input line, good for variables
    -> Sink ByteString m ()
unpackMultiFile root fixLine =
    CT.decode CT.utf8 =$ CT.lines =$ map fixLine =$ start
  where
    start =
        await >>= maybe (return ()) go
      where
        go t =
            case getFileName t of
                Nothing -> error $ "Invalid input: " ++ show t
                Just (fp', isBinary) -> do
                    let fp = root </> fromText fp'
                    liftIO $ createTree $ directory fp
                    let src
                            | isBinary  = binaryLoop
                            | otherwise = textLoop
                    src =$ sinkFile fp
                    start

    binaryLoop = do
        await >>= maybe (error "binaryLoop needs 1 line") go
      where
        go = yield . B64.decodeLenient . encodeUtf8
    textLoop =
        await >>= maybe (return ()) go
      where
        go t =
            case getFileName t of
                Just{} -> leftover t
                Nothing -> do
                    yield $ encodeUtf8 t
                    yield "\n"
                    textLoop

    getFileName t =
        case words t of
            ["{-#", "START_FILE", fn, "#-}"] -> Just (fn, False)
            ["{-#", "START_FILE", "BASE64", fn, "#-}"] -> Just (fn, True)
            _ -> Nothing

createMultiFile
    :: MonadIO m
    => FilePath -- ^ folder containing the files
    -> Conduit FilePath m ByteString -- ^ FilePath is relative to containing folder
createMultiFile root = do
    awaitForever handleFile
  where
    handleFile fp' = do
        bs <- readFile fp
        case runIdentity $ runExceptionT $ yield bs $$ CT.decode CT.utf8 =$ sinkNull of
            Left{} -> do
                yield "{-# START_FILE BASE64 "
                yield $ encode fp'
                yield " #-}\n"
                yield $ B64.encode bs
                yield "\n"
            Right{} -> do
                yield "{-# START_FILE "
                yield $ encode fp'
                yield " #-}\n"
                yield bs
                unless ("\n" `S.isSuffixOf` bs) $ yield "\n"
      where
        fp = root </> fp'
