{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson.Types
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml (encode)
import SimpleCmdArgs

import Fedora.PDC
import Paths_pdc (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Query PDC web API with YAML output"
    "This tool queries various PDC API service endpoints outputting YAML" $
    subcommands
    [ Subcommand "branches" "Component branches (params: active (bool), critical_path (bool), global_component, name, type, ordering, fields, exclude_fields)" $
      paramsCmd pdcComponentBranches <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    ]
  where
    jsonOpt = switchWith 'j' "json" "Output json instead of yaml"

    keysOpt = switchWith 'k' "keys" "List keys of object"

    valuesOpt = optional (splitOn "." <$> strOptionWith 'v' "value" "KEY[.KEY..]" "Key value to show")

    argCmd :: (String -> IO Object) -> Bool -> Bool -> Maybe [String] -> String -> IO ()
    argCmd cmd json listkeys mkeys arg = do
      obj <- cmd arg
      if listkeys
      then mapM_ putKeys $ filter (not . null) $ getKeys (concat mkeys) (Object obj)
      else putKeysVal json (concat mkeys) (Object obj)

    argCmdMaybe :: (String -> IO (Maybe Object)) -> Bool -> Bool -> Maybe [String] -> String -> IO ()
    argCmdMaybe cmd json listkeys mkeys arg = do
      mobj <- cmd arg
      case mobj of
        Nothing -> error "Query failed"
        Just obj ->
          if listkeys
          then mapM_ putKeys $ filter (not . null) $ getKeys (concat mkeys) (Object obj)
          else putKeysVal json (concat mkeys) (Object obj)

--    paramsCmd :: (Query -> IO [Object]) -> Bool -> Bool -> Maybe [String] -> String -> IO ()
    paramsCmd cmd json listkeys mkeys args = do
      let params = readQuery args
      objs <- cmd params
      if listkeys then
        mapM_ putKeys $ (filter (not . null) . nub) $ concatMap (getKeys (concat mkeys) . Object) objs
        else
        mapM_ (putKeysVal json (concat mkeys) . Object) objs
      where
        readQuery [] = []
        readQuery (param:rest) =
          case splitOn "=" param of
            [k,_] | null k -> error $ "Bad key: " ++ param
            [_,v] | null v -> error $ "Missing value: " ++ param
            [k,v] -> makeItem k v : readQuery rest
            _ -> error $ "Bad parameter: " ++ param

    putPretty json = if json
                     then BL.putStrLn . encodePretty
                     else B.putStrLn . encode

    putKeysVal :: Bool -> [String] -> Value -> IO ()
    putKeysVal json [] val = putPretty json val
    putKeysVal json (k:ks) val =
      case val of
        Object obj ->
          case parseMaybe (.: T.pack k) obj of
            Nothing -> return ()
            Just v -> if null ks then
              case v of
                Array arr -> mapM_ (putKeysVal json []) arr
                _ -> putPretty json v
              else putKeysVal json ks v
        Array arr -> mapM_ (putKeysVal json (k:ks)) arr
        _ -> putPretty json val

    putKeys = T.putStrLn . T.intercalate ", "

    getKeys :: [String] -> Value -> [[T.Text]]
    getKeys [] val =
      case val of
        Object obj -> [H.keys obj]
        _ -> []
    getKeys (k:ks) val =
      case val of
        Object obj ->
          case parseMaybe (.: T.pack k) obj of
            Nothing -> []
            Just v -> if null ks then
              case v of
                Object o -> [H.keys o]
                Array arr -> nub $ concatMap (getKeys []) arr
                _ -> []
              else getKeys ks v
        Array arr -> nub $ concatMap (getKeys (k:ks)) arr
        _ -> []
