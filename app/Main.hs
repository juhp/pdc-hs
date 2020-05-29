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
    [ Subcommand "branches" "Component branches - params: active (bool), critical_path (bool), global_component, name, type" $
      paramsCmd pdcComponentBranches <$> serverOpt <*> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "images" "List images - params: arch, bootable (bool), compose, disc_count, disc_number, file_name, image_format, image_type, implant_md5, md5, mtime, sha1, sha256, size, subvariant, volume_id" $
      paramsCmd pdcImages <$> serverOpt <*> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "modules" "List modules - params: active (bool), build_dep_name, build_dep_stream, component_branch, component_name, context, koji_tag, name, rpm_filename, runtime_dep_name, runtime_dep_stream, stream, uid, version" $
      paramsCmd pdcModules <$> serverOpt <*> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "rpms" "Search rpm binary packages - params: arch, built_for_release, compose, conflicts, epoch, filename, has_no_deps (bool), linked_release, name (regular expression), obsoletes, provides, recommends, release, requires, srpm_commit_branch, srpm_commit_hash, srpm_name, srpm_nevra, suggests, version" $
      paramsCmd pdcRpms <$> serverOpt <*> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "product-versions" "Product versions - params: active (bool), allowed_push_targets, name, product_version_id, short, version" $
      paramsCmd pdcProductVersions <$> serverOpt <*> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "branch-slas" "SLA for component branches - params: branch, branch_active (bool), branch_critical_path (bool), branch_type, eol, eol_after, eol_before, global_component, sla" $
      paramsCmd pdcComponentBranchSLAs <$> serverOpt <*> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "changeset" "Log of PDC changes - params: author, changed_since, changed_until, comment, resource" $
      paramsCmd pdcChangesets <$> serverOpt <*> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "releases" "Releases - params: active (bool), allow_buildroot_push (bool), allowed_debuginfo_services, allowed_push_targets, base_product, bugzilla_product, dist_git_branch, has_base_product, integrated_with, name, product_version, release_id, release_type, short, sigkey, version" $
      paramsCmd pdcReleases <$> serverOpt <*> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "components" "PDC global components - params: dist_git_path, label, name, upstream_homepage, upstream_scm_type, upstream_scm_url" $
      paramsCmd pdcGlobalComponents <$> serverOpt <*> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    ]
  where
    serverOpt = strOptionalWith 's' "server" "SERVER" "PDC server" fedoraPDC

    jsonOpt = switchWith 'j' "json" "Output json instead of yaml"

    keysOpt = switchWith 'k' "keys" "List keys of object"

    valuesOpt = optional (splitOn "." <$> strOptionWith 'v' "value" "KEY[.KEY..]" "Key value to show")

    -- argCmd :: (String -> IO Object) -> Bool -> Bool -> Maybe [String] -> String -> IO ()
    -- argCmd cmd json listkeys mkeys arg = do
    --   obj <- cmd arg
    --   if listkeys
    --     then mapM_ putKeys $ filter (not . null) $ getKeys (concat mkeys) (Object obj)
    --     else putKeysVal json (concat mkeys) (Object obj)

    -- argCmdMaybe :: (String -> IO (Maybe Object)) -> Bool -> Bool -> Maybe [String] -> String -> IO ()
    -- argCmdMaybe cmd json listkeys mkeys arg = do
    --   mobj <- cmd arg
    --   case mobj of
    --     Nothing -> error "Query failed"
    --     Just obj ->
    --       if listkeys
    --       then mapM_ putKeys $ filter (not . null) $ getKeys (concat mkeys) (Object obj)
    --       else putKeysVal json (concat mkeys) (Object obj)

    -- FIXME add --count
    -- FIXME special format fields= output
    paramsCmd :: (String -> Query -> IO Object) -> String -> Bool -> Bool -> Maybe [String] -> [String] -> IO ()
    paramsCmd cmd server json listkeys mkeys args = do
      let params = readQuery args
      result <- cmd server params
      case lookupKey "count" result :: Maybe Int of
        Nothing -> putPretty json result
        Just n -> do
          putStrLn $ show n ++ " hits:\n"
          let objs = getResultsList result
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
