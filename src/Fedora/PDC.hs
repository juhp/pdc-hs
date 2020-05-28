{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright: (c) 2020 Jens Petersen
SPDX-License-Identifier: MIT
Maintainer: Jens Petersen <juhpetersen@gmail.com>

Fedora PDC web api client library
-}

module Fedora.PDC
where

#if (defined(VERSION_lens_aeson))
import Control.Lens
import Data.Aeson.Lens
#else
import Lens.Micro
import Lens.Micro.Aeson
#endif
import Data.Aeson.Types
#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,3))
#else
import Data.ByteString (ByteString)
#endif
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Text (Text)
import Network.HTTP.Simple
import System.FilePath ((</>))

server :: String
server = "pdc.fedoraproject.org"

-- | Arch List
--
-- https://pdc.fedoraproject.org/rest_api/v1/arches/
pdcArches :: IO Object
pdcArches = do
  res <- queryPdc "arches" []
  return $ res ^. _Object

-- https://pdc.fedoraproject.org/rest_api/v1/auth/ *

-- https://pdc.fedoraproject.org/rest_api/v1/base-products/ no-op?

-- https://pdc.fedoraproject.org/rest_api/v1/bugzilla-components/ no-op?

-- | Changeset List
--
-- https://pdc.fedoraproject.org/rest_api/v1/changesets/
pdcChangesets :: Query -> IO Object
pdcChangesets params = do
  res <- queryPdc "changesets" params
  return $ res ^. _Object

-- | Sla To Component Branch List
--
-- https://pdc.fedoraproject.org/rest_api/v1/component-branch-slas/
pdcComponentBranchSLAs :: Query -> IO Object
pdcComponentBranchSLAs params = do
  res <- queryPdc "component-branch-slas" params
  return $ res ^. _Object

-- | Component Branch List
--
-- https://pdc.fedoraproject.org/rest_api/v1/component-branches/
pdcComponentBranches :: Query -> IO [Object]
pdcComponentBranches params = do
  res <- queryPdc "component-branches" params
  return $ res ^.. key "results" . values . _Object

-- | Sla List
--
-- https://pdc.fedoraproject.org/rest_api/v1/component-sla-types/
pdcComponentSLATypes :: Query -> IO Object
pdcComponentSLATypes params = do
  res <- queryPdc "component-sla-types"  params
  return $ res ^. _Object

-- | Compose Image Rtt Test List
--
-- https://pdc.fedoraproject.org/rest_api/v1/compose-image-rtt-tests/ (all untested?)
pdcComposeImageRttTests :: Query -> IO Object
pdcComposeImageRttTests params = do
  view _Object <$> queryPdc "compose-image-rtt-tests" params

-- https://pdc.fedoraproject.org/rest_api/v1/compose-images/ {compose_id}/

-- https://pdc.fedoraproject.org/rest_api/v1/compose-rpms/ {compose_id}/

-- | Compose List
--
-- https://pdc.fedoraproject.org/rest_api/v1/composes/
pdcComposes :: Query -> IO Object
pdcComposes params = do
  view _Object <$> queryPdc "composes" params

-- https://pdc.fedoraproject.org/rest_api/v1/composes/%7Bcompose_id%7D/rpm-mapping/%7Bpackage%7D/

-- | Content Format List
--
-- https://pdc.fedoraproject.org/rest_api/v1/content-delivery-content-formats/
pdcContentDeliveryContentFormats :: Query -> IO Object
pdcContentDeliveryContentFormats params = do
  view _Object <$> queryPdc "content-delivery-content-formats" params

-- | Global Component List
--
-- https://pdc.fedoraproject.org/rest_api/v1/global-components/
pdcGlobalComponents :: Query -> IO Object
pdcGlobalComponents params = do
   view _Object <$> queryPdc "global-components" params

-- | Image List
--
-- https://pdc.fedoraproject.org/rest_api/v1/images/
pdcImages :: Query -> IO Object
pdcImages params =
   view _Object <$> queryPdc "images" params

-- | Module List
--
-- https://pdc.fedoraproject.org/rest_api/v1/modules/
pdcModules :: Query -> IO Object
pdcModules params = do
  view _Object <$> queryPdc "modules" params

-- | Product Version List
--
-- https://pdc.fedoraproject.org/rest_api/v1/product-versions/
pdcProductVersions :: Query -> IO Object
pdcProductVersions params = do
  view _Object <$> queryPdc "product-versions" params

-- | Product List
--
-- https://pdc.fedoraproject.org/rest_api/v1/products/
pdcProducts :: Query -> IO Object
pdcProducts params = do
   view _Object <$> queryPdc "products" params

-- | Release List
--
-- https://pdc.fedoraproject.org/rest_api/v1/releases/
pdcReleases :: Query -> IO Object
pdcReleases params = do
  view _Object <$> queryPdc "releases" params

-- | Filter Bugzilla Products And Components List
--
-- https://pdc.fedoraproject.org/rest_api/v1/rpc/where-to-file-bugs/
pdcWhereToFileBugs :: Query -> IO Object
pdcWhereToFileBugs params =
  view _Object <$> queryPdc "rpc/where-to-file-bugs" params

-- | Rpm List
--
-- https://pdc.fedoraproject.org/rest_api/v1/rpms/
pdcRpms :: Query -> IO Object
pdcRpms params = do
  view _Object <$> queryPdc "rpms" params

-- | low-level query
queryPdc :: String -> Query -> IO Value
queryPdc path params = do
  let url = "https://" ++ server </> "rest_api/v1" </> path
  req <- setRequestQueryString params <$> parseRequest url
  getResponseBody <$> httpJSON req

-- | Maybe create a query key
maybeKey :: String -> Maybe String -> Query
maybeKey _ Nothing = []
maybeKey k mval = [(B.pack k, fmap B.pack mval)]

-- | make a singleton key-value Query
makeKey :: String -> String -> Query
makeKey k val = [(B.pack k, Just (B.pack val))]

-- | make a key-value QueryItem
makeItem :: String -> String -> QueryItem
makeItem k val = (B.pack k, Just (B.pack val))

-- | looks up key in object
lookupKey :: FromJSON a => Text -> Object -> Maybe a
lookupKey k = parseMaybe (.: k)

-- | like lookupKey but raises an error if no key found
lookupKey' :: FromJSON a => Text -> Object -> a
lookupKey' k obj =
  fromMaybe (error ("no key: " ++ show k)) (lookupKey k obj)

#if !MIN_VERSION_http_conduit(2,3,1)
type Query = [(ByteString, Maybe ByteString)]
#endif
#if !MIN_VERSION_http_conduit(2,3,3)
type QueryItem = (ByteString, Maybe ByteString)
#endif
