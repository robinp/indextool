{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Lib where

import Database.Cayley.Client
import Data.Text (Text, unpack)
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HM
import NeatInterpolation

-- | Creates a Cayley query (using Gizmo API) that retrieves the callgraph of
-- top-level entries in a given file (identified by its Kythe node id), for
-- example: 'kythe:corpus?path=some/path/to.hs'.
--
-- Note: The callgraph contains cycles in case of recursion.
-- Note: the Has(kind, variable) works for now, since haskell-indexer doesn't
--       emit proper type data yet, the granularity is only package/file/other,
--       with latter denoted 'variable' ad-hocly. But it's an ugly hack, in
--       the future the indexer could emit custom Kythe facts (like
--       is-toplevel-definition).
q :: Text -> Text
q fileNode = [text|
var tops = g.M().Out("/kythe/edge/childof").Has("/kythe/node/kind", "package").In("/kythe/edge/childof").Has("/kythe/node/kind", "variable");
var fileNode = g.V("$fileNode");
var simplify = function(s) { var parts = s.split('%3A'); return parts[parts.length - 1]; };
fileNode
  .Follow(tops).As("target")
  .In("/kythe/edge/ref").Out("/kythe/edge/childof").Has("/kythe/node/kind", "variable")
  .Intersect(fileNode.Follow(tops))
  .As("source")
  .ForEach(function(d) {
    g.Emit({source: simplify(d.source), target: simplify(d.target)});
  });
|]

-- Blatantly partial functions below, use at your own risk.

-- | Executes the above created Cayley query.
runCayley :: Text -> IO (V.Vector (Text, Text))
runCayley fileNode = do
    -- cayley-client doesn't have a withCayley method? Also doesn't have a
    -- release?
    conn <- connectCayley defaultCayleyConfig
    Right result <- query conn (q fileNode)
    return $ convert result

-- | Converts the Cayley query's JSON result into source-target pairs.
convert :: Value -> V.Vector (Text, Text)
convert (Array v) = flip fmap v $ \(Object o) ->
    let src = forceString $ HM.lookupDefault "?" "source" o
        dst = forceString $ HM.lookupDefault "?" "target" o
    in (src, dst)
    where
      forceString (String s) = s
      forceString _ = error "not a string"
convert _ = error ("unexpected result: " ++ show x)

