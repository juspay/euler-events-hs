module Main where

import           Test.DocTest (doctest)

-- Taken from https://github.com/kowainik/membrain/blob/master/test/Doctest.hs

main :: IO ()
main = do
  doctest
    [ "-isrc"
    , "src/Euler/Events/MetricAPI.hs"
    ]
