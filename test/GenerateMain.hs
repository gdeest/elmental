module Main (main) where

import Codegen.SampleTypes (sampleTypes)
import Elmental.Generate

main :: IO ()
main = generateAll "src" sampleTypes
