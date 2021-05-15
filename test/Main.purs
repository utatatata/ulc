module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.ULC (testFv, testFreshVar, testAlphaConversion, testSbst, testBetaReduction, testIsPrenexNF, testCallByValue)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ]
    $ do
        testFv
        testFreshVar
        testAlphaConversion
        testSbst
        testBetaReduction
        testIsPrenexNF
        testCallByValue
