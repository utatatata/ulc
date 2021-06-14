module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.ULC (testFv, testFetchFreshVar, testAlphaConversion, testSbst, testBetaReduction, testCallByValue, testCallByValueCPS, testCallByValueTrampoline)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ]
    $ do
        testFv
        testFetchFreshVar
        testAlphaConversion
        testSbst
        testBetaReduction
        testCallByValue
        testCallByValueCPS
        testCallByValueTrampoline
