module RegAlloc.Tests (tests) where

import           Test.Tasty

import qualified RegAlloc.Amd64Tests as Amd64Tests

tests :: TestTree
tests = testGroup "Register allocation tests"
  [ Amd64Tests.tests
  ]
