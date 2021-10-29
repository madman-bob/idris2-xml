module XMLTests

import Test.Golden

prologTests : TestPool
prologTests = MkTestPool "Prolog" [] Nothing [
    "DocType", "Prolog", "XMLDecl"
  ]

miscTests : TestPool
miscTests = MkTestPool "Misc" [] Nothing [
    "Comment", "ProcessingInstruction"
  ]

documentTests : TestPool
documentTests = MkTestPool "XMLDocument" [] Nothing [
    "CharData", "Element", "Name", "ReadmeExample", "XMLDocument"
  ]

main : IO ()
main = runner [
    testPaths "Prolog" prologTests,
    testPaths "Misc" miscTests,
    testPaths "XMLDocument" documentTests
  ]
  where
    testPaths : String -> TestPool -> TestPool
    testPaths dir = record { testCases $= map ((dir ++ "/") ++) }
