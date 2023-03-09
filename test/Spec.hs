import Test.Hspec

import BoardSpec qualified

main :: IO ()
main = hspec $ do
    describe "Board" BoardSpec.spec
