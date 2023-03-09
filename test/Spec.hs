import Test.Hspec

import BoardSpec qualified
import LogicSpec qualified

main :: IO ()
main = hspec $ do
    describe "Board" BoardSpec.spec
    describe "Logic" LogicSpec.spec
