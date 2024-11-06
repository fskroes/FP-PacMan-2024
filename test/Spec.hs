import Test.Hspec
import qualified AISpec

main :: IO ()
main = hspec $ do
    AISpec.spec
