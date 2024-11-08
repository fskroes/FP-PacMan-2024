import Test.Hspec
import qualified AISpec
import qualified PhysicsSpec

main :: IO ()
main = hspec $ do
    AISpec.spec
    PhysicsSpec.spec
