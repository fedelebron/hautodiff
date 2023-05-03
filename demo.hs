import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import Definition
import Error
import Evaluate
import GHC.Stack
import Linearize
import Optimizations
import Text.Parsec (parse)
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Transpose
import TypeInference

main :: IO ()
main = do
    let echo = lift . putStrLn
    result <- runExceptT $ do
        file <- lift $ readFile "transpose.hx"
        def <- except (first toError (parse parseDefinition "" file))
        echo "Initial source:"
        echo (showDefinition def)
        typedDef <- except (inferTypes def)
        echo "After type inference:"
        echo (showDefinition typedDef)
        (inputs, cotangents) <- except (makeSampleInput typedDef)
        lift . putStr $ "Evaluating at " ++ prettyShow inputs ++ "..."
        res <- except (evaluate typedDef inputs)
        echo " result: "
        echo (prettyShow res)
        echo "Linearized: "
        (res', df) <- except (linearize inputs typedDef)
        echo (showDefinition df)
        _ <-
            except . assertTrue (res == res') $
                Error "Linearization and evalution produced different pointwise results!" callStack
        echo $ "Evaluating derivative at " ++ prettyShow inputs
        dres <- except (evaluate df inputs)
        echo (prettyShow dres)
        echo "After constant folding: "
        let constantFoldedDf = constantFoldDef df
        echo (showDefinition constantFoldedDf)
        echo "After dead code elimination: "
        let dceDf = eliminateDeadCode constantFoldedDf
        echo (showDefinition dceDf)
        echo "After transposing: "
        transposed <- except (transposeDef dceDf)
        echo (showDefinition transposed)
        lift . putStr $
            "Evaluating at transposed at cotangents "
                ++ prettyShow cotangents
                ++ "..."
        dtres <- except (evaluate transposed cotangents)
        echo " result: "
        echo (prettyShow dtres)

    either (putStrLn . prettyShow) return result
  where
    toError parseError = Error (show parseError) callStack
