import Control.Monad.Free

data DSL next = One String String next | Two Int Int (Int -> next) | End
	deriving (Functor)

type P = Free DSL

one :: String -> String -> P ()
one s r = liftF $ One s r ()

two :: Int -> Int -> P Int
two x y = liftF $ Two x y id

end :: P () 
end = liftF End

program :: P () 
program = do
	x <- two 2 3
	one "2 + 3 is " (show x)
	end

liftF :: (Functor f) => f r -> Free f r
liftF command = Impure (fmap Pure command)

showProgram :: (Show r) => P r -> String
showProgram (Impure (One a b x)) =
    "One " ++ show a ++ ", " ++ show b ++ "\r\n" ++ showProgram x
showProgram (Impure (Two a b x)) =
    "Two " ++ show a ++ ", " ++ show b ++ "\r\n" ++ showProgram (x (a + b))
showProgram (Impure End) =
    "done\r\n"
showProgram (Pure r) =
    "return " ++ show r ++ "\r\n"

interpret :: (Show b) => P b -> IO ()
interpret (Impure (One a b x)) = print (a ++ b) >> interpret x
interpret (Impure (Two a b x)) = interpret (x (a + b))
interpret (Impure End) = print "End"

main :: IO ()
main = undefined
