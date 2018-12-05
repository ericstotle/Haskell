-- A tautology checker for propositional logic based on Hutton (2016). 

module Tautology where

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop

-- A and not A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- (A and B) => B
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A => (A and B)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A and (A => B)) => B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- A or B inclusive disjunction
p5 :: Prop
p5 = Or (Var 'A') (Var 'B')

-- A <--> B biconditional
p6 :: Prop
p6 = And (Imply (Var 'A') (Var 'B')) (Imply (Var 'B') (Var 'A'))

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k]

--function for evaluating propositions given a substitution, using pattern matching
--on the five possible forms
eval :: Subst -> Prop -> Bool
eval _ (Const b)    = b
eval s (Var x)      = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Or p q)     = eval s p || eval s q
eval s (Imply p q)  = eval s p <= eval s q

--function returning list of variables in a proposition
vars :: Prop -> [Char]
vars (Const _)    = []
vars (Var x)      = [x]
vars (Not p)      = vars p
vars (And p q)    = vars p ++ vars q
vars (Or p q)     = vars p ++ vars q
vars (Imply p q)  = vars p ++ vars q

--we need a function to generate lists of all possible logical values of length 3
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True:) bss
             where bss = bools (n-1)

-- function for filtering out duplicates on a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
               where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

main :: Prop -> Bool -> IO ()
main       = do
             putStrLn "Welcome to the tautology checker. What can I do for you?"
             getLine
             if prop == p2
                then putStrLn "Definitely a tautology!"
             else if prop == p4
                then putStrLn "Definitely a tautology!"
             else putStrLn "Nope!"
