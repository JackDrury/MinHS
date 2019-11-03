module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | Clos VEnv Exp
           -- Others as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE E.empty e
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value
-- First we start with the eacy constants and boolean constructors
-- We add Nil since it also fits the pattern
evalE env (Num n)     = I n
evalE env (Con str)  = case str of
                          "Nil"   -> Nil
                          "False" -> B False
                          "True"  -> B True
                          _       -> error "unknown constant"


-- Then we need to know how to look up a variable in the environment
-- (which was demonstrated in the lecture)

--Perhaps this needs another case? Like for closures or recfun or something?
evalE env (Var v) = case E.lookup env v of
                      Just r  -> r
                      Nothing -> error "could not find the variable in environment :("



-- We address the special case of an integer primops with only one argument, negation:
evalE env (App (Prim Neg) e) = let v = evalE env e
                                 in case v of
                                      I n -> I (-n)
                                      _   -> error "can only negate integers"

--We address the list operations (other than Nil):
evalE env (App (Prim operator) x) = case evalE env x of
                                Nil       -> case operator of
                                               Head -> error "empty list has no head, boo!"
                                               Tail -> error "empty list has no tail, boo!"
                                               Null -> B True
                                               _    -> error "op not defined for empty list"
                                         
                                Cons v vs -> case operator of
                                               Head -> I v
                                               Tail -> vs
                                               Null -> B False
                                               _    -> error "op not defined for non-empty lists"
                                _         -> error (" not defined ")
-- will the last line stop integer primops from ever working?                                
                              


--We address the primitive operations for integers:

evalE env (App (App (Prim operator) e1) e2) = case (evalE env e1, evalE env e2) of
                                          (I v1, I v2) -> case operator of
                                                            Add  -> I (v1 + v2)
                                                            Sub  -> I (v1 - v2)
                                                            Mul  -> I (v1 * v2)
                                                            Quot -> case v2 of
                                                                      0 ->  error "div by zero ==> there is no god"
                                                                      _ ->  I (quot v1 v2)
                                                            Rem  -> I (rem v1 v2)
                                                            Gt   -> B (v1 > v2)
                                                            Ge   -> B (v1 >= v2)
                                                            Lt   -> B (v1 < v2)
                                                            Le   -> B (v1 <= v2)
                                                            Eq   -> B (v1 == v2)
                                                            Ne   -> B (v1 /= v2)
                                                            Ge   -> B (v1 >= v2)
                                                            _    -> error "unknown operator..."
                                                     _ -> error "these operators only work on integers"
-- Is there a way to force that these will only happen for integers????
-- Maybe I just have to deal with the list ops beforehand and then
-- we will only reach this scenario afterwards...
                                                      

-- If expressions are very simple:
evalE env (If exp1 exp2 exp3) = case evalE env exp1 of
                                   B False -> evalE env exp3
                                   B True  -> evalE env exp2
                                   _       -> error "Something weird happened, boolean logic is not sound"

-- The abstract syntax defines let expressions with a list of Binds applied to an expression.
-- When we evaluate things in the Let statement we need to add the let bindings to the environment.
-- We want to enable multiple let bindings be applied to a single expression (TASK 4)
evalE env (Let e1 e2) = case e1 of
                              []                   -> evalE env e2
                              (Bind str _ l e3):bs -> case l of
                                                        []       -> let env' = E.add env (str, (evalE env e3))
                                                                      in evalE env' (Let bs e2)
                                                        arg_list -> let env' = E.add env (str, (Clos env e3))
                                                                      in evalE env' (Let bs e2)
-- May need to expand closures so I can feed the arg_list into it... I think it doesn't work otherwise...   


{-                                      

NOT EXACTLY CORRECT, BUT GIVES A GENERAL IDEA FOR ONE OF THE RECFUN JUDGEMENTS

evalE env (Recfun x e)
  let env' = E.add env (x,v)
         v = evalE env' e
    in v

-}

evalE env exp = error "implement me!"
