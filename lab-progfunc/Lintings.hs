module Lintings where

import AST
import LintTypes
--------------------------------------------------------------------------------
-- AUXILIARES
--------------------------------------------------------------------------------

-- Computa la lista de variables libres de una expresión
freeVariables :: Expr -> [Name]
freeVariables = undefined

--------------------------------------------------------------------------------
-- LINTINGS
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Computación de constantes
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Reduce expresiones aritméticas/booleanas
-- Construye sugerencias de la forma (LintCompCst e r)
lintComputeConstant :: Linting Expr
lintComputeConstant expr = case expr of
   Infix And (Lit (LitBool a)) (Lit (LitBool b)) -> (Lit (LitBool (a && b)), [LintCompCst expr (Lit (LitBool (a && b)))])
   Infix Or (Lit (LitBool a)) (Lit (LitBool b)) -> (Lit (LitBool (a || b)), [LintCompCst expr (Lit (LitBool (a || b)))])
   Infix Mult (Lit (LitInt a)) (Lit (LitInt b)) -> (Lit (LitInt (a * b)), [LintCompCst expr (Lit (LitInt (a * b)))])
   Infix Div (Lit (LitInt a)) (Lit (LitInt b)) -> (Lit (LitInt (div a b)), [LintCompCst expr (Lit (LitInt (div a  b)))])
   Infix Add (Lit (LitInt a)) (Lit (LitInt b)) -> (Lit (LitInt (a + b)), [LintCompCst expr (Lit (LitInt (a + b)))])
   Infix Sub (Lit (LitInt a)) (Lit (LitInt b)) -> (Lit (LitInt (a - b)), [LintCompCst expr (Lit (LitInt (a - b)))])
   Infix op e1 e2  -> lintComputeConstant e2
   _ -> (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de chequeos redundantes de booleanos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Elimina chequeos de la forma e == True, True == e, e == False y False == e
-- Construye sugerencias de la forma (LintBool e r)
lintRedBool :: Linting Expr
lintRedBool (Infix Eq (Var a) (Lit (LitBool True))) = (Var a, [LintBool (Infix Eq (Var a) (Lit (LitBool True))) (Var a)])
lintRedBool (Infix Eq (Var a) (Lit (LitBool False))) = (App (Var "not") (Var a), [LintBool (Infix Eq (Var a) (Lit (LitBool False))) (App (Var "not") (Var a))])
lintRedBool (Infix Eq (Lit (LitBool True)) (Var a)) = (Var a, [LintBool (Infix Eq (Lit (LitBool True)) (Var a)) (Var a)])
lintRedBool (Infix Eq (Lit (LitBool False)) (Var a)) = (App (Var "not") (Var a), [LintBool (Infix Eq (Lit (LitBool False)) (Var a)) (App (Var "not") (Var a))])
lintRedBool (Infix Eq e1 e2) = lintRedBool e1
lintRedBool expr = (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de if redundantes
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Sustitución de if con literal en la condición por la rama correspondiente
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfCond :: Linting Expr
lintRedIfCond expr = case expr of
   If (Lit (LitBool True)) exp2 exp3 -> (exp2, [LintRedIf expr exp2])
   If (Lit (LitBool False)) exp2 exp3 -> (exp3, [LintRedIf expr exp3])
   If e1 e2 e3 -> lintRedIfCond e1
   _ -> (expr, [])

--------------------------------------------------------------------------------
-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfAnd :: Linting Expr
lintRedIfAnd expr = case expr of
   If (Var a) (Var b) (Lit (LitBool False)) -> (Infix And (Var a) (Var b), [LintRedIf expr (Infix And (Var a) (Var b))])
   If e1 e2 e3 -> lintRedIfAnd e1
   _ -> (expr, [])

--------------------------------------------------------------------------------
-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfOr :: Linting Expr
lintRedIfOr expr = case expr of
   If (Var a) (Lit (LitBool True)) (Var b) -> (Infix Or (Var a) (Var b), [LintRedIf expr (Infix Or (Var a) (Var b))])
   If e1 e2 e3 -> lintRedIfOr e1
   _ -> (expr, [])

--------------------------------------------------------------------------------
-- Chequeo de lista vacía
--------------------------------------------------------------------------------
-- Sugiere el uso de null para verificar si una lista es vacía
-- Construye sugerencias de la forma (LintNull e r)
lintNull :: Linting Expr
lintNull expr = case expr of
   Infix Eq (App (Var "length") (Var a)) (Lit (LitInt 0)) -> (App (Var "null") (Var a), [LintNull expr (App (Var "null") (Var a))])
   Infix Eq (Lit (LitInt 0)) (App (Var "length") (Var a)) -> (App (Var "null") (Var a), [LintNull expr (App (Var "null") (Var a))])
   Infix Eq (Lit LitNil) (Var a)  -> (App (Var "null") (Var a), [LintNull expr (App (Var "null") (Var a))])
   Infix Eq (Var a) (Lit LitNil) -> (App (Var "null") (Var a), [LintNull expr (App (Var "null") (Var a))])
   Infix op e2 e3 -> lintNull e2
   _ -> (expr, [])
--------------------------------------------------------------------------------
-- Eliminación de la concatenación
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (e:[] ++ es), reemplazando por (e:es)
-- Construye sugerencias de la forma (LintAppend e r)
lintAppend :: Linting Expr
lintAppend expr = case expr of
   Infix Append (Infix Cons e1 (Lit LitNil)) e2 -> (Infix Cons e1 e2, [LintAppend expr (Infix Cons e1 e2)])
   Infix Append e1 (Infix Cons e2 (Lit LitNil)) -> (Infix Cons e1 e2, [LintAppend expr (Infix Cons e1 e2)])
   Infix Append e1 e2 -> lintAppend e1
   _ -> (expr, [])

--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (f (g t)), reemplazando por (f . g) t
-- Construye sugerencias de la forma (LintComp e r)
lintComp :: Linting Expr
lintComp = undefined


--------------------------------------------------------------------------------
-- Eta Redución
--------------------------------------------------------------------------------
-- se aplica en casos de la forma \x -> e x, reemplazando por e
-- Construye sugerencias de la forma (LintEta e r)
lintEta :: Linting Expr
lintEta = undefined


--------------------------------------------------------------------------------
-- Eliminación de recursión con map
--------------------------------------------------------------------------------

-- Sustituye recursión sobre listas por `map`
-- Construye sugerencias de la forma (LintMap f r)
lintMap :: Linting FunDef
lintMap = undefined


--------------------------------------------------------------------------------
-- Combinación de Lintings
--------------------------------------------------------------------------------


-- Dada una transformación a nivel de expresión, se construye
-- una transformación a nivel de función
liftToFunc :: Linting Expr -> Linting FunDef
liftToFunc lintExpr (FunDef name expr) =
  let (newExpr, suggestions) = lintExpr expr
  in (FunDef name newExpr, suggestions)

-- encadenar transformaciones:
(>==>) :: Linting a -> Linting a -> Linting a
lint1 >==> lint2 = \expr ->
   let (expr', suggestions1) = lint1 expr
       (expr'', suggestions2) = lint2 expr'
   in (expr'', suggestions1 ++ suggestions2)

-- aplica las transformaciones 'lints' repetidas veces y de forma incremental,
-- hasta que ya no generen más cambios en 'func'
lintRec :: Linting a -> Linting a
lintRec lints func =
   if null suggestions
      then (func, [])
      else (finalFunc, suggestions ++ moreSuggestions)
   where
      (func', suggestions) = lints func
      (finalFunc, moreSuggestions) = lintRec lints func'
