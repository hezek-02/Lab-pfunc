module Lintings where

import AST
import LintTypes
--------------------------------------------------------------------------------
-- AUXILIARES
--------------------------------------------------------------------------------

-- Computa la lista de variables libres de una expresión
freeVariables :: Expr -> [Name]
freeVariables (Var x) = [x]
freeVariables (Lit _) = []
freeVariables (Infix _ e1 e2) = freeVariables e1 ++ freeVariables e2
freeVariables (App e1 e2) = freeVariables e1 ++ freeVariables e2
freeVariables (Lam x e) = filter (/= x) (freeVariables e)
freeVariables (Case e1 e2 (x, y, e3)) = freeVariables e1 ++ freeVariables e2 ++ filter (\v -> v /= x && v /= y) (freeVariables e3)
freeVariables (If e1 e2 e3) = freeVariables e1 ++ freeVariables e2 ++ freeVariables e3

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
   Infix op expr1 expr2 ->
      let (expr1', suggestions1) = lintComputeConstant expr1
          (expr2', suggestions2) = lintComputeConstant expr2
      in (Infix op expr1' expr2', suggestions1 ++ suggestions2)
   App e1 e2 ->
      let (e1', suggestions1) = lintComputeConstant e1
          (e2', suggestions2) = lintComputeConstant e2
      in (App e1' e2', suggestions1 ++ suggestions2)
   Lam x e ->
      let (e', suggestions) = lintComputeConstant e
      in (Lam x e', suggestions)
   Case e1 e2 (x, y, e3) ->
      let (e1', suggestions1) = lintComputeConstant e1
          (e2', suggestions2) = lintComputeConstant e2
          (e3', suggestions3) = lintComputeConstant e3
      in (Case e1' e2' (x, y, e3'), suggestions1 ++ suggestions2 ++ suggestions3)
   If e1 e2 e3 ->
      let (e1', suggestions1) = lintComputeConstant e1
          (e2', suggestions2) = lintComputeConstant e2
          (e3', suggestions3) = lintComputeConstant e3
      in (If e1' e2' e3', suggestions1 ++ suggestions2 ++ suggestions3)
   _ -> (expr, [])

--lintComputeConstant expr = (expr, [LintCompCst expr expr]) 

--------------------------------------------------------------------------------
-- Eliminación de chequeos redundantes de booleanos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Elimina chequeos de la forma e == True, True == e, e == False y False == e
-- Construye sugerencias de la forma (LintBool e r)
lintRedBool :: Linting Expr
lintRedBool (Infix Eq (Lit (LitBool True))  (Lit (LitBool True))) = (Lit (LitBool True), [LintBool (Infix Eq (Lit (LitBool True)) (Lit (LitBool True))) (Lit (LitBool True))])
lintRedBool (Infix Eq (Lit (LitBool False))  (Lit (LitBool False))) = (App (Var "not") (Lit (LitBool False)), [LintBool (Infix Eq (Lit (LitBool False)) (Lit (LitBool False))) (App (Var "not") (Lit (LitBool False)))])
lintRedBool (Infix Eq (Lit (LitBool True))  (Lit (LitBool False))) = (Lit (LitBool False), [LintBool (Infix Eq (Lit (LitBool True)) (Lit (LitBool False))) (Lit (LitBool False))])
lintRedBool (Infix Eq (Lit (LitBool False))  (Lit (LitBool True))) = (Lit (LitBool False), [LintBool (Infix Eq (Lit (LitBool False)) (Lit (LitBool True))) (Lit (LitBool False))])
lintRedBool (Infix Eq (Var x)  (Lit (LitBool True))) = (Var x, [LintBool (Infix Eq (Var x) (Lit (LitBool True))) (Var x)])
lintRedBool (Infix Eq (Lit (LitBool True)) (Var y)) = (Var y, [LintBool (Infix Eq (Lit (LitBool True)) (Var y)) (Var y)])
lintRedBool (Infix Eq (Var x) (Lit (LitBool False))) = (App (Var "not") (Var x), [LintBool (Infix Eq (Var x) (Lit (LitBool False))) (App (Var "not") (Var x))])
lintRedBool (Infix Eq (Lit (LitBool False)) (Var y)) = (App (Var "not") (Var y), [LintBool (Infix Eq (Lit (LitBool False)) (Var y)) (App (Var "not") (Var y))])
lintRedBool (Infix a2 e1 e2) =
   let (e1', suggestions1) = lintRedBool e1
       (e2', suggestions2) = lintRedBool e2
   in (Infix a2 e1' e2', suggestions1 ++ suggestions2)
lintRedBool (App e1 e2) =
   let (e1', suggestions1) = lintRedBool e1
       (e2', suggestions2) = lintRedBool e2
   in (App e1' e2', suggestions1 ++ suggestions2)
lintRedBool (Lam x e) =
   let (e', suggestions) = lintRedBool e
   in (Lam x e', suggestions)
lintRedBool (Case e1 e2 (x, y, e3)) =
   let (e1', suggestions1) = lintRedBool e1
       (e2', suggestions2) = lintRedBool e2
       (e3', suggestions3) = lintRedBool e3
   in (Case e1' e2' (x, y, e3'), suggestions1 ++ suggestions2 ++ suggestions3)
lintRedBool expr = (expr, [])
-- lintRedBool expr = (expr, [LintBool expr expr])

--------------------------------------------------------------------------------
-- Eliminación de if redundantes
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Sustitución de if con literal en la condición por la rama correspondiente
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfCond :: Linting Expr
lintRedIfCond expr = case expr of
   If (Lit (LitBool True)) (Var x) exp3 -> (Var x, [LintRedIf expr (Var x)])
   If (Lit (LitBool False)) exp2 (Var x) -> (Var x, [LintRedIf expr (Var x)])
   If (Lit (LitBool True)) (Lit e) exp3  -> (Lit e, [LintRedIf expr (Lit e)])
   If (Lit (LitBool False)) exp2 (Lit e) -> (Lit e, [LintRedIf expr (Lit e)])
   If (Lit (LitBool True)) exp2 exp3 ->
      let (e2', suggestions2) = lintRedIfCond exp2
      in (e2', suggestions2 ++ [LintRedIf (If (Lit (LitBool True)) e2' exp3) e2'])
   If e1 e2 e3 ->
      let (e1', suggestions1) = lintRedIfCond e1
          (e2', suggestions2) = lintRedIfCond e2
          (e3', suggestions3) = lintRedIfCond e3
      in (If e1' e2' e3', suggestions1 ++ suggestions2 ++ suggestions3)
   Infix op e1 e2 ->
      let (e1', suggestions1) = lintRedIfCond e1
          (e2', suggestions2) = lintRedIfCond e2
      in (Infix op e1' e2', suggestions1 ++ suggestions2)
   App e1 e2 ->
      let (e1', suggestions1) = lintRedIfCond e1
          (e2', suggestions2) = lintRedIfCond e2
      in (App e1' e2', suggestions1 ++ suggestions2)
   Lam x e ->
      let (e', suggestions) = lintRedIfCond e
      in (Lam x e', suggestions)
   Case e1 e2 (x, y, e3) ->
      let (e1', suggestions1) = lintRedIfCond e1
          (e2', suggestions2) = lintRedIfCond e2
          (e3', suggestions3) = lintRedIfCond e3
      in (Case e1' e2' (x, y, e3'), suggestions1 ++ suggestions2 ++ suggestions3)
   _ -> (expr, [])


--------------------------------------------------------------------------------
-- Sustitución de if por conjunción entre la condición y su rama _then_
-- Construye sugerencias de la forma (LintRedIf e r) --if c then True else if c then c else False
lintRedIfAnd :: Linting Expr
lintRedIfAnd expr = case expr of
   If (Lit e1) (Lit e2) (Lit (LitBool False)) -> (Infix And (Lit e1) (Lit e2), [LintRedIf expr (Infix And (Lit e1) (Lit e2))])
   If (Var x) (Var y) (Lit (LitBool False)) -> (Infix And (Var x) (Var y), [LintRedIf expr (Infix And (Var x) (Var y))])
   If (Lit e1) (Var y) (Lit (LitBool False)) -> (Infix And (Lit e1) (Var y), [LintRedIf expr (Infix And (Lit e1) (Var y))])
   If (Var x) (Lit e2) (Lit (LitBool False)) -> (Infix And (Var x) (Lit e2), [LintRedIf expr (Infix And (Var x) (Lit e2))])
   If exp1 expr2 (Lit (LitBool False)) ->
      let (exp1', suggestions1) = lintRedIfAnd exp1
          (expr2', suggestions2) = lintRedIfAnd expr2
      in (Infix And exp1' expr2', suggestions1 ++ suggestions2 ++ [LintRedIf (If exp1' expr2' (Lit (LitBool False))) (Infix And exp1' expr2')])
   If e1 e2 e3 ->
      let (e1', suggestions1) = lintRedIfAnd e1 --c
          (e2', suggestions2) = lintRedIfAnd e2 -- True
          (e3', suggestions3) = lintRedIfAnd e3-- c && c
      in (If e1' e2' e3', suggestions1 ++ suggestions2 ++ suggestions3)
   Infix op e1 e2 ->
      let (e1', suggestions1) = lintRedIfAnd e1
          (e2', suggestions2) = lintRedIfAnd e2
      in (Infix op e1' e2', suggestions1 ++ suggestions2)
   App e1 e2 ->
      let (e1', suggestions1) = lintRedIfAnd e1
          (e2', suggestions2) = lintRedIfAnd e2
      in (App e1' e2', suggestions1 ++ suggestions2)
   Lam x e ->
      let (e', suggestions) = lintRedIfAnd e
      in (Lam x e', suggestions)
   Case e1 e2 (x, y, e3) ->
      let (e1', suggestions1) = lintRedIfAnd e1
          (e2', suggestions2) = lintRedIfAnd e2
          (e3', suggestions3) = lintRedIfAnd e3
      in (Case e1' e2' (x, y, e3'), suggestions1 ++ suggestions2 ++ suggestions3)
   _ -> (expr, [])

--------------------------------------------------------------------------------
-- Sustitución de if por disyunción entre la condición y su rama _else_
-- Construye sugerencias de la forma (LintRedIf e r)
lintRedIfOr :: Linting Expr
lintRedIfOr expr = case expr of
   If (Var x) (Lit (LitBool True)) (Var y) -> (Infix Or (Var x) (Var y), [LintRedIf expr (Infix Or (Var x) (Var y))])
   If (Lit e1) (Lit (LitBool True)) (Lit e2) -> (Infix Or (Lit e1) (Lit e2), [LintRedIf expr (Infix Or (Lit e1) (Lit e2))])
   If (Lit e1) (Lit (LitBool True)) (Var y) -> (Infix Or (Lit e1) (Var y), [LintRedIf expr (Infix Or (Lit e1) (Var y))])
   If (Var x) (Lit (LitBool True)) (Lit e2) -> (Infix Or (Var x) (Lit e2), [LintRedIf expr (Infix Or (Var x) (Lit e2))])
   If exp1 (Lit (LitBool True)) exp2 ->
      let (exp1', suggestions1) = lintRedIfOr exp1
          (exp2', suggestions2) = lintRedIfOr exp2
      in (Infix Or exp1' exp2', suggestions1 ++ suggestions2 ++ [LintRedIf (If exp1' (Lit (LitBool True)) exp2') (Infix Or exp1' exp2')])
   Infix op e1 e2 ->
      let (e1', suggestions1) = lintRedIfOr e1
          (e2', suggestions2) = lintRedIfOr e2
      in (Infix op e1' e2', suggestions1 ++ suggestions2)
   App e1 e2 ->
      let (e1', suggestions1) = lintRedIfOr e1
          (e2', suggestions2) = lintRedIfOr e2
      in (App e1' e2', suggestions1 ++ suggestions2)
   Lam x e ->
      let (e', suggestions) = lintRedIfOr e
      in (Lam x e', suggestions)
   Case e1 e2 (x, y, e3) ->
      let (e1', suggestions1) = lintRedIfOr e1
          (e2', suggestions2) = lintRedIfOr e2
          (e3', suggestions3) = lintRedIfOr e3
      in (Case e1' e2' (x, y, e3'), suggestions1 ++ suggestions2 ++ suggestions3)
   If e1 e2 e3 ->
      let (e1', suggestions1) = lintRedIfOr e1
          (e2', suggestions2) = lintRedIfOr e2
          (e3', suggestions3) = lintRedIfOr e3
      in (If e1' e2' e3', suggestions1 ++ suggestions2 ++ suggestions3)
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
   Infix Eq exp1 (Lit LitNil) ->
      let (exp1', suggestions1) = lintNull exp1
      in (exp1', suggestions1 ++ [LintNull (Infix Eq exp1' (Lit LitNil)) exp1'])
   Infix Eq (Lit LitNil) exp2 ->
      let (exp2', suggestions2) = lintNull exp2
      in (exp2', suggestions2 ++ [LintNull (Infix Eq (Lit LitNil) exp2') exp2'])
   Infix Eq exp1 (App (Var "length") (Lit (LitInt 0))) ->
      let (exp1', suggestions1) = lintNull exp1
      in (exp1', suggestions1 ++ [LintNull (Infix Eq exp1' (App (Var "length") (Lit (LitInt 0)))) exp1'])
   Infix Eq (App (Var "length") (Lit (LitInt 0))) exp2 ->
      let (exp2', suggestions2) = lintNull exp2
      in (exp2', suggestions2 ++ [LintNull (Infix Eq (App (Var "length") (Lit (LitInt 0))) exp2') exp2'])
   Infix op e1 e2 ->
      let (e1', suggestions1) = lintNull e1
          (e2', suggestions2) = lintNull e2
      in (Infix op e1' e2', suggestions1 ++ suggestions2)
   App e1 e2 ->
      let (e1', suggestions1) = lintNull e1
          (e2', suggestions2) = lintNull e2
      in (App e1' e2', suggestions1 ++ suggestions2)
   Lam x e ->
      let (e', suggestions) = lintNull e
      in (Lam x e', suggestions)
   Case e1 e2 (x, y, e3) ->
      let (e1', suggestions1) = lintNull e1
          (e2', suggestions2) = lintNull e2
          (e3', suggestions3) = lintNull e3
      in (Case e1' e2' (x, y, e3'), suggestions1 ++ suggestions2 ++ suggestions3)
   If e1 e2 e3 ->
      let (e1', suggestions1) = lintNull e1
          (e2', suggestions2) = lintNull e2
          (e3', suggestions3) = lintNull e3
      in (If e1' e2' e3', suggestions1 ++ suggestions2 ++ suggestions3)
   _ -> (expr, [])
--------------------------------------------------------------------------------
-- Eliminación de la concatenación
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (e:[] ++ es), reemplazando por (e:es)
-- Construye sugerencias de la forma (LintAppend e r)

lintAppend :: Linting Expr
lintAppend expr = case expr of
   Infix Append (Infix Cons (Var x) (Lit LitNil)) (Var y)  -> (Infix Cons (Var x) (Var y), [LintAppend expr (Infix Cons (Var x) (Var y))])
   Infix Append (Infix Cons (Var x) (Lit LitNil)) (App n2 e2)  -> (Infix Cons (Var x) (App n2 e2), [LintAppend expr (Infix Cons (Var x) (App n2 e2))])
   Infix Append (Infix Cons (App n e) (Lit LitNil)) (Var y)  -> (Infix Cons (App n e) (Var y), [LintAppend expr (Infix Cons (App n e) (Var y))])
   Infix Append (Infix Cons (App n e) (Lit LitNil)) (App n2 e2)  -> (Infix Cons (App n e) (App n2 e2), [LintAppend expr (Infix Cons (App n e) (App n2 e2))])
   Infix Append (Infix Cons exp1 (Lit LitNil)) exp2 ->
      let (exp1', suggestions1) = lintAppend exp1
          (exp2', suggestions2) = lintAppend exp2
      in (Infix Cons exp1' exp2', suggestions1 ++ suggestions2 ++ [LintAppend (Infix Append (Infix Cons exp1' (Lit LitNil)) exp2') (Infix Cons exp1' exp2')])
   Infix op e1 e2 -> --generales recursion de exploracion
      let (e1', suggestions1) = lintAppend e1
          (e2', suggestions2) = lintAppend e2
      in (Infix op e1' e2', suggestions1 ++ suggestions2)
   App e1 e2 ->
      let (e1', suggestions1) = lintAppend e1
          (e2', suggestions2) = lintAppend e2
      in (App e1' e2', suggestions1 ++ suggestions2)
   Lam x e ->
      let (e', suggestions) = lintAppend e
      in (Lam x e', suggestions)
   Case e1 e2 (x, y, e3) ->
      let (e1', suggestions1) = lintAppend e1
          (e2', suggestions2) = lintAppend e2
          (e3', suggestions3) = lintAppend e3
      in (Case e1' e2' (x, y, e3'), suggestions1 ++ suggestions2 ++ suggestions3)
   If e1 e2 e3 ->
      let (e1', suggestions1) = lintAppend e1
          (e2', suggestions2) = lintAppend e2
          (e3', suggestions3) = lintAppend e3
      in (If e1' e2' e3', suggestions1 ++ suggestions2 ++ suggestions3)
   _ -> (expr, [])

--------------------------------------------------------------------------------
-- Composición
--------------------------------------------------------------------------------
-- se aplica en casos de la forma (f (g t)), reemplazando por (f . g) t
-- Construye sugerencias de la forma (LintComp e r)
lintComp :: Linting Expr--f (g (h x))
lintComp expr = case expr of
   App (Var f) (App (Var g) (Var t)) -> (App (Infix Comp (Var f) (Var g)) (Var t), [LintComp expr (App (Infix Comp (Var f) (Var g)) (Var t))])
   App (Var f) (App (Var g) (Lit t)) -> (App (Infix Comp (Var f) (Var g)) (Lit t), [LintComp expr (App (Infix Comp (Var f) (Var g)) (Lit t))])
   App exp1 (App exp2 exp3) ->
      let (exp1', suggestions1) = lintComp exp1
          (exp2', suggestions2) = lintComp exp2
          (exp3', suggestions3) = lintComp exp3
      in (App (Infix Comp exp1' exp2') exp3', suggestions1 ++ suggestions2 ++ suggestions3 ++ [LintComp (App exp1' (App exp2' exp3')) (App (Infix Comp exp1' exp2') exp3')] )
   App e1 e2 ->
      let (e1', suggestions1) = lintComp e1
          (e2', suggestions2) = lintComp e2
      in (App e1' e2', suggestions1 ++ suggestions2)
   Infix op e1 e2 ->
      let (e1', suggestions1) = lintComp e1
          (e2', suggestions2) = lintComp e2
      in (Infix op e1' e2', suggestions1 ++ suggestions2)
   Lam x e ->
      let (e', suggestions) = lintComp e
      in (Lam x e', suggestions)
   Case e1 e2 (x, y, e3) ->
      let (e1', suggestions1) = lintComp e1
          (e2', suggestions2) = lintComp e2
          (e3', suggestions3) = lintComp e3
      in (Case e1' e2' (x, y, e3'), suggestions1 ++ suggestions2 ++ suggestions3)
   If e1 e2 e3 ->
      let (e1', suggestions1) = lintComp e1
          (e2', suggestions2) = lintComp e2
          (e3', suggestions3) = lintComp e3
      in (If e1' e2' e3', suggestions1 ++ suggestions2 ++ suggestions3)
   _ -> (expr, [])


--------------------------------------------------------------------------------
-- Eta Reducción
--------------------------------------------------------------------------------
-- se aplica en casos de la forma \x -> e x, reemplazando por e
-- Construye sugerencias de la forma (LintEta e r)
lintEta :: Linting Expr -- < \x -> (\y -> y + 1) x
lintEta expr = case expr of --hallar forma de llegar al fondo
   Lam x (App e (Var y)) ->
      if x == y && notElem x (freeVariables e)
         then (e, [LintEta expr e])
         else let (e', suggestions) = lintEta e
              in (Lam x (App e' (Var y)), suggestions)
   Infix op e1 e2 ->
      let (e1', suggestions1) = lintEta e1
          (e2', suggestions2) = lintEta e2
      in (Infix op e1' e2', suggestions1 ++ suggestions2)
   App e1 e2 ->
      let (e1', suggestions1) = lintEta e1
          (e2', suggestions2) = lintEta e2
      in (App e1' e2', suggestions1 ++ suggestions2)
   Lam x e ->
      let (e', suggestions) = lintEta e
      in (Lam x e', suggestions)
   Case e1 e2 (x, y, e3) ->
      let (e1', suggestions1) = lintEta e1
          (e2', suggestions2) = lintEta e2
          (e3', suggestions3) = lintEta e3
      in (Case e1' e2' (x, y, e3'), suggestions1 ++ suggestions2 ++ suggestions3)
   If e1 e2 e3 ->
      let (e1', suggestions1) = lintEta e1
          (e2', suggestions2) = lintEta e2
          (e3', suggestions3) = lintEta e3
      in (If e1' e2' e3', suggestions1 ++ suggestions2 ++ suggestions3)
   _ -> (expr, [])

--------------------------------------------------------------------------------
-- Eliminación de recursión con map
--------------------------------------------------------------------------------

-- Sustituye recursión sobre listas por `map`
-- Construye sugerencias de la forma (LintMap f r)
lintMap :: Linting FunDef
lintMap (FunDef name expr) = case expr of
   Lam l (Case (Var l') (Lit LitNil) (x, xs, Infix Cons e (App (Var name') (Var xs'))))
      | l == l' && name == name' && xs == xs' && not (name `elem` freeVariables e || xs `elem` freeVariables e || l `elem` freeVariables e) ->
            let newExpr = App (Var "map") (Lam x e)
            in (FunDef name newExpr, [LintMap (FunDef name expr) (FunDef name newExpr)])
   _ -> (FunDef name expr, [])

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
