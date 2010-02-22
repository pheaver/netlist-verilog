-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
--
-- Convert a Netlist AST to Verilog.
--
-- TODO: endianness - currently we're hardcoded to little endian verilog
-- -----------------------------------------------------------------------------

module Netlist.GenVerilog ( mk_module) where

import Netlist.AST
--import Operators
import qualified Language.Verilog.AST as V

-- -----------------------------------------------------------------------------

mk_module :: Module -> V.Module
mk_module (Module name ins outs decls)
  = V.Module (mk_ident name) ports items
  where
    ports = map (mk_ident . fst) ins ++ map (mk_ident . fst) outs
    items = [ V.InputDeclItem (V.InputDecl (size2MaybeRange sz) [mk_ident x])
              | (x, sz) <- ins ] ++

            [ V.OutputDeclItem (V.OutputDecl (size2MaybeRange sz) [mk_ident x])
              | (x, sz) <- outs ] ++

            concatMap mk_decl decls
             

mk_decl :: Decl -> [V.Item]
mk_decl (NetDecl x sz mb_expr)
  = [V.NetDeclItem (V.NetDeclAssign "wire" Nothing mb_range Nothing assigns)]
  where
    mb_range = fmap V.SimpleRange (size2MaybeRange sz)
    assigns  = case mb_expr of
                Just expr -> [mkAssign x expr]
                Nothing   -> []

mk_decl (NetAssign x expr)
  = [V.AssignItem Nothing Nothing [mkAssign x expr]]

mk_decl (MemDecl x sz)
  = [V.RegDeclItem (V.RegDecl (size2MaybeRange sz)
                    [V.RegVar (mk_ident x) Nothing])]

mk_decl (InstDecl mod_name inst_name params inputs outputs)
  = [V.ModuleInstItem (V.ModuleInst (mk_ident mod_name) v_params [inst])]
  where
    v_params  = [ V.Parameter (mk_ident x) (mk_expr expr) | (x, expr) <- params ]
    inst      = V.Instance (mk_ident inst_name) Nothing (V.NamedConnections cs)
    cs        = [ V.NamedConnection (mk_ident x) (mk_expr expr)
                  | (x, expr) <- inputs ++ outputs ]

mk_decl (InitProcessDecl stmt)
  = [V.InitialItem (mk_stmt stmt)]

-- nothing to do here
mk_decl (ProcessDecl [])
  = []

mk_decl (ProcessDecl xs)
  = [V.AlwaysItem (V.DelayOrEventControlStmt e (Just s))]
  where
    s = mk_process_stmt xs
    e = V.EventControl (mk_trigger (map fst xs))

mk_process_stmt :: [(Event, Stmt)] -> V.Statement
mk_process_stmt []
  = error "mk_process_stmt: empty list"
mk_process_stmt [(_, stmt)]
  -- if this is the last one, then we don't have to check the event condition
  = mk_stmt stmt
mk_process_stmt ((Event ident edge, stmt):xs)
  = V.IfStmt cond (Just (mk_stmt stmt)) (Just (mk_process_stmt xs))
  where
    cond = case edge of
             PosEdge -> expr_var ident
             NegEdge -> V.ExprUnary LNeg (expr_var ident)

-- create a Verilog event expression from a list of triggers.
-- the list must have at least one 'Event' field in it.
mk_trigger :: [Event] -> V.EventExpr
mk_trigger []
  = error "mk_trigger: empty event list"
mk_trigger xs0
  = foldr1 V.EventOr (f xs0)
  where
    f []                  = []
    f (Event x edge : xs) = e : f xs
      where
        e = case edge of
              PosEdge -> V.EventPosedge (expr_var x)
              NegEdge -> V.EventNegedge (expr_var x)
              -- AnyEdge -> V.EventExpr (expr_var x)
                         -- V.EventOr (V.EventPosedge (expr_var x)) (V.EventNegedge (expr_var x))
                         -- is this the right thing to do?

mk_stmt :: Stmt -> V.Statement
mk_stmt (Assign x expr)
  = V.NonBlockingAssignment (mk_expr x) Nothing (mk_expr expr)
mk_stmt (If cond s1 mb_s2)
  = V.IfStmt (mk_expr cond) (Just (mk_stmt s1)) (fmap mk_stmt mb_s2)
mk_stmt (Case e case_items mb_default)
  = V.CaseStmt (mk_expr e) $
    [ V.CaseItem (map mk_expr es) (Just (mk_stmt stmt))
      | (es, stmt) <- case_items ]
    ++
    case mb_default of
      Just stmt -> [V.CaseDefault (Just (mk_stmt stmt))]
      Nothing   -> []

mk_stmt (Seq stmts)
  = V.SeqBlock Nothing [] (map mk_stmt stmts)

mk_expr :: Expr -> V.Expression
mk_expr (ExprNum Nothing x)
  = V.ExprNum x
mk_expr (ExprNum (Just sz) x)
  = V.ExprSizedNum sz x
mk_expr (ExprVar x)
  = expr_var x
mk_expr (ExprIndex x e)
  = V.ExprIndex (mk_ident x) (mk_expr e)
mk_expr (ExprSlice x e1 e2)
  = V.ExprSlice (mk_ident x) (mk_expr e1) (mk_expr e2)
mk_expr (ExprSliceOff x e i)
  = f (mk_ident x) (mk_expr e) (V.ExprNum (abs (fromIntegral i)))
  where
    f = if i < 0 then V.ExprSliceMinus else V.ExprSlicePlus
mk_expr (ExprConcat exprs)
  = V.ExprConcat (map mk_expr exprs)
-- TODO: in ExprUnary and ExprBinary, check that the op is valid in Verilog
mk_expr (ExprUnary op expr)
  = V.ExprUnary op (mk_expr expr)
mk_expr (ExprBinary op expr1 expr2)
  = V.ExprBinary op (mk_expr expr1) (mk_expr expr2)
mk_expr (ExprCond expr1 expr2 expr3)
  = V.ExprCond (mk_expr expr1) (mk_expr expr2) (mk_expr expr3)
mk_expr (ExprFunCall x es)
  = V.ExprFunCall (mk_ident x) (map mk_expr es)

mk_ident :: Ident -> V.Ident
mk_ident x = V.Ident x

expr_var :: Ident -> V.Expression
expr_var x = V.ExprVar (mk_ident x)

-- -----------------------------------------------------------------------------

size2MaybeRange :: Size -> Maybe V.Range
size2MaybeRange 1 = Nothing
size2MaybeRange sz
  | sz > 1 
  = Just (V.Range (V.ExprNum (fromIntegral (sz - 1))) (V.ExprNum 0))
  | otherwise
  = error ("size2MaybeRange: invalid size: " ++ show sz)

mkAssign :: Ident -> Expr -> V.Assignment
mkAssign ident expr
  = V.Assignment (expr_var ident) (mk_expr expr)

-- -----------------------------------------------------------------------------
