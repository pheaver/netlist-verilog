--------------------------------------------------------------------------------
-- |
-- Module       :  Language.Netlist.GenVerilog
-- Copyright    :  (c) Signali Corp. 2010
-- License      :  All rights reserved
--
-- Maintainer   : pweaver@signalicorp.com
-- Stability    : experimental
-- Portability  : non-portable (DeriveDataTypeable)
--
-- Translate a Netlist AST into a Verilog AST.
--
-- The @netlist@ package defines the Netlist AST and the @verilog@ package
-- defines the Verilog AST.
--
-- Not every Netlist AST is compatible with Verilog.  For example, the Netlist
-- AST permits left- and right-rotate operators, which are not supported in
-- Verilog.
--------------------------------------------------------------------------------

-- TODO: endianness - currently we're hardcoded to little endian verilog

module Language.Netlist.GenVerilog ( mk_module
                                   , mk_decl
                                   , mk_stmt
                                   , mk_expr
                                   ) where

import Numeric          ( showIntAtBase )

import Language.Netlist.AST
import qualified Language.Verilog.Syntax as V

-- -----------------------------------------------------------------------------

mk_module :: Module -> V.Module
mk_module (Module name ins outs statics decls)
  = V.Module (mk_ident name) ports items
  where
    params= [ V.ParamDeclItem (V.ParamDecl [V.ParamAssign (mk_ident x) (mk_expr expr)])
              | (x, expr) <- statics
            ]
    ports = map (mk_ident . fst) ins ++ map (mk_ident . fst) outs
    items = [ V.InputDeclItem (V.InputDecl (fmap mk_range mb_range) [mk_ident x])
              | (x, mb_range) <- ins ] ++

            [ V.OutputDeclItem (V.OutputDecl (fmap mk_range mb_range) [mk_ident x])
              | (x, mb_range) <- outs ] ++

            params ++
            concatMap mk_decl decls


mk_decl :: Decl -> [V.Item]
mk_decl (NetDecl x mb_range mb_expr)
  = [V.NetDeclItem decl]
  where
    mb_range' = fmap (V.SimpleRange . mk_range) mb_range
    decl = case mb_expr of
             Nothing   -> V.NetDecl "wire" mb_range' Nothing [mk_ident x]
             Just expr -> V.NetDeclAssign "wire" Nothing mb_range' Nothing [mkAssign x expr]

mk_decl (NetAssign x expr)
  = [V.AssignItem Nothing Nothing [mkAssign x expr]]

mk_decl (MemDecl x mb_range1 mb_range2)
  = [V.RegDeclItem (V.RegDecl (fmap mk_range mb_range2)
                    [V.RegVar (mk_ident x) (fmap mk_range mb_range1)])]

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

mk_range :: Range -> V.Range
mk_range (Range e1 e2)
  = V.Range (mk_expr e1) (mk_expr e2)

mk_process_stmt :: [(Event, Stmt)] -> V.Statement
mk_process_stmt []
  = error "mk_process_stmt: empty list"
mk_process_stmt [(_, stmt)]
  -- if this is the last one, then we don't have to check the event condition
  = mk_stmt stmt
mk_process_stmt ((Event e edge, stmt):xs)
  = V.IfStmt cond (Just (mk_stmt stmt)) (Just (mk_process_stmt xs))
  where
    cond = case edge of
             PosEdge -> (mk_expr e)
             NegEdge -> V.ExprUnary V.UBang (mk_expr e)

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
              PosEdge -> V.EventPosedge (mk_expr x)
              NegEdge -> V.EventNegedge (mk_expr x)
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
mk_stmt (FunCallStmt x es)
  | head x == '$'
  = V.TaskStmt (mk_ident (tail x)) (Just (map mk_expr es))
  | otherwise
  = error ("FunCallStmt " ++ x)

mk_expr :: Expr -> V.Expression
mk_expr (ExprNum x)
  = V.intExpr x
mk_expr (ExprLit sz x)
  = V.ExprNum (V.IntNum Nothing (Just (show sz)) (Just base) str)
  where
    str = showIntAtBase base_int (hexdigits !!) x ""
    -- we show everything in hexadecimal, except 1-bit values
    (base_int, base) = if sz == 1 then (2, V.BinBase) else (16, V.HexBase)
    hexdigits = "0123456789abcdef"

mk_expr (ExprBit x)
  = V.ExprNum (V.IntNum Nothing (Just "1") (Just V.BinBase) (show x))
mk_expr (ExprString x)
  = V.ExprString x
mk_expr (ExprVar x)
  = expr_var x
mk_expr (ExprIndex x e)
  = V.ExprIndex (mk_ident x) (mk_expr e)
mk_expr (ExprSlice x e1 e2)
  = V.ExprSlice (mk_ident x) (mk_expr e1) (mk_expr e2)
mk_expr (ExprSliceOff x e i)
  = f (mk_ident x) (mk_expr e) (V.intExpr (abs (fromIntegral i)))
  where
    f = if i < 0 then V.ExprSliceMinus else V.ExprSlicePlus
mk_expr (ExprConcat exprs)
  = V.ExprConcat (map mk_expr exprs)
mk_expr (ExprUnary op expr)
  = V.ExprUnary (unary_op op) (mk_expr expr)
mk_expr (ExprBinary op expr1 expr2)
  = V.ExprBinary (binary_op op) (mk_expr expr1) (mk_expr expr2)
mk_expr (ExprCond expr1 expr2 expr3)
  = V.ExprCond (mk_expr expr1) (mk_expr expr2) (mk_expr expr3)
mk_expr (ExprFunCall x es)
  = V.ExprFunCall (mk_ident x) (map mk_expr es)

mk_ident :: Ident -> V.Ident
mk_ident x = V.Ident x

expr_var :: Ident -> V.Expression
expr_var x = V.ExprVar (mk_ident x)

mkAssign :: Ident -> Expr -> V.Assignment
mkAssign ident expr
  = V.Assignment (expr_var ident) (mk_expr expr)

unary_op :: UnaryOp -> V.UnaryOp
unary_op UPlus  = V.UPlus
unary_op UMinus = V.UMinus
unary_op LNeg   = V.UBang
unary_op Neg    = V.UTilde
unary_op UAnd   = V.UAnd
unary_op UNand  = V.UNand
unary_op UOr    = V.UOr
unary_op UNor   = V.UNor
unary_op UXor   = V.UXor
unary_op UXnor  = V.UXnor

binary_op :: BinaryOp -> V.BinaryOp
binary_op Pow          = V.Pow
binary_op Plus         = V.Plus
binary_op Minus        = V.Minus
binary_op Times        = V.Times
binary_op Divide       = V.Divide
binary_op Modulo       = V.Modulo
binary_op Equals       = V.Equals
binary_op NotEquals    = V.NotEquals
binary_op CEquals      = V.CEquals
binary_op CNotEquals   = V.CNotEquals
binary_op LAnd         = V.LAnd
binary_op LOr          = V.LOr
binary_op LessThan     = V.LessThan
binary_op LessEqual    = V.LessEqual
binary_op GreaterThan  = V.GreaterThan
binary_op GreaterEqual = V.GreaterEqual
binary_op And          = V.And
binary_op Nand         = V.Nand
binary_op Or           = V.Or
binary_op Nor          = V.Nor
binary_op Xor          = V.Xor
binary_op Xnor         = V.Xnor
binary_op ShiftLeft    = V.ShiftLeft
binary_op ShiftRight   = V.ShiftRight
binary_op RotateLeft   = error "GenVerilog: no left-rotate operator in Verilog"
binary_op RotateRight  = error "GenVerilog: no right-rotate operator in Verilog"

-- -----------------------------------------------------------------------------
