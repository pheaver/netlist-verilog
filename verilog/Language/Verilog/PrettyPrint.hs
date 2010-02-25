-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
--
-- A pretty-printer for the Verilog AST.
-- -----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Verilog.PrettyPrint where

import Data.Maybe               ( fromMaybe )
import Numeric                  ( showIntAtBase )
import Text.PrettyPrint

import Language.Verilog.AST

-- -----------------------------------------------------------------------------
-- some utilities, which should go in a common module elsewhere

commasep :: [Doc] -> Doc
commasep = fsep . punctuate comma

mb :: (x -> Doc) -> Maybe x -> Doc
mb = maybe empty

period :: Doc
period = char '.'

tick :: Doc
tick = char '\''

-- -----------------------------------------------------------------------------
-- 1. Source Text

ppDescription :: Description -> Doc
ppDescription = ppModule

ppModule :: Description -> Doc
ppModule (Module name ports body)
  = text "module" <+> ppIdent name <+> parens (ppIdents ports) <> semi $$
    nest 2 (vcat (map ppItem body)) $$
    text "endmodule" <> char '\n'

ppItem :: Item -> Doc
ppItem (ParamDeclItem x)   = ppParamDecl x
ppItem (InputDeclItem x)   = ppInputDecl x
ppItem (OutputDeclItem x)  = ppOutputDecl x
ppItem (InOutDeclItem x)   = ppInOutDecl x
ppItem (NetDeclItem x)     = ppNetDecl x
ppItem (RegDeclItem x)     = ppRegDecl x
ppItem (TimeDeclItem x)    = ppTimeDecl x
ppItem (IntegerDeclItem x) = ppIntegerDecl x
ppItem (RealDeclItem x)    = ppRealDecl x
ppItem (EventDeclItem x)   = ppEventDecl x
ppItem (ModuleInstItem x)  = ppModuleInst x
ppItem (AssignItem mb_strength mb_delay assignments)
  = text "assign" <+>
    mb ppDriveStrength mb_strength <+>
    mb ppDelay mb_delay <+>
    commasep (map ppAssignment assignments) <> semi
ppItem (InitialItem (DelayOrEventControlStmt ctrl stmt))
  = fsep [ text "initial", ppDelayOrEventControl ctrl, nest 2 (maybe semi ppStatement stmt) ]
ppItem (InitialItem stmt)
  = fsep [ text "initial", nest 2 (ppStatement stmt) ]
ppItem (AlwaysItem (DelayOrEventControlStmt ctrl stmt))
  = fsep [ text "always", ppDelayOrEventControl ctrl, nest 2 (maybe semi ppStatement stmt) ]
ppItem (AlwaysItem stmt)
  = fsep [ text "always", nest 2 (ppStatement stmt) ]

-- -----------------------------------------------------------------------------
-- 2. Declarations

ppParamDecl :: ParamDecl -> Doc
ppParamDecl (ParamDecl paramAssigns)
  = text "parameter" <+> commasep (map ppParamAssign paramAssigns) <> semi

ppInputDecl :: InputDecl -> Doc
ppInputDecl (InputDecl mb_range vars)
  = text "input" <+> mb ppRange mb_range <+> ppIdents vars <> semi

ppOutputDecl :: OutputDecl -> Doc
ppOutputDecl (OutputDecl mb_range vars)
  = text "output" <+> mb ppRange mb_range <+> ppIdents vars <> semi

ppInOutDecl :: InOutDecl -> Doc
ppInOutDecl (InOutDecl mb_range vars)
  = text "inout" <+> mb ppRange mb_range <+> ppIdents vars <> semi

ppNetDecl :: NetDecl -> Doc
ppNetDecl (NetDecl t mb_range mb_delay vars)
  = text t <+>
    mb ppExpandRange mb_range <+>
    mb ppDelay mb_delay <+>
    ppIdents vars <> semi
ppNetDecl (NetDeclAssign t mb_strength mb_range mb_delay assignments)
  = text t <+>
    mb ppDriveStrength mb_strength <+>
    mb ppExpandRange mb_range <+>
    mb ppDelay mb_delay <+>
    commasep (map ppAssignment assignments) <> semi

ppRegDecl :: RegDecl -> Doc
ppRegDecl (RegDecl mb_range vars)
  = text "reg" <+> mb ppRange mb_range <+> ppRegVars vars <> semi

ppTimeDecl :: TimeDecl -> Doc
ppTimeDecl (TimeDecl vars)
  = text "time" <+> ppRegVars vars <> semi

ppIntegerDecl :: IntegerDecl -> Doc
ppIntegerDecl (IntegerDecl vars)
  = text "integer" <+> ppRegVars vars <> semi

ppRealDecl :: RealDecl -> Doc
ppRealDecl (RealDecl vars)
  = text "real" <+> ppIdents vars <> semi

ppEventDecl :: EventDecl -> Doc
ppEventDecl (EventDecl vars)
  = text "event" <+> ppIdents vars <> semi

-- -----------------------------------------------------------------------------
-- 3. Primitive Instances

-- -----------------------------------------------------------------------------
-- 4. Module Instantiations

ppModuleInst :: ModuleInst -> Doc
ppModuleInst (ModuleInst name params insts)
  = ppIdent name<+> ppParameters params $$
    nest 2 (ppInstances insts) <> semi

ppParameters :: [Parameter] -> Doc
ppParameters [] = empty
ppParameters ps
  = char '#' <> parens (commasep (map ppParameter ps))

ppParameter :: Parameter -> Doc
ppParameter (Parameter x expr)
  = period <> ppIdent x <> parens (ppExpr expr)

ppInstances :: [Instance] -> Doc
ppInstances insts
  = vcat (punctuate comma (map ppInstance insts))

ppInstance :: Instance -> Doc
ppInstance (Instance x r cs)
  = ppIdent x <> mb ppRange r <> parens (commasep ppCs)
  where
    ppCs = case cs of
             Connections exprs    -> map ppExpr exprs
             NamedConnections ncs -> map ppNamedConnection ncs

-- this is used for both port connections and parameter assignments
ppNamedConnection :: NamedConnection -> Doc
ppNamedConnection (NamedConnection x expr)
  = period <> ppIdent x <> parens (ppExpr expr)

-- ----------------------------------------------------------------------------
-- 5. Behavioral Statements

ppStatement :: Statement -> Doc
ppStatement (BlockingAssignment x ctrl expr)
  = ppLValue x <+> equals <+> mb ppDelayOrEventControl ctrl <+> ppExpr expr <> semi
ppStatement (NonBlockingAssignment x ctrl expr)
  = ppLValue x <+> text "<=" <+> mb ppDelayOrEventControl ctrl <+> ppExpr expr <> semi

-- we have to add a begin-end pair in order to avoid ambiguity, otherwise in the
-- concrete syntax the else-branch (if2) will be associated with if1 instead of
-- the outer if-statement.
ppStatement (IfStmt expr (Just if1@IfStmt {}) (Just if2@IfStmt {}))
  = ppStatement (IfStmt expr (Just if1') (Just if2))
  where
    if1' = SeqBlock Nothing [] [if1]
ppStatement (IfStmt expr stmt1 stmt2)
  = (text "if" <+> parens (ppExpr expr)) `nestStmt` (maybe semi ppStatement stmt1) $$
    case stmt2 of
      Just stmt -> ppElseBranch stmt
      Nothing   -> empty
  where
    ppElseBranch (IfStmt e s1 s2)
      = (text "else if" <+> parens (ppExpr e)) `nestStmt` (maybe semi ppStatement s1) $$
        case s2 of
          Just s  -> ppElseBranch s
          Nothing -> empty
    ppElseBranch s
      = text "else" `nestStmt` ppStatement s

ppStatement (CaseStmt expr case_items)
  = text "case" <+> parens (ppExpr expr) $$
    vcat (map ppCaseItem case_items) $$
    text "endcase"
ppStatement (ForeverStmt stmt)
  = text "forever" `nestStmt` ppStatement stmt
ppStatement (RepeatStmt expr stmt)
  = (text "repeat" <+> parens (ppExpr expr)) `nestStmt` ppStatement stmt
ppStatement (WhileStmt expr stmt)
  = (text "while" <+> parens (ppExpr expr)) `nestStmt` ppStatement stmt
ppStatement (ForStmt init_assign expr_cond loop_assign stmt)
  = x `nestStmt` ppStatement stmt
  where
    x = text "for" <+> parens (ppAssignment init_assign <> semi <+>
                               ppExpr expr_cond <> semi <+>
                               ppAssignment loop_assign)
ppStatement (DelayOrEventControlStmt ctrl stmt)
  = ppDelayOrEventControl ctrl `nestStmt` maybe semi ppStatement stmt
ppStatement (WaitStmt expr stmt)
  = (text "wait" <+> parens (ppExpr expr)) `nestStmt` maybe semi ppStatement stmt
ppStatement (SeqBlock mb_name decls stmts)
  = text "begin" <+> x $$
    nest 2 (vcat (map ppBlockDecl decls ++ map ppStatement stmts)) $$
    text "end"
  where x = case mb_name of
              Just name -> colon <+> ppIdent name
              Nothing   -> empty
ppStatement (ParBlock mb_name decls stmts)
  = text "fork" <+> x $$
    nest 2 (vcat (map ppBlockDecl decls ++ map ppStatement stmts)) $$
    text "join"
  where x = case mb_name of
              Just name -> colon <+> ppIdent name
              Nothing   -> empty
ppStatement (TaskStmt x mb_es)
  = char '$' <> ppIdent x <> maybe empty (parens . commasep . map ppExpr) mb_es <> semi
ppStatement (AssignStmt assignment)
  = text "assign" <+> ppAssignment assignment <> semi
ppStatement (DeAssignStmt x)
  = text "deassign" <+> ppLValue x <> semi
ppStatement (ForceStmt assignment)
  = text "force" <+> ppAssignment assignment <> semi
ppStatement (ReleaseStmt x)
  = text "release" <+> ppLValue x <> semi

-- a helper for pretty-printing statement.  'fsep' chooses whether to put the
-- statement on the same line as 'x', or nest it on the next line if it doesn't
-- fit on the same line.
nestStmt :: Doc -> Doc -> Doc
nestStmt x stmt
  = fsep [x, nest 2 stmt ]

ppAssignment :: Assignment -> Doc
ppAssignment (Assignment x expr)
  = ppLValue x <+> equals <+> ppExpr expr

ppCaseItem :: CaseItem -> Doc
ppCaseItem (CaseItem es mb_stmt)
  = fsep [ commasep (map ppExpr es), maybe semi ppStatement mb_stmt ]
ppCaseItem (CaseDefault mb_stmt)
  = fsep [ text "default" <+> colon, maybe semi ppStatement mb_stmt ]

ppBlockDecl :: BlockDecl -> Doc
ppBlockDecl (ParamDeclBlock x)   = ppParamDecl x
ppBlockDecl (RegDeclBlock x)     = ppRegDecl x
ppBlockDecl (IntegerDeclBlock x) = ppIntegerDecl x
ppBlockDecl (RealDeclBlock x)    = ppRealDecl x
ppBlockDecl (TimeDeclBlock x)    = ppTimeDecl x
ppBlockDecl (EventDeclBlock x)   = ppEventDecl x

-- -----------------------------------------------------------------------------
-- 7. Expressions

ppLValue :: LValue -> Doc
ppLValue = ppExpr

ppExpr :: Expression -> Doc
ppExpr = ppExpr' 0

-- precedence-aware expression pretty printer - adds parens when it needs to
ppExpr' :: Int -> Expression -> Doc
ppExpr' _ (ExprNum x)
  = integer x
ppExpr' _ (ExprLit sz x)
  = int sz <> tick <> char base_char <> text str
  where
    str = showIntAtBase base (hexdigits !!) x ""
    -- we show everything in hexadecimal, except 1-bit values
    (base, base_char) = if sz == 1 then (2, 'b') else (16, 'h')
    hexdigits = "0123456789abcdef"

ppExpr' _ (ExprVar x)
  = ppIdent x
ppExpr' _ (ExprString x)
  = doubleQuotes (text x)
ppExpr' _ (ExprIndex x expr)
  = ppIdent x <> brackets (ppExpr expr)
ppExpr' _ (ExprSlice x e1 e2)
  = ppIdent x <> brackets (ppExpr e1 <> colon <> ppExpr e2)
ppExpr' _ (ExprSlicePlus x e1 e2)
  = ppIdent x <> brackets (ppExpr e1 <> text "+:" <> ppExpr e2)
ppExpr' _ (ExprSliceMinus x e1 e2)
  = ppIdent x <> brackets (ppExpr e1 <> text "-:" <> ppExpr e2)
ppExpr' _ (ExprConcat es)
  = braces (commasep (map ppExpr es))
ppExpr' _ (ExprMultiConcat e es)
  = braces (ppExpr e <> braces (commasep (map ppExpr es)))
ppExpr' prec (ExprUnary op expr)
  = if prec >= unary_prec then parens e else e
   where
    e = text x <> ppExpr' unary_prec expr
    x = lookupOp op unary_op_table
ppExpr' prec (ExprBinary op expr1 expr2)
  = if prec > op_prec then parens e else e
  where
    e = fsep [ppExpr' op_prec expr1, text x, ppExpr' (op_prec + 1) expr2 ]
    (x, op_prec) = lookupOp op binary_op_table
ppExpr' prec (ExprCond e1 e2 e3)
  = if prec > cond_prec then parens e else e
  where
    e = ppExpr e1 <+> char '?' <+> ppExpr e2 <+> colon <+> ppExpr e3
ppExpr' _ (ExprFunCall x es)
  = ppIdent x <+> parens (commasep (map ppExpr es))

cond_prec, unary_prec :: Int
cond_prec = 1
unary_prec = 11

lookupOp :: (Eq op, Show op) => op -> [(op, x)] -> x
lookupOp op table
  = fromMaybe (error msg) (lookup op table)
  where msg = "showOp: cannot find operator: " ++ show op

-- precedence tables, also for showing.
-- these tables could also be used for parsing operators.
unary_op_table :: [(UnaryOp, String)]
unary_op_table
  = [ (UPlus, "+"), (UMinus, "-"), (UBang, "!"), (UTilde, "~")
    , (UAnd, "&"), (UNand, "~&"), (UOr, "|"), (UNor, "~|")
    , (UXor, "^"), (UXnor, "~^"), (UXnor, "^~")
    ]

binary_op_table :: [(BinaryOp, (String, Int))]
binary_op_table
  = [ (LOr, ("||", 2))
    , (LAnd, ("&&", 3))
    , (Or, ("|", 4)), (Nor, ("~|", 4))
    , (And, ("&", 5)), (Nand, ("~&", 5)), (Xor, ("^", 5)), (Xnor, ("^~", 5)), (Xnor, ("~^", 5))
    , (Equals, ("==", 6)), (NotEquals, ("!=", 6)), (CEquals, ("===", 6)), (CNotEquals, ("!==", 6))
    , (LessThan, ("<", 7)), (LessEqual, ("<=", 7)), (GreaterThan, (">", 7)), (GreaterEqual, (">=", 7))
    , (ShiftLeft, ("<<", 8)), (ShiftRight, (">>", 8))
    , (Plus, ("+", 9)), (Minus, ("-", 9))
    , (Times, ("*", 10)), (Divide, ("/", 10)), (Modulo, ("%", 10))
    ]

-- -----------------------------------------------------------------------------
-- Miscellaneous

ppParamAssign :: ParamAssign -> Doc
ppParamAssign (ParamAssign ident expr)
  = ppIdent ident <+> equals <+> ppExpr expr

ppExpandRange :: ExpandRange -> Doc
ppExpandRange (SimpleRange r)
  = ppRange r
ppExpandRange (ScalaredRange r)
  = text "scalared" <+> ppRange r
ppExpandRange (VectoredRange r)
  = text "vectored" <+> ppRange r

ppRange :: Range -> Doc
ppRange (Range e1 e2)
  = brackets (ppExpr e1 <> colon <> ppExpr e2)

ppDelayOrEventControl :: DelayOrEventControl -> Doc
ppDelayOrEventControl (DelayControl x)
  = ppDelayControl x
ppDelayOrEventControl (EventControl x)
  = ppEventControl x
ppDelayOrEventControl (RepeatControl expr x)
  = text "repeat" <+> parens (ppExpr expr) <+> ppEventControl x

ppDelayControl :: DelayControl -> Doc
ppDelayControl = ppDelay

ppEventControl :: EventControl -> Doc
ppEventControl x = char '@' <> parens (ppEventExpr x)

ppDelay :: Delay -> Doc
ppDelay x = char '#' <> ppExpr x

ppEventExpr :: EventExpr -> Doc
ppEventExpr (EventExpr expr)
  = ppExpr expr
ppEventExpr (EventPosedge expr)
  = text "posedge" <+> ppExpr expr
ppEventExpr (EventNegedge expr)
  = text "negedge" <+> ppExpr expr
ppEventExpr (EventOr expr1 expr2)
  = ppEventExpr expr1 <+> text "or" <+> ppEventExpr expr2

-- TODO: check if the string is a valid Verilog identifier.
--       throw error, or convert it into a valid identifier.
ppIdent :: Ident -> Doc
ppIdent (Ident x) = text x

ppIdents :: [Ident] -> Doc
ppIdents = commasep . map ppIdent

ppRegVar :: RegVar -> Doc
ppRegVar (RegVar x r)
  = ppIdent x <> mb ppRange r

ppRegVars :: [RegVar] -> Doc
ppRegVars = commasep . map ppRegVar

ppDriveStrength :: DriveStrength -> Doc
ppDriveStrength (Strength01 s0 s1)
  = parens (ppStrength0 s0 <> comma <+> ppStrength1 s1)
ppDriveStrength (Strength10 s1 s0)
  = parens (ppStrength1 s1 <> comma <+> ppStrength0 s0)

ppStrength0 :: Strength0 -> Doc
ppStrength0 = text

ppStrength1 :: Strength1 -> Doc
ppStrength1 = text

-- -----------------------------------------------------------------------------
