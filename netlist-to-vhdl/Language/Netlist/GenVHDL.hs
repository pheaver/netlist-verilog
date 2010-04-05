module Language.Netlist.GenVHDL(genVHDL) where

import Language.Netlist.AST

import Text.PrettyPrint
import Data.Maybe(catMaybes)
import Data.Generics.Schemes
import Data.List(nub, (\\))
genVHDL :: Module -> Doc
genVHDL m =  imports $$
             entity m $$
             architecture m

imports = vcat $ [
          text "library IEEE" <> semi,
          text "use IEEE.STD_LOGIC_1164.ALL" <> semi,
          text "use IEEE.NUMERIC_STD.ALL" <> semi,
          text "use work.all" <> semi
          ]


entity :: Module -> Doc
entity m = text "entity" <+> text (module_name m) <+> text "is" $$
            nest 2 (text "port" <> parens (vcat $ punctuate semi ports) <> semi) $$
            text "end" <+> text "entity" <+> text (module_name m) <> semi

  where name = text (module_name m)
        ports = [text i <+> colon <+> text "in" <+> slv_type ran | (i,ran) <- module_inputs m ] ++
                [text i <+> colon <+> text "out" <+> slv_type ran | (i,ran) <- module_outputs m ]


architecture :: Module -> Doc
architecture m = text "architecture" <+> text "str" <+> text "of" <+>  text (module_name m) <+> text "is" $$
                 utilities $$
                 nest 2 (decls (module_decls m)) $$
                 text "begin" $$
                 nest 2 (insts (module_decls m)) $$
                 text "end" <+> text "architecture" <+> text "str" <> semi

utilities = vcat $ [
 text "function active_high (arg : boolean) return std_ulogic is",
 text "  begin",
 text "  if arg  then return '1';",
 text "    else return '0';",
 text "   end if;",
 text "end function active_high;"
            ]



decls :: [Decl] -> Doc
decls [] = empty
decls ds = (vcat $ punctuate semi $ catMaybes $ map decl ds) <> semi
decl (NetDecl i r Nothing) = Just $
  text "signal" <+> text i <+> colon <+> slv_type r

decl (NetDecl i r (Just init)) = Just $
  text "signal" <+> text i <+> colon <+> slv_type r <+> text ":=" <+> expr init

decl (MemDecl i Nothing dsize) = Just $
    text "signal" <+> text i <+> colon <+> slv_type dsize

decl (MemDecl i asize dsize) = Just $
  text "type" <+> mtype  <+> text "is" <+>
       text "array" <+> text "range" <+> text "of" <+> text "ldots" <> semi $$
  text "signal" <+> text i <+> colon <+> mtype
 where mtype = text i <> text "_memory_type"

decl d = Nothing




insts ::  [Decl] -> Doc
insts [] = empty
insts is = case catMaybes $ zipWith inst gensyms is of
             [] -> empty
             is' -> (vcat $ punctuate semi is') <> semi
  where gensyms = ["proc" ++ show i | i <- [0..]]

inst :: String -> Decl -> Maybe Doc
inst _ (NetAssign i e) = Just $ text i <+> text "<=" <+> expr e

inst gensym proc@(ProcessDecl evs) = Just $
    text gensym <+> colon <+> text "process" <> senlist <+> text "is" $$
    text "begin" $$
    nest 2 (pstmts evs) $$
    text "end process" <+> text gensym
  where senlist = parens $ cat $ punctuate comma $ map expr $   mkSensitivityList proc



inst _ (InstDecl nm inst gens ins outs) = Just $
  text inst <+> colon <+> text "entity" <+> text nm $$
       gs $$
       ps
 where
   gs | null gens = empty
      | otherwise =
        text "port map" <+>
         (parens (cat (punctuate comma  [text i <+> text "=>" <+> expr e | (i,e) <- gens])))
   -- Assume that ports is never null
   ps = text "port map" <+>
         parens (cat (punctuate comma  [text i <+> text "=>" <+> expr e | (i,e) <- (ins ++ outs)]))


inst gensym (InitProcessDecl s) = Just $
    text "-- synthesis_off" $$
    text gensym <+> colon <+> text "process" <> senlist <+> text "is" $$
    text "begin" $$
    nest 2 (stmt s) $$
    text "wait" <> semi $$
    text "end process" <+> text gensym $$
    text "-- synthesis_on"
  where senlist = parens empty


inst _ d = Nothing


pstmts ss = (vcat $ zipWith mkIf is ss)  $$ text "end if" <> semi
  where is = (text "if"):(repeat (text "elsif"))
        mkIf i (p,s) = i <+> nest 2 (event p) <+> text "then" $$
                       nest 2 (stmt s)


event (Event i PosEdge) = text "rising_edge" <> parens (expr i)
event (Event i NegEdge) = text "falling_edge" <> parens (expr i)

stmt (Assign l r) = expr l <+> text "<=" <+> expr r <> semi
stmt (Seq ss) = vcat (map stmt ss)
stmt (If e t Nothing) =
  text "if" <+> expr e <+> text "then" $$
  nest 2 (stmt t) $$
  text "end if" <> semi
stmt (If p t (Just e)) =
  text "if" <+> expr p <+> text "then" $$
  nest 2 (stmt t) $$
  text "else" $$
  nest 2 (stmt e) $$
  text "end if" <> semi
stmt (Case d ps def) =
    text "case" <+> expr d <+> text "of" $$
    vcat (map mkAlt ps) $$
    defDoc $$
    text "end case" <> semi
  where defDoc = maybe empty mkDefault def
        mkDefault s = text "when others =>" $$
                      nest 2 (stmt s)
        mkAlt ([g],s) = text "when" <+> expr g <+> text "=>" $$
                        nest 2 (stmt s)



expr (ExprNum i) = int $ fromIntegral i
expr (ExprBit x) = quotes (int x)
expr (ExprLit 1 val) = quotes (integer val)
expr (ExprLit size val) =
  text "std_logic_vector" <> parens (integer val <> comma <+> int size)
expr (ExprVar n) = text n
expr (ExprIndex s i) = text s <> parens (expr i)
expr (ExprSlice s h l)
  | h >= l = text s <> parens (expr h <+> text "downto" <+> expr l)
  | otherwise = text s <> parens (expr h <+> text "to" <+> expr l)

expr (ExprConcat ss) = hcat $ punctuate (text " & ") (map expr ss)
expr (ExprUnary op e) = lookupUnary op (expr e)
expr (ExprBinary op a b) = lookupBinary op (expr a) (expr b)
expr (ExprFunCall f args) = text f <> parens (cat $ punctuate comma $ map expr args)
expr (ExprCond c t e) = expr t <+> text "when" <+> expr c <+> text "else" $$ expr e
expr (ExprCase _ [] Nothing) = text "0"
expr (ExprCase _ [] (Just e)) = expr e
expr (ExprCase e (([],_):alts) def) = expr (ExprCase e alts def)
expr (ExprCase e ((p:ps,alt):alts) def) = 
	expr (ExprCond (ExprBinary Equals e p) alt (ExprCase e ((ps,alt):alts) def))
expr x = text (show x)


-- | mkSensitivityList takes a process and extracts the appropriate sensitify list
mkSensitivityList  proc@(ProcessDecl evs) = nub labels \\ nub as
  where as = map getLHS $ listify isAssign proc
        isAssign (Assign _ _) = True
        isAssign _ = False
        getLHS (Assign lhs _) = lhs -- Only applied to as

        labels = listify isVar proc

        isVar (ExprVar v) = True
        isVar (ExprIndex _ _) = True
        isVar (ExprSlice _ _ _) = True
        isVar _ = False

lookupUnary op e = text (unOp op) <> parens e
unOp UPlus = ""
unOp UMinus = "-"
unOp LNeg = "not"
unOp UAnd = "and"
unOp UNand = "nand"
unOp UOr = "or"
unOp UNor = "nor"
unOp UXor = "xor"
unOp UXnor = "xnor"
unOp Neg = "-"


-- "(\\(.*\\), text \\(.*\\)),"
lookupBinary op a b = parens $ a <+> text (binOp op) <+> b


binOp Pow = "**"
binOp Plus = "+"
binOp Minus = "-"
binOp Times = "*"
binOp Divide = "/"
binOp Modulo = "mod"
binOp Equals = "="
binOp NotEquals = "!="
binOp CEquals = "="
binOp CNotEquals = "!="
binOp LAnd = "and"
binOp LOr = "or"
binOp LessThan = "<"
binOp LessEqual = "<="
binOp GreaterThan = ">"
binOp GreaterEqual = ">="
binOp And = "and"
binOp Nand = "nand"
binOp Or = "or"
binOp Nor = "nor"
binOp Xor = "xor"
binOp Xnor = "xnor"
binOp ShiftLeft = "sll"
binOp ShiftRight = "shr"
binOp RotateLeft = "rotl"
binOp RotateRight = "rotr"


slv_type Nothing = text "std_logic"
slv_type (Just r) =  text "std_logic_vector" <> range r


range (Range high low) = parens (expr high <+> text "downto" <+> expr low)