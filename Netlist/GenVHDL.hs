module Netlist.GenVHDL(genVHDL) where

import Netlist.AST
import Netlist.Examples
import Operators

import Text.PrettyPrint
import Data.Maybe(catMaybes)



genVHDL :: Module -> Doc
genVHDL m =  entity m $$
             architecture m

entity :: Module -> Doc
entity m = text "entity" <+> text (module_name m) <+> text "is" $$
            nest 2 (text "port" <> parens (vcat $ punctuate semi ports)) $$
            text "end" <+> text "entity" <> semi

  where name = text (module_name m)
        ports = [text i <+> colon <+> text "in" <+> slv_type (s-1) 0 | (i,s) <- module_inputs m ] ++
                [text i <+> colon <+> text "out" <+> slv_type (s-1) 0 | (i,s) <- module_outputs m ]


architecture :: Module -> Doc
architecture m = text "architecture" <+> text "synth" <+> text "of" <+>  text (module_name m) <+> text "is" $$
                 nest 2 (decls (module_decls m)) $$
                 text "begin" $$
                 nest 2 (insts (module_decls m)) $$
                 text "end" <+> text "architecture" <+> text (module_name m) <> semi


decls :: [Decl] -> Doc
decls [] = empty
decls ds = (vcat $ punctuate semi $ catMaybes $ map decl ds) <> semi
decl (NetDecl i size Nothing) = Just $
  text "signal" <+> text i <+> colon <+> slv_type (size - 1) 0

decl (NetDecl i size (Just init)) = Just $
  text "signal" <+> text i <+> colon <+> slv_type (size - 1) 0 <+> text ":=" <+> expr init
decl (MemDecl i size) = Just $
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
inst gensym (ProcessDecl evs) = Just $
    text gensym <+> colon <+> text "process" <> senlist <+> text "is" $$
    text "begin" $$
    nest 2 (pstmts evs) $$
    text "end process" <+> text gensym
  where senlist = parens empty
inst _ (InstDecl nm inst gens ins outs) = Just $
  text inst <+> colon <+> text "entity" <+> text nm $$
       gs $$
       ps
 where
   gs | null gens = empty
      | otherwise =
        text "generic map" <+>
         (parens (cat (punctuate comma  [text i <+> text "=>" <+> expr e | (i,e) <- gens])))
   -- Assume that ports is never null
   ps = text "generic map" <+>
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


pstmts ss = (vcat $ zipWith mkIf is ss) $$ (text "else" <+> text "null") <> semi $$ text "end if"
  where is = (text "if"):(repeat (text "elsif"))
        mkIf i (p,s) = i <+> nest 2 (event p) <+> text "then" $$
                       nest 2 (stmt s)


event (Event i PosEdge) = text "rising_edge" <> parens (text i)
event (Event i NegEdge) = text "falling_edge" <> parens (text i)

stmt (Assign l r) = expr l <+> text "<=" <+> expr r
stmt (Seq ss) = vcat (punctuate semi (map stmt ss)) <> semi
stmt (If e t Nothing) =
  text "if" <+> expr e <+> text "then" $$
  nest 2 (stmt t <> semi) $$
  text "end if" <> semi
stmt (If p t (Just e)) =
  text "if" <+> expr p <+> text "then" $$
  nest 2 (stmt t) $$
  text "else" $$
  nest 2 (stmt e) $$
  text "end if" <> semi



expr (ExprNum Nothing i) = int $ fromIntegral i
expr (ExprNum (Just s) i) =
  text "to_unsigned" <> parens (int s <> comma <> int s)
expr (ExprVar n) = text n
expr (ExprIndex s i) = text s <> parens (expr i)
expr (ExprSlice s h l)
  | h > l = text s <> parens (expr h <+> text "downto" <+> expr l)
  | otherwise = text s <> parens (expr h <+> text "upto" <+> expr l)

expr (ExprConcat ss) = hcat $ punctuate (text "&") (map expr ss)
expr (ExprUnary op e) = lookupUnary op (expr e)
expr (ExprBinary op a b) = lookupBinary op (expr a) (expr b)
expr x = text (show x)


lookupUnary op e = text (unOp op) <> e
unOp UPlus = ""
unOp UMinus = "-"
unOp LNeg = "!"
unOp UAnd = "and"
unOp UNand = "nand"
unOp UOr = "or"
unOp UNor = "nor"
unOp UXor = "xor"
unOp UXnor = "xnor"


-- "(\\(.*\\), text \\(.*\\)),"
lookupBinary op a b = a <+> text (binOp op) <+> b


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



slv_type high low = text "std_logic_vector" <>
                    range high low

range high low = parens (int (fromIntegral high) <+> text "downto" <+> int (fromIntegral low))