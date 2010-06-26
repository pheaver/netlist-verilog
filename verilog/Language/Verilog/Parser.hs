--------------------------------------------------------------------------------
-- |
-- Module       :  Language.Verilog.Parser
-- Copyright    :  (c) Signali Corp. 2010
-- License      :  All rights reserved
--
-- Maintainer   : philip.weaver@gmail.com
-- Stability    : experimental
-- Portability  : ghc
--
-- A parser for the Verilog AST.  The following sources were used to define the
-- AST and the parser:
--
--  * <http://www.verilog.com/VerilogBNF.html>
--
--  * <http://www.hdlworks.com/hdl_corner/verilog_ref/index.html>
--
--  * <http://en.wikipedia.org/wiki/Verilog>
--
-- The specifications at the first two links contradict each other in several
-- places.  When in doubt, we try to make this parser match Icarus Verilog.
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Verilog.Parser where

import Control.Monad            ( liftM, liftM2 )
import Control.Monad.Identity   ( Identity )
import Data.Either              ( partitionEithers)
import Data.Maybe               ( fromMaybe )

import Text.Parsec
import Text.Parsec.Expr hiding (Operator)
import qualified Text.Parsec.Expr as E
import Text.Parsec.Token hiding (GenTokenParser(..))
import qualified Text.Parsec.Token as T

import Language.Verilog.Syntax

-- --------------------

type P s a = ParsecT s () Identity a

type Operator s a = E.Operator s () Identity a

type OpTable s a = [[Operator s a]]

-- --------------------

verilog :: Stream s Identity Char => T.GenTokenParser s () Identity
verilog = makeTokenParser verilogDef

verilogDef :: Stream s Identity Char => GenLanguageDef s () Identity
verilogDef
  = LanguageDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , nestedComments  = True
    , identStart      = letter <|> char '_'
    , identLetter     = alphaNum <|> oneOf "_$"
    , opStart         = oneOf "+-!~&|^*/%><=?:"
    , opLetter        = oneOf "+-!~&|^*/%><=?:"
    , reservedNames   = verilogKeywords
    , reservedOpNames = verilogOps
    , caseSensitive   = True
    }

lexeme = T.lexeme verilog
lexeme :: (Stream s Identity Char) => P s a -> P s a

whiteSpace :: Stream s Identity Char => P s ()
whiteSpace = T.whiteSpace verilog

identifier :: Stream s Identity Char => P s String
identifier = T.identifier verilog

reserved :: Stream s Identity Char => String -> P s ()
reserved = T.reserved verilog

reservedOp :: Stream s Identity Char => String -> P s ()
reservedOp = T.reservedOp verilog

symbol :: Stream s Identity Char => String -> P s ()
symbol x = T.symbol verilog x >> return ()

stringLiteral :: Stream s Identity Char => P s String
stringLiteral = T.stringLiteral verilog

-- integer :: P s Integer
-- integer = T.integer verilog

parens, brackets, braces :: Stream s Identity Char => P s a -> P s a
parens     = T.parens verilog
brackets   = T.brackets verilog
braces     = T.braces verilog

comma, semi, colon, dot :: Stream s Identity Char => P s ()
comma      = T.comma verilog  >> return ()
semi       = T.semi verilog   >> return ()
colon      = T.colon verilog  >> return ()
dot        = T.dot verilog    >> return ()

commaSep, commaSep1 :: Stream s Identity Char => P s a -> P s [a]
commaSep   = T.commaSep verilog
commaSep1  = T.commaSep1 verilog

-- http://www.hdlworks.com/hdl_corner/verilog_ref/index.html
verilogKeywords :: [String]
verilogKeywords
  = [ "always", "and", "assign", "begin", "buf", "bufif0", "bufif1"
    , "case", "casex", "casez", "cmos"
    , "deassign", "default", "defparam", "disable"
    , "edge", "else", "end", "endcase", "endfunction", "endmodule"
    , "endprimitive", "endspecify", "endtable", "endtask", "event"
    , "for", "force", "forever", "fork", "function"
    , "highz0", "highz1", "if", "ifnone", "initial", "inout", "input", "integer"
    , "join", "large", "macromodule", "medium", "module"
    , "nand", "negedge", "nmos", "nor", "not", "notif0", "notif1"
    , "or", "output", "parameter", "pmos", "posedge", "primitive"
    , "pull0", "pull1", "pulldown", "pullup"
    , "rcmos", "real", "realtime", "reg", "release", "repeat"
    , "rnmos", "rpmos", "rtran", "rtranif0", "rtranif1"
    , "scalared", "small", "specify", "specparam"
    , "strong0", "strong1", "supply0", "supply1"
    , "table", "task", "time", "tran", "tranif0", "tranif1"
    , "tri", "tri0", "tri1", "triand", "trior", "trireg"
    , "vectored", "wait", "wand", "weak0", "weak1", "while", "wire", "wor"
    , "xnor", "xor"
    ] ++

    -- introduced in Verilog-2001
    [ "automatic", "cell", "config", "design", "endconfig", "endgenerate"
    , "generate", "genvar", "instance", "liblist", "localparam"
    , "noshowcancelled", "pulsestyle_ondetect", "pulsestyle_onevent"
    , "showcancelled", "signed", "use"
    ]


verilogOps :: [String]
verilogOps
  = [ "+", "-", "!", "~", "&", "~&", "|", "~|", "^", "~^", "^~"
    , "+", "-", "*", "/", "%", ">", ">=", "<", "<="
    , "&&", "||", "==", "!=", "===", "!===", "&", "|", "^", "^~", "~^"
    , "<<", ">>", "<<<", ">>>"
    , "?", ":", "->"
    ] ++

    -- introduced in Verilog-2001
    [ "**" ]

-- --------------------

verilogFile :: Stream s Identity Char => P s Verilog
verilogFile
  = do whiteSpace
       ds <- many description
       eof
       return (Verilog ds)

description :: Stream s Identity Char => P s Description
description
  = liftM ModuleDescription module_description <|>
    liftM UDPDescription udp_description

module_description :: Stream s Identity Char => P s Module
module_description
  = do reserved "module"
       name <- ident
       ports <- parens (commaSep ident) <|> return []
       semi
       items <- many module_item
       reserved "endmodule"
       return (Module name ports items)

module_item :: Stream s Identity Char => P s Item
module_item
  = liftM ParamDeclItem parameter_declaration <|>
    liftM InputDeclItem input_declaration <|>
    liftM OutputDeclItem output_declaration <|>
    liftM InOutDeclItem inout_declaration <|>
    liftM NetDeclItem net_declaration <|>
    liftM RegDeclItem reg_declaration <|>
    liftM EventDeclItem event_declaration <|>
    liftM PrimitiveInstItem primitive_instance <|>
    liftM InstanceItem module_or_udp_instance <|>
    liftM ParamOverrideItem (fail "TODO param override") <|>
    continuous_assign <|>
    (reserved "initial" >> liftM InitialItem statement) <|>
    (reserved "always" >> liftM AlwaysItem statement) <|>
    task_decl <|>
    function_decl
  <?> "module item"

task_decl :: Stream s Identity Char => P s Item
task_decl
  = do reserved "task"
       x <- ident
       semi
       ys <- many local_decl
       s <- statement
       reserved "endtask"
       return (TaskItem x ys s)

function_decl :: Stream s Identity Char => P s Item
function_decl
  = do reserved "function"
       t <- optionMaybe function_type
       x <- ident
       semi
       ys <- many local_decl
       s <- statement
       reserved "endfunction"
       return (FunctionItem t x ys s)

function_type :: Stream s Identity Char => P s FunctionType
function_type
  = liftM FunctionTypeRange range <|>
    (reserved "integer" >> return FunctionTypeInteger) <|>
    (reserved "real" >> return FunctionTypeReal)

udp_description :: Stream s Identity Char => P s UDP
udp_description
  = do reserved "primitive"
       x <- ident
       symbol "("
       x_out <- ident
       comma
       x_ins <- commaSep1 ident
       symbol ")"
       semi
       ds <- many1 udp_port_decl
       y <- optionMaybe udp_initial_statement
       t <- udp_table
       reserved "endprimitive"
       return (UDP x x_out x_ins ds y t)

udp_port_decl :: Stream s Identity Char => P s UDPDecl
udp_port_decl
  = liftM UDPOutputDecl output_declaration <|>
    liftM UDPInputDecl input_declaration <|>
    do reserved "reg"
       x <- ident
       semi
       return (UDPRegDecl x)
  <?> "UDP port declaration"

udp_initial_statement :: Stream s Identity Char => P s UDPInitialStatement
udp_initial_statement
  = do reserved "initia"
       x <- ident
       symbol "="
       e <- expression
       semi
       return (UDPInitialStatement x e)
  <?> "UDP initial statement"

udp_table :: Stream s Identity Char => P s TableDefinition
udp_table
  = do reserved "table"
       es <- many1 table_entry
       t <- case partitionEithers es of
              (es', []) -> return (CombinationalTable es')
              ([], es') -> return (SequentialTable es')
              _ -> fail "UDP table: mix of sequential and combinational entries"
       reserved "endtable"
       return t

table_entry :: Stream s Identity Char =>
               P s (Either CombinationalEntry SequentialEntry)
table_entry
  = try (liftM Left combinational_entry) <|>
    liftM Right sequential_entry

combinational_entry :: Stream s Identity Char => P s CombinationalEntry
combinational_entry
  = do xs <- many1 level_symbol
       colon
       y <- choice [ lexeme (char x) | x <- outputSymbols ]
       semi
       return (CombinationalEntry xs y)

sequential_entry :: Stream s Identity Char => P s SequentialEntry
sequential_entry
  = do x <- many1 (liftM Left level_symbol <|> liftM Right edge)
       colon
       y <- level_symbol
       colon
       z <- next_state
       semi
       return (SequentialEntry x y z)

level_symbol :: Stream s Identity Char => P s LevelSymbol
level_symbol = choice [ lexeme (char x) | x <- levelSymbols ]

next_state :: Stream s Identity Char => P s NextState
next_state = choice [ lexeme (char x) | x <- nextStates ]

edge :: Stream s Identity Char => P s Edge
edge
  = parens (liftM2 EdgeLevels (oneOf levelSymbols) (oneOf levelSymbols)) <|>
    liftM EdgeSymbol (choice [ lexeme (char x) | x <- edgeSymbols ])

-- -----------------------------------------------------------------------------
-- declarations

local_decl :: Stream s Identity Char => P s LocalDecl
local_decl
  = liftM LocalParamDecl parameter_declaration <|>
    liftM LocalInputDecl input_declaration <|>
    liftM LocalOutputDecl output_declaration <|>
    liftM LocalInOutDecl inout_declaration <|>
    liftM LocalRegDecl reg_declaration
  <?> "local declaration"

parameter_declaration :: Stream s Identity Char => P s ParamDecl
parameter_declaration
  = do reserved "parameter"
       param_assigns <- (commaSep1 parameter_assignment)
                        <?> "parameter list"
       semi
       return (ParamDecl param_assigns)
  <?> "parameter declaration"

input_declaration :: Stream s Identity Char => P s InputDecl
input_declaration
  = do reserved "input"
       r <- optionMaybe range
       xs <- commaSep1 ident
       semi
       return (InputDecl r xs)
  <?> "input declaration"

output_declaration :: Stream s Identity Char => P s OutputDecl
output_declaration
  = do reserved "output"
       r <- optionMaybe range
       xs <- commaSep1 ident
       semi
       return (OutputDecl r xs)
  <?> "output declaration"

inout_declaration :: Stream s Identity Char => P s InOutDecl
inout_declaration
  = do reserved "inout"
       r <- optionMaybe range
       xs <- commaSep1 ident
       semi
       return (InOutDecl r xs)
  <?> "inout declaration"

net_declaration :: Stream s Identity Char => P s NetDecl
net_declaration
  = do t <- net_type
       try (net_decl t) <|> net_decl_assign t
  <?> "net declaration"

net_decl :: Stream s Identity Char => NetType -> P s NetDecl
net_decl t
  = do r <- optionMaybe expand_range
       d <- optionMaybe delay
       xs <- commaSep1 ident
       semi
       return (NetDecl t r d xs)

net_decl_assign :: Stream s Identity Char => NetType -> P s NetDecl
net_decl_assign t
  = do s <- optionMaybe drive_strength
       r <- optionMaybe expand_range
       d <- optionMaybe delay
       xs <- commaSep1 (assign ident)
       semi
       return (NetDeclAssign t s r d xs)

reg_declaration :: Stream s Identity Char => P s RegDecl
reg_declaration
  = do t <- reg_type
       -- only the "reg" type can have a vector range before the identifier
       r <- case t of
              Reg_reg -> optionMaybe range
              _      -> return Nothing
       xs <- commaSep1 reg_var
       semi
       return (RegDecl t r xs)
  <?> "reg declaration"

event_declaration :: Stream s Identity Char => P s EventDecl
event_declaration
  = do reserved "event"
       xs <- commaSep1 ident
       return (EventDecl xs)
  <?> "event declaration"

continuous_assign :: Stream s Identity Char => P s Item
continuous_assign
  = do reserved "assign"
       s <- optionMaybe drive_strength
       d <- optionMaybe delay
       xs <- commaSep1 assignment
       semi
       return (AssignItem s d xs)

-- -----------------------------------------------------------------------------
-- primitive instantiation

primitive_instance :: Stream s Identity Char => P s PrimitiveInst
primitive_instance
  = do t <- prim_type
       s <- optionMaybe (try drive_strength)
            -- have to use 'try' here because drive_strength starts with the the
            -- open paren token, just like in prim_inst
       d <- optionMaybe delay
       xs <- commaSep1 prim_inst
       semi
       return (PrimitiveInst t s d xs)

prim_inst :: Stream s Identity Char => P s PrimInst
prim_inst
  = liftM2 PrimInst (optionMaybe prim_inst_name) (parens (commaSep expression))

prim_inst_name :: Stream s Identity Char => P s PrimInstName
prim_inst_name
  = liftM2 PrimInstName ident (optionMaybe range)

-- -----------------------------------------------------------------------------
-- module instantiations

module_or_udp_instance :: Stream s Identity Char => P s Instance
module_or_udp_instance
  = do x <- ident
       -- TODO optional strength
       ys <- optionMaybe $
             do symbol "#"
                symbol "("
                ys <- liftM Left (commaSep1 expression) <|>
                      liftM Right (commaSep1 parameter)
                symbol ")"
                return ys
       insts <- commaSep1 inst
       semi
       return (Instance x (fromMaybe (Left []) ys) insts)

parameter :: Stream s Identity Char => P s Parameter
parameter
  = do dot
       x <- ident
       e <- parens expression
       return (Parameter x e)

inst :: Stream s Identity Char => P s Inst
inst
  = do x <- ident
       r <- optionMaybe range
       c <- connections
       return (Inst x r c)

connections :: Stream s Identity Char => P s Connections
connections
  = parens (liftM Connections (commaSep1 expression) <|>
            liftM NamedConnections (commaSep1 named_connection))

named_connection :: Stream s Identity Char => P s NamedConnection
named_connection
  = do dot
       x <- ident
       e <- parens expression
       return (NamedConnection x e)

-- -----------------------------------------------------------------------------
-- statements

statement :: Stream s Identity Char => P s Statement
statement
  = assignment_stmt <|>
    if_stmt <|>
    case_stmt <|>
    for_stmt <|>
    while_stmt <|>
    delay_stmt <|>
    event_control_stmt <|>
    seq_block <|>
    par_block <|>
    task_stmt <|>
    assign_stmt
  <?> "statement"

assignment_stmt :: Stream s Identity Char => P s Statement
assignment_stmt
  = do x <- lvalue
       f <- (symbol "=" >> return BlockingAssignment) <|>
            (symbol "<=" >> return NonBlockingAssignment)
       c <- optionMaybe assignment_control
       e <- expression
       semi
       return (f x c e)

if_stmt :: Stream s Identity Char => P s Statement
if_stmt
  = do reserved "if"
       e <- parens expression
       s1 <- maybe_statement
       s2 <- (reserved "else" >> maybe_statement) <|>
             return Nothing
       return (IfStmt e s1 s2)

case_stmt :: Stream s Identity Char => P s Statement
case_stmt
  = do t <- (reserved "case" >> return Case) <|>
            (reserved "casex" >> return Casex) <|>
            (reserved "casez" >> return Casez)
       e <- parens expression
       xs <- many case_item
       reserved "endcase"
       return (CaseStmt t e xs)

case_item :: Stream s Identity Char => P s CaseItem
case_item
  = do f <- (reserved "default" >> return CaseDefault) <|>
            (many1 expression >>= return . CaseItem)
       colon
       s <- maybe_statement
       return (f s)
  <?> "case item"

for_stmt :: Stream s Identity Char => P s Statement
for_stmt
  = do reserved "for"
       symbol "("
       x <- assignment
       semi
       y <- expression
       semi
       z <- assignment
       symbol ")"
       s <- statement
       return (ForStmt x y z s)

while_stmt :: Stream s Identity Char => P s Statement
while_stmt
  = do reserved "while"
       symbol "("
       e <- expression
       symbol ")"
       s <- statement
       return (WhileStmt e s)

delay_stmt :: Stream s Identity Char => P s Statement
delay_stmt
  = liftM2 DelayStmt delay maybe_statement

event_control_stmt :: Stream s Identity Char => P s Statement
event_control_stmt
  = liftM2 EventControlStmt event_control maybe_statement

maybe_statement :: Stream s Identity Char => P s (Maybe Statement)
maybe_statement
  = (semi >> return Nothing) <|> liftM Just statement

seq_block :: Stream s Identity Char => P s Statement
seq_block
  = do reserved "begin"
       maybe_label <- optionMaybe (colon >> ident)
       -- TODO local block declarations
       stmts <- many1 statement
       reserved "end"
       return (SeqBlock maybe_label [] stmts)

par_block :: Stream s Identity Char => P s Statement
par_block
  = do reserved "fork"
       maybe_label <- optionMaybe (colon >> ident)
       -- TODO local block declarations
       stmts <- many1 statement
       reserved "join"
       return (ParBlock maybe_label [] stmts)

task_stmt :: Stream s Identity Char => P s Statement
task_stmt
  = do _ <- char '$'
       x <- ident
       args <- optionMaybe (parens (commaSep expression))
       semi
       return (TaskStmt x args)

assign_stmt :: Stream s Identity Char => P s Statement
assign_stmt
  = do reserved "assign"
       x <- assignment
       semi
       return (AssignStmt x)

event_control :: Stream s Identity Char => P s EventControl
event_control
  = do symbol "@"
       choice [ symbol "*" >> return EventControlWildCard
              , parens ((symbol "*" >> return EventControlWildCard) <|>
                        liftM EventControlExpr event_expr)
              , liftM EventControlIdent ident
              ]

assignment_control :: Stream s Identity Char => P s AssignmentControl
assignment_control
  = fail "assignment control" -- TODO

event_expr :: Stream s Identity Char => P s EventExpr
event_expr
  = do e1 <- choice [ liftM EventExpr expression
                    , reserved "posedge" >> liftM EventPosedge expression
                    , reserved "negedge" >> liftM EventNegedge expression
                    ]
       choice [ reserved "or" >> liftM (EventOr e1) event_expr
              , return e1
              ]

-- parse an assignment statement.
-- parametrized over the parser of the left-hand side.
assign :: Stream s Identity Char => P s a -> P s (a, Expression)
assign lhs
  = do x <- lhs
       symbol "="
       y <- expression
       return (x, y)

assignment :: Stream s Identity Char => P s Assignment
assignment
  = liftM (uncurry Assignment) (assign lvalue)

-- -----------------------------------------------------------------------------
-- expressions

const_expr :: Stream s Identity Char => P s Expression
const_expr = expression
             <?> "constant expression"

expression :: Stream s Identity Char => P s Expression
expression
  = do e1 <- expression'
       choice [ do symbol "?"
                   e2 <- expression
                   symbol ":"
                   e3 <- expression
                   return (ExprCond e1 e2 e3)
              , return e1
              ]

expression' :: Stream s Identity Char => P s Expression
expression'
  = buildExpressionParser opTable factor <?> "expression"
  where
    factor
      = choice [ parens expression
               , ident >>= expr_ident
               , expr_number
               , expr_string
               , expr_concat
               ]
      <?> "factor"

-- parse an expression that starts with an identifier
expr_ident :: Stream s Identity Char => Ident -> P s Expression
expr_ident x
  = liftM (ExprFunCall x) (parens (commaSep expression)) <|>
    (brackets $
     do e <- expression
        -- for ExprSlice, 'e' is actually a constant expression,
        -- but const_expr = expression, so it does not matter.
        choice [ colon >> liftM (ExprSlice x e) const_expr
               , symbol "+:" >> liftM (ExprSlicePlus x e) const_expr
               , symbol "-:" >> liftM (ExprSliceMinus x e) const_expr
               , return (ExprIndex x e)
               ]) <|>
    return (ExprVar x)

opTable :: Stream s Identity Char => OpTable s Expression
opTable
  = [ [ unaryOp "+" UPlus
      , unaryOp "-" UMinus
      , unaryOp "!" UBang
      , unaryOp "~" UTilde
      ]

    , [ binaryOp "*" Times
      , binaryOp "/" Divide
      , binaryOp "%" Modulo
      , binaryOp "**" Pow
      ]

    , [ binaryOp "+" Plus
      , binaryOp "-" Minus
      ]

    -- TODO <<< and >>> operators
    , [ binaryOp "<<" ShiftLeft
      , binaryOp ">>" ShiftRight
      ]

    , [ binaryOp "<" LessThan
      , binaryOp "<=" LessEqual
      , binaryOp ">" GreaterThan
      , binaryOp ">=" GreaterEqual
      ]

    , [ binaryOp "==" Equals
      , binaryOp "!=" NotEquals
      , binaryOp "===" CEquals
      , binaryOp "!==" CNotEquals
      ]

    , [ unaryOp "&" UAnd
      , unaryOp "~&" UNand
      , binaryOp "&" And
      ]

    , [ unaryOp "^" UXor
      , unaryOp "^~" UXnor
      , unaryOp "~^" UXnor
      , binaryOp "^" Xor
      , binaryOp "^~" Xnor
      , binaryOp "~^" Xnor
      ]
    , [ unaryOp "|" UOr
      , unaryOp "~|" UNor
      , binaryOp "|" Or
      , binaryOp "~|" Nor
      ]

    , [ binaryOp "&&" LAnd ]

    , [ binaryOp "||" LOr ]

    ]

unaryOp :: Stream s Identity Char => String -> UnaryOp -> Operator s Expression
unaryOp name fun
  = Prefix (reservedOp name >> return (ExprUnary fun))

binaryOp :: Stream s Identity Char => String -> BinaryOp -> Operator s Expression
binaryOp name fun
  = Infix (reservedOp name >> return (ExprBinary fun)) AssocLeft

expr_number :: Stream s Identity Char => P s Expression
expr_number
  = liftM ExprNum number

{- syntax for numbers:
  [ sign ] [ size ] [ 'base ] value               // integer
  [ sign ] value[.value] [ sign ] baseExponent    // real

where an integer value is allowed to have some subset of
"0123456789abcdefABCDEFxXzZ?_", depending on the base,
and a real value contains only decimal characters: "0123456789".
-}

expr_string :: Stream s Identity Char => P s Expression
expr_string
  = liftM ExprString stringLiteral

expr_index :: Stream s Identity Char => P s Expression
expr_index
  = liftM2 ExprIndex ident (brackets const_expr)

expr_slice :: Stream s Identity Char => P s Expression
expr_slice
  = do x <- ident
       symbol "["
       e1 <- const_expr
       colon
       e2 <- const_expr
       symbol "]"
       return (ExprSlice x e1 e2)

expr_concat :: Stream s Identity Char => P s Expression
expr_concat
  = do symbol "{"
       e <- expression
       choice [ do comma
                   es <- commaSep expression
                   symbol "}"
                   return (ExprConcat (e:es))
              , do es <- braces (commaSep expression)
                   symbol "}"
                   return (ExprMultiConcat e es)
              ]

lvalue :: Stream s Identity Char => P s LValue
lvalue
  = try expr_index <|>
    try expr_slice <|>
    liftM ExprVar ident <|>
    expr_concat

number :: Stream s Identity Char => P s Number
number
  = do { s <- optionMaybe sign
       ; whiteSpace
       ; base_integral s Nothing <|>
         do n <- decimal_number
            whiteSpace
            -- n could be the size of an integral, the integral value itself,
            -- or the integral part of a real.
            base_integral s (Just n) <|> real_number s n
       }
  where
    base_integral maybe_sign maybe_size

      = do b <- base
           whiteSpace
           x <- digits b
           whiteSpace
           return (IntNum maybe_sign maybe_size (Just b) x)

    -- given the optional sign and the integral part, parse the remainder of a
    -- real number, or yield an integer.
    real_number maybe_sign int_value
      = choice [ do maybe_fractional <- optionMaybe (dot >> decimal_number)
                    whiteSpace
                    maybe_exponent <- optionMaybe $
                                      do _ <- oneOf "eE"
                                         s <- optionMaybe sign
                                         e <- decimal_number
                                         return (s, e)
                    case (maybe_fractional, maybe_exponent) of
                      (Nothing, Nothing)
                        -> return $ IntNum maybe_sign Nothing Nothing int_value
                      _ -> return $ RealNum maybe_sign int_value
                                            maybe_fractional maybe_exponent
               ]


decimal_number :: Stream s Identity Char => P s String
decimal_number = digits DecBase

digits :: Stream s Identity Char => Base -> P s String
digits BinBase
  = many1 (oneOf "01xXzZ?_") <?> "binary digit"
digits OctBase
  = many1 (oneOf "01234567xXzZ?_") <?> "octal digit"
digits HexBase
  = many1 (oneOf "0123456789abcdefABCDEFxXzZ?_") <?> "hexadecimal digit"
digits DecBase
  = many1 (oneOf "0123456789_") <?> "decimal digit"

sign :: Stream s Identity Char => P s Sign
sign = (symbol "+" >> return Pos) <|>
       (symbol "-" >> return Neg)

base :: Stream s Identity Char => P s Base
base = do { _ <- char '\''
          ; (oneOf "bB" >> return BinBase) <|>
            (oneOf "oO" >> return OctBase) <|>
            (oneOf "dD" >> return DecBase) <|>
            (oneOf "hH" >> return HexBase)
          } <?> "base"

-- -----------------------------------------------------------------------------
-- miscellaneous

ident :: Stream s Identity Char => P s Ident
ident = liftM Ident identifier

reg_var :: Stream s Identity Char => P s RegVar
reg_var
  = do { x <- ident
       ; liftM (MemVar x) range <|>
         liftM (RegVar x) (optionMaybe (symbol "=" >> expression))
       }

parameter_assignment :: Stream s Identity Char => P s ParamAssign
parameter_assignment
  = do x <- ident
       _ <- symbol "="
       e <- const_expr
       return (ParamAssign x e)

expand_range :: Stream s Identity Char => P s ExpandRange
expand_range
  = liftM SimpleRange range <|>
    (reserved "scalared" >> liftM ScalaredRange range) <|>
    (reserved "vectored" >> liftM VectoredRange range)
  <?> "expand range"

range :: Stream s Identity Char => P s Range
range
  = brackets $ do e1 <- const_expr
                  colon
                  e2 <- const_expr
                  return (Range e1 e2)

delay :: Stream s Identity Char => P s Delay
delay = do symbol "#"
           expression -- expr_number <|> expr_var <|> expression

drive_strength :: Stream s Identity Char => P s DriveStrength
drive_strength
  = parens $
    (do s0 <- strength0
        comma
        s1 <- strength1
        return (Strength01 s0 s1)) <|>
    (do s1 <- strength1
        comma
        s0 <- strength0
        return (Strength10 s1 s0))


prim_type :: Stream s Identity Char => P s PrimType
prim_type = parse_table

strength0 :: Stream s Identity Char => P s Strength0
strength0 = parse_table

strength1 :: Stream s Identity Char => P s Strength1
strength1 = parse_table

net_type :: Stream s Identity Char => P s NetType
net_type = parse_table

reg_type :: Stream s Identity Char => P s RegType
reg_type = parse_table

-- this can be used for NetType, RegType, Strength0, Strength1, etc.
parse_table :: (Stream s Identity Char, Show a, Enum a, Bounded a) => P s a
parse_table
  = choice [ reserved (show x) >> return x
             | x <- [minBound..maxBound]
           ]

-- -----------------------------------------------------------------------------
