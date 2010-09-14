--------------------------------------------------------------------------------
-- |
-- Module       :  Language.Netlist.Util
-- Copyright    :  (c) Signali Corp. 2010
-- License      :  All rights reserved
--
-- Maintainer   : pweaver@signalicorp.com
-- Stability    : experimental
-- Portability  : non-portable
--
-- Utility functions for constructing Netlist AST elements.
--------------------------------------------------------------------------------

module Language.Netlist.Util where

import Language.Netlist.AST

-- -----------------------------------------------------------------------------

data Direction = Up | Down

unsizedInteger :: Integer -> Expr
unsizedInteger = unsizedIntegral

unsizedIntegral :: Integral a => a -> Expr
unsizedIntegral = ExprLit Nothing . ExprNum . toInteger

sizedInteger :: Int -> Integer -> Expr
sizedInteger = sizedIntegral

sizedIntegral :: Integral a => Int -> a -> Expr
sizedIntegral sz = ExprLit (Just sz) . ExprNum . toInteger

-- | Given a direction and size, maybe generate a 'Range', where a size of 1
-- yields 'Nothing'.
makeRange :: Direction -> Size -> Maybe Range
makeRange _ 1 = Nothing
makeRange d sz
  | sz > 1
  = let upper = unsizedIntegral (sz - 1)
        lower = unsizedInteger 0
    in Just $ case d of
                Up    -> Range lower upper
                Down  -> Range upper lower

  | otherwise
  = error ("makeRange: invalid size: " ++ show sz)

-- | Concatenate a list of expressions, unless there is just one expression.
exprConcat :: [Expr] -> Expr
exprConcat [e] = e
exprConcat es  = ExprConcat es

-- | Make a 'Seq' statement from a list of statements, unless there is just one
-- statement.
statements :: [Stmt] -> Stmt
statements [x] = x
statements xs  = Seq xs

-- -----------------------------------------------------------------------------

-- | generate a process declaration for a generic register based on the following:
--
--  * the register name (as an expression)
--
--  * clock expression
--
--  * width of the register
--
--  * optional asynchronous reset and initial value
--
--  * optional clock enable
--
--  * optional synchronous restart and initial value
--
--  * optional load enable
--
--  * when enabled, the expression to assign to the identifier
--
-- You can implement a shift register by passing in a concatenation for the
-- register expression and the input expression, though that is not compatible
-- with VHDL.
--

-- TODO
--  * support negative-edge triggered clock/reset, active-low reset/restart
--  * support true clock enable (as opposed to load enable)?

generateReg :: Expr -> Expr -> Maybe (Expr, Expr) -> Maybe (Expr, Expr) ->
               Maybe Expr -> Expr -> Decl
generateReg x clk mb_reset mb_restart mb_enable expr
  = ProcessDecl as
  where
    as = case mb_reset of
           Just (reset, initial)
             -> [ (Event reset PosEdge, Assign x initial), a0]
           Nothing
             -> [a0]

    a0 = (Event clk PosEdge, stmt2)

    stmt2 = case mb_restart of
              Just (restart, initial)
                -> If restart (Assign x initial) (Just stmt1)
              Nothing
                -> stmt1

    stmt1 = case mb_enable of
              Just enable  -> If enable stmt0 Nothing
              Nothing      -> stmt0

    stmt0 = Assign x expr

-- -----------------------------------------------------------------------------
