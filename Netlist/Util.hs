-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
--
-- Utility functions for Netlist, particularly for generating AST elements.
-- -----------------------------------------------------------------------------

module Netlist.Util where

import Netlist.AST

-- -----------------------------------------------------------------------------

-- generate a process declaration for a generic register based on the following:
--  * the register name (as an expression)
--  * clock expression
--  * width of the register
--  * length of the delay in clock cycles (> 1 requires name generation)
--  * optional asynchronous reset and initial value
--  * optional clock enable
--  * optional synchronous restart and initial value
--  * optional load enable
--  * when enabled, the expression to assign to the identifier
-- TODO
--  * support negative-edge triggered clock/reset, active-low reset/restart
--  * support true clock enable (as opposed to load enable)?
--  * support delay amount > 1

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
