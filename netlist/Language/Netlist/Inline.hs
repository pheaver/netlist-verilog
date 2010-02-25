-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
--
-- An inliner for Netlist.AST.
-- -----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types, PatternGuards #-}

module Language.Netlist.Inline ( inlineModule ) where

import Data.Generics
--import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Language.Netlist.AST

-- -----------------------------------------------------------------------------

inlineModule :: Module -> Module
inlineModule (Module name inputs outputs decls)
  = Module name inputs outputs decls''
  where
    deps    = getIdentExprs decls
    bs      = getBindings decls
    bs'     = Map.filterWithKey (shouldInline (map fst outputs) deps) bs
    decls'  = replaceExprs bs' decls
    decls'' = removeDecls (Map.keys bs') decls'

-- given a list of identifier-to-expression bindings, replace the identifiers everywhere in an AST.
-- Note: "everywhere" applies bottom-up.  We want everywhere', which is top-down.
replaceExprs :: forall a. (Data a) => Map Ident Expr -> a -> a
replaceExprs bs a = everywhere' (mkT f) a
  where
    f e
      | ExprVar x <- e, Just e' <- Map.lookup x bs
                       = e' -- replaceExprs bs e'
      | otherwise      = e

-- this is essentially a DCE pass.  it removes the declarations that have been inlined.
removeDecls :: [Ident] -> [Decl] -> [Decl]
removeDecls xs = mapMaybe f
  where
    f d@(NetDecl x _ _)
      = if elem x xs then Nothing else Just d
    f d@(NetAssign x _)
      = if elem x xs then Nothing else Just d
    f decl
      = Just decl

-- -----------------------------------------------------------------------------
-- utility functions

getBindings :: [Decl] -> Map Ident Expr
getBindings = Map.unions . map getDeclBinding

getDeclBinding :: Decl -> Map Ident Expr
getDeclBinding (NetDecl x _ (Just expr))
  = Map.singleton x expr
getDeclBinding (NetAssign x expr)
  = Map.singleton x expr
getDeclBinding _
  = Map.empty

shouldInline :: [Ident] -> Map Ident [Expr] -> Ident -> Expr -> Bool
shouldInline ignore deps x e
  | x `notElem` ignore, Just n <- checkUsers
  = case e of
      -- always inline trivial expressions regardless of the number of users.
      ExprNum _                 -> True
      ExprLit _ _               -> True
      ExprString _              -> True
      ExprVar _                 -> True
      ExprIndex _ _             -> True
      ExprSlice _ _ _           -> True
      -- ExprSliceOff _ _ _        -> True

      -- any complex expressions should only be inlined if they're used once.
      _                         -> n == 1

  | otherwise
  = False
  where
    -- returns Nothing if this identifier cannot be inlined because it is
    -- referred to by a Index/Project/FuncCall.  returns Just n if the only
    -- users are 'n' number of ExprVar expressions.
    checkUsers
      = if all checkUser zs then Just (length zs) else Nothing
      where
        zs = fromMaybe [] (Map.lookup x deps)
        checkUser (ExprVar _) = True
        checkUser _           = False

-- map each identifier to every expression that directly refers to that identifier.
getIdentExprs :: forall a. (Data a) => a -> Map Ident [Expr]
getIdentExprs a = f Map.empty (getAll a)
  where
    f :: Map Ident [Expr] -> [Expr] -> Map Ident [Expr]
    f m [] = m
    f m (expr:rest)
      = f m' rest
      where m' = case maybeExprIdent expr of
                   Just v  -> Map.insertWith (++) v [expr] m
                   Nothing -> m

-- generically get a list of all terms of a certain type.
getAll :: forall a b. (Data a, Typeable b) => a -> [b]
getAll = listify (\_ -> True)

-- if an expression references an identifier directly, return the identifier.
-- note that subexpressions are not counted here!
maybeExprIdent :: Expr -> Maybe Ident
maybeExprIdent (ExprVar x)               = Just x
maybeExprIdent (ExprIndex x _)           = Just x
maybeExprIdent (ExprSlice x _ _)         = Just x
maybeExprIdent (ExprSliceOff x _ _)      = Just x
maybeExprIdent (ExprFunCall x _)         = Just x
maybeExprIdent _                         = Nothing

-- -----------------------------------------------------------------------------
