{-# LANGUAGE DataKinds #-}

module AST where

import GHC.TypeLits (Symbol)

data Token (x :: Symbol) = Token deriving Show
type KwIden = String
type SymbolIden = String
type AlphaIden = String

-- TODO: rename this to literal ast and make it literally store tokens (is this actually necessary?)

data KeywordRef
    = KeywordRef'Path Type KwIden
    | KeywordRef'Single KwIden
    deriving Show

data Operator
    = Operator'Path Type SymbolIden
    | Operator'Single SymbolIden
    deriving Show

data Decl
    = Decl'Value Pattern (Token "Equal") Expr
    | Decl'Data AlphaIden [AlphaIden] [DataVariant]
    | Decl'TypeSyn AlphaIden Type
    -- TODO: | Decl'Import Type
    deriving Show

data DataVariant
    = DataVariant'Anon AlphaIden [Type]
    | DataVariant'Named AlphaIden [(AlphaIden, Type)]
    -- TODO: keyword variants, which also replace named variants
    deriving Show

data Type
    = Type'Refer AlphaIden
    | Type'Get Type AlphaIden
    | Type'Tuple [Type] -- TODO: anonymous named products? (ie field names, but no datatype name)
    | Type'Hole AlphaIden
    | Type'Function Type Type
    | Type'Forall [AlphaIden] Type
    | Type'Apply Type [Type]
    | Type'Wild -- TODO: come up with better name for this
    deriving Show

data Expr
    = Expr'ReferAlpha (Maybe Type) AlphaIden
    | Expr'Char Char
    | Expr'String String
    | Expr'Int Integer
    | Expr'Float Float
    | Expr'Bool Bool -- TODO: replace with identifier exprs
    | Expr'Tuple [Expr]
    | Expr'Lambda [Pattern] Expr
    | Expr'Let [Decl] Expr
    | Expr'LetRec [Decl] Expr
    | Expr'Where Expr [Decl]
    | Expr'BinaryOps Expr [(Operator, Expr)] -- TODO: fix this
    | Expr'Call Expr [Expr]
    | Expr'If (Token "If") Expr Expr Expr
    | Expr'Match (Token "Match") Expr [(Pattern, Expr)]
    | Expr'Forall [AlphaIden] Expr -- TODO: add constraints like '#(T, U; Constraint#(T, U)) ...' (actually this todo is outdated with the syntax redesign in the lr1 overhaul)
    | Expr'TypeApply Expr [Type]
    | Expr'TypeAnnotation Type Expr
    | Expr'Hole AlphaIden
    deriving Show

data Pattern
    = -- TODO: symbol and keyword patterns
      Pattern'AlphaVar AlphaIden
    | Pattern'Wildcard (Token "Underscore")
    | Pattern'Tuple [Pattern]
    | -- TODO: symbol and keyword named patterns
      Pattern'NamedAlpha AlphaIden (Token "At") Pattern
    | Pattern'AnonADTVariant (Maybe Type) AlphaIden [Pattern]
    | Pattern'NamedADTVariant (Maybe Type) AlphaIden [(AlphaIden, Pattern)]
    deriving Show
