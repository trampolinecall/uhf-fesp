{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDeriving #-}

module SIR
    ( SIR (..)
    , Stage.Stage (..)
    , CU (..)
    , ADT
    , TypeSynonym
    , Decl (..)
    , ExternPackage (..)
    , ModuleKey
    , Module (..)
    , BoundValue (..)
    , VariableKey
    , Variable (..)
    , Binding (..)
    , HoleIdentifier
    , TypeExpr (..)
    , SplitIdentifier (..)
    , Expr (..)
    , Pattern (..)
    , expr_type
    , pattern_type
    , type_expr_evaled
    ) where

import qualified Arena

-- import qualified UHF.Data.IR.Intrinsics as Intrinsics

import Data.List.NonEmpty (NonEmpty)
import qualified Stage
import qualified Type
import Keys

-- TODO: clean up SIR to match AST a little more

-- "syntax based ir"
data SIR stage
    = SIR
        (Arena.Arena (Module stage) ModuleKey)
        (Arena.Arena (ADT stage) ADTKey)
        (Arena.Arena (TypeSynonym stage) TypeSynonymKey)
        (Arena.Arena Type.QuantVar QuantVarKey)
        (Arena.Arena (Variable stage) VariableKey)
        (CU stage)

-- TODO: when support for compiling libraries that should not need a main function, a field should be added that identifies whether or not the compilation unit is a library or an executable or this should be split into 2 constructors for libraries or executables
data CU stage = CU {cu_root_module :: ModuleKey, cu_main_function :: Maybe VariableKey}

type ADT stage = Type.ADT (TypeExpr stage, Stage.TypeExprEvaledAsType stage)
type TypeSynonym stage = Type.TypeSynonym (TypeExpr stage, Stage.TypeExprEvaledAsType stage)

data Decl ty
    = Decl'Module ModuleKey
    | Decl'Type ty
    | Decl'ExternPackage ExternPackage
    deriving Show

data ExternPackage
    -- = ExternPackage'IntrinsicsPackage
    deriving Show

data Module stage
    = Module [Binding stage] [ADTKey] [TypeSynonymKey]
deriving instance Stage.AllShowable stage => Show (Module stage)

data Variable stage
    = Variable (Stage.TypeInfo stage) String
deriving instance Stage.AllShowable stage => Show (Variable stage)

data BoundValue
    = BoundValue'Variable VariableKey
    | BoundValue'ADTVariant Type.VariantIndex
    deriving -- | BoundValue'Intrinsic Intrinsics.IntrinsicBoundValue
        Show

data Binding stage
    = Binding (Pattern stage) (Expr stage)
deriving instance Stage.AllShowable stage => Show (Binding stage)

type HoleIdentifier = String

data TypeExpr stage
    = TypeExpr'Refer (Stage.TypeExprEvaled stage) (Stage.DIdenStart stage)
    | TypeExpr'Get (Stage.TypeExprEvaled stage) (TypeExpr stage) String
    | TypeExpr'Tuple (Stage.TypeExprEvaled stage) (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Hole (Stage.TypeExprEvaled stage) (Stage.TypeExprEvaledAsType stage) HoleIdentifier
    | TypeExpr'Function (Stage.TypeExprEvaled stage) (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Forall (Stage.TypeExprEvaled stage) (NonEmpty QuantVarKey) (TypeExpr stage)
    | TypeExpr'Apply (Stage.TypeExprEvaled stage) (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Wild (Stage.TypeExprEvaled stage)
    | TypeExpr'Poison (Stage.TypeExprEvaled stage)
deriving instance Stage.AllShowable stage => Show (TypeExpr stage)

-- TODO: rename all Identifier to Refer
data SplitIdentifier stage start
    = SplitIdentifier'Get (TypeExpr stage) String
    | SplitIdentifier'Single start
deriving instance (Stage.AllShowable stage, Show start) => Show (SplitIdentifier stage start)

data Expr stage
    = Expr'Identifier (Stage.TypeInfo stage) (SplitIdentifier stage (Stage.VIdenStart stage)) (Stage.VIdenResolved stage)
    | Expr'Char (Stage.TypeInfo stage) Char
    | Expr'String (Stage.TypeInfo stage) String
    | Expr'Int (Stage.TypeInfo stage) Integer
    | Expr'Float (Stage.TypeInfo stage) Float
    | Expr'Bool (Stage.TypeInfo stage) Bool -- TODO: replace with identifier exprs
    | Expr'Tuple (Stage.TypeInfo stage) (Expr stage) (Expr stage)
    | Expr'Lambda (Stage.TypeInfo stage) (Pattern stage) (Expr stage)
    | Expr'Let (Stage.TypeInfo stage) [Binding stage] [ADTKey] [TypeSynonymKey] (Expr stage)
    | Expr'LetRec (Stage.TypeInfo stage) [Binding stage] [ADTKey] [TypeSynonymKey] (Expr stage)
    | Expr'BinaryOps
        (Stage.BinaryOpsAllowed stage)
        (Stage.TypeInfo stage)
        (Expr stage)
        [(SplitIdentifier stage (Stage.VIdenStart stage), Stage.VIdenResolved stage, Expr stage)]
    | Expr'Call (Stage.TypeInfo stage) (Expr stage) (Expr stage)
    | Expr'If (Stage.TypeInfo stage) (Expr stage) (Expr stage) (Expr stage)
    | Expr'Match (Stage.TypeInfo stage) (Expr stage) [(Pattern stage, Expr stage)]
    | Expr'Forall (Stage.TypeInfo stage) (NonEmpty QuantVarKey) (Expr stage)
    | Expr'TypeApply (Stage.TypeInfo stage) (Expr stage) (TypeExpr stage, Stage.TypeExprEvaledAsType stage)
    | Expr'TypeAnnotation (Stage.TypeInfo stage) (TypeExpr stage, Stage.TypeExprEvaledAsType stage) (Expr stage)
    | Expr'Hole (Stage.TypeInfo stage) HoleIdentifier
    | Expr'Poison (Stage.TypeInfo stage)
deriving instance Stage.AllShowable stage => Show (Expr stage)

data Pattern stage
    = Pattern'Identifier (Stage.TypeInfo stage) VariableKey
    | Pattern'Wildcard (Stage.TypeInfo stage)
    | Pattern'Tuple (Stage.TypeInfo stage) (Pattern stage) (Pattern stage)
    | Pattern'Named (Stage.TypeInfo stage) VariableKey (Pattern stage)
    | Pattern'AnonADTVariant
        (Stage.TypeInfo stage)
        (SplitIdentifier stage (Stage.PIdenStart stage))
        (Stage.PIdenResolved stage)
        [Stage.TypeInfo stage]
        [Pattern stage]
    | Pattern'NamedADTVariant
        (Stage.TypeInfo stage)
        (SplitIdentifier stage (Stage.PIdenStart stage))
        (Stage.PIdenResolved stage)
        [Stage.TypeInfo stage]
        [(String, Pattern stage)]
    | Pattern'Poison (Stage.TypeInfo stage)
deriving instance Stage.AllShowable stage => Show (Pattern stage)

type_expr_evaled :: TypeExpr stage -> Stage.TypeExprEvaled stage
type_expr_evaled (TypeExpr'Refer evaled _) = evaled
type_expr_evaled (TypeExpr'Get evaled _ _) = evaled
type_expr_evaled (TypeExpr'Tuple evaled _ _) = evaled
type_expr_evaled (TypeExpr'Hole evaled _ _) = evaled
type_expr_evaled (TypeExpr'Function evaled _ _) = evaled
type_expr_evaled (TypeExpr'Forall evaled _ _) = evaled
type_expr_evaled (TypeExpr'Apply evaled _ _) = evaled
type_expr_evaled (TypeExpr'Wild evaled) = evaled
type_expr_evaled (TypeExpr'Poison evaled) = evaled

expr_type :: Expr stage -> Stage.TypeInfo stage
expr_type (Expr'Identifier type_info _ _) = type_info
expr_type (Expr'Char type_info _) = type_info
expr_type (Expr'String type_info _) = type_info
expr_type (Expr'Int type_info _) = type_info
expr_type (Expr'Float type_info _) = type_info
expr_type (Expr'Bool type_info _) = type_info
expr_type (Expr'Tuple type_info _ _) = type_info
expr_type (Expr'Lambda type_info _ _) = type_info
expr_type (Expr'Let type_info _ _ _ _) = type_info
expr_type (Expr'LetRec type_info _ _ _ _) = type_info
expr_type (Expr'BinaryOps _ type_info _ _) = type_info
expr_type (Expr'Call type_info _ _) = type_info
expr_type (Expr'If type_info _ _ _ ) = type_info
expr_type (Expr'Match type_info _ _) = type_info
expr_type (Expr'Poison type_info) = type_info
expr_type (Expr'Hole type_info _) = type_info
expr_type (Expr'Forall type_info _ _) = type_info
expr_type (Expr'TypeApply type_info _ _) = type_info
expr_type (Expr'TypeAnnotation type_info _ _) = type_info

pattern_type :: Pattern stage -> Stage.TypeInfo stage
pattern_type (Pattern'Identifier type_info _) = type_info
pattern_type (Pattern'Wildcard type_info) = type_info
pattern_type (Pattern'Tuple type_info _ _) = type_info
pattern_type (Pattern'Named type_info _ _) = type_info
pattern_type (Pattern'Poison type_info) = type_info
pattern_type (Pattern'AnonADTVariant type_info _ _ _ _) = type_info
pattern_type (Pattern'NamedADTVariant type_info _ _ _ _) = type_info
