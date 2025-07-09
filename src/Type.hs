module Type
    ( Type (..)
    , ADT (..)
    , ADTKey
    , FieldIndex (..)
    , QuantVar (..)
    , QuantVarKey
    , TypeSynonym (..)
    , TypeSynonymKey
    , Variant (..)
    , VariantIndex (..)
    , get_field_type
    , get_variant
    , type_kind
    , variant_field_idxs
    , variant_field_types
    , variant_idxs
    , variant_name
    ) where

import qualified Arena
import Data.Function ((&))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty, toList)
import Keys

data Type
    = Type'ADT ADTKey [Type]
    | Type'Synonym TypeSynonymKey
    | Type'Int
    | Type'Float
    | Type'Char
    | Type'String
    | Type'Bool
    | Type'Function Type Type
    | Type'Tuple Type Type
    | Type'QuantVar QuantVarKey
    | Type'Forall (NonEmpty QuantVarKey) Type
    | Type'Kind'Type
    | Type'Kind'Arrow Type Type
    | Type'Kind'Kind
    deriving Show

type_kind ::
    Arena.Arena (ADT (t, Type)) ADTKey -> Arena.Arena (TypeSynonym (t, Type)) TypeSynonymKey -> Arena.Arena QuantVar QuantVarKey -> Type -> Type
type_kind adt_arena type_synonym_arena quant_var_arena = go
    where
        go :: Type -> Type
        go t = case t of
            Type'ADT adt_key applied ->
                let ADT _ quant_vars _ = Arena.get adt_arena adt_key
                in make_arrows (map quant_var_kind (drop (length applied) quant_vars)) Type'Kind'Type
            Type'Synonym ts_key ->
                let TypeSynonym _ (_, expansion) = Arena.get type_synonym_arena ts_key
                in go expansion -- TODO: need to modify this when type synonyms can be parameterized
            Type'Int -> Type'Kind'Type
            Type'Float -> Type'Kind'Type
            Type'Char -> Type'Kind'Type
            Type'String -> Type'Kind'Type
            Type'Bool -> Type'Kind'Type
            Type'Function _ _ -> Type'Kind'Type
            Type'Tuple _ _ -> Type'Kind'Type
            Type'QuantVar qvk -> quant_var_kind qvk
            Type'Forall quant_vars result -> make_arrows (map quant_var_kind (toList quant_vars)) (go result)
            Type'Kind'Type -> Type'Kind'Kind
            Type'Kind'Arrow _ _ -> Type'Kind'Kind
            Type'Kind'Kind -> Type'Kind'Kind

        quant_var_kind :: QuantVarKey -> Type
        quant_var_kind qvk = Type'Kind'Type -- TODO: quant vars with different kinds
        make_arrows :: [Type] -> Type -> Type
        make_arrows [] res = res
        make_arrows (cur_arg : more_args) res = Type'Kind'Arrow cur_arg (make_arrows more_args res)

data DoNotConstruct = DoNotConstruct deriving (Show, Eq, Ord)

data ADT ty = ADT String [QuantVarKey] [Variant ty] deriving Show
data Variant ty
    = Variant'Named String [(String, ty)]
    | Variant'Anon String [ty]
    deriving Show
data VariantIndex = VariantIndex DoNotConstruct ADTKey Int deriving (Show, Eq, Ord)
data FieldIndex = FieldIndex DoNotConstruct VariantIndex Int deriving (Show, Eq, Ord)

variant_idxs :: Arena.Arena (ADT ty) ADTKey -> ADTKey -> [VariantIndex]
variant_idxs arena key =
    let (ADT _ _ variants) = Arena.get arena key
    in map (VariantIndex DoNotConstruct key) [0 .. length variants - 1]
variant_field_idxs :: Arena.Arena (ADT ty) ADTKey -> VariantIndex -> [FieldIndex]
variant_field_idxs arena v_idx =
    let variant = get_variant arena v_idx
    in case variant of
        Variant'Anon _ fields -> fields & zipWith (\i _ -> FieldIndex DoNotConstruct v_idx i) [0 ..]
        Variant'Named _ fields -> fields & zipWith (\i _ -> FieldIndex DoNotConstruct v_idx i) [0 ..]

variant_name :: Variant ty -> String
variant_name (Variant'Anon name _) = name
variant_name (Variant'Named name _) = name
variant_field_types :: Variant ty -> [ty]
variant_field_types (Variant'Anon _ tys) = tys
variant_field_types (Variant'Named _ tys) = tys & map snd

-- technically is partial, but because VariantIndexes cannot be constructed outside of this module and this module is careful to only construct them to valid variants, this should hopefully never error in practice
get_variant :: Arena.Arena (ADT ty) ADTKey -> VariantIndex -> Variant ty
get_variant adts (VariantIndex _ key i) =
    let (ADT _ _ variants) = Arena.get adts key
    in variants List.!! i

-- same note about partial but should not be as above
get_field_type :: Arena.Arena (ADT ty) ADTKey -> FieldIndex -> ty
get_field_type adts (FieldIndex _ variant i) =
    case get_variant adts variant of
        Variant'Named _ fields -> fields List.!! i & snd
        Variant'Anon _ fields -> fields List.!! i

newtype QuantVar = QuantVar String deriving Show -- TODO: put id

data TypeSynonym ty = TypeSynonym String ty deriving Show
