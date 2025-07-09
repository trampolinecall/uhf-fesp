{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module IRPP (dump_main_module) where

import qualified SIR as SIR
import qualified PP as PP
import qualified PP.Precedence as PP.Precedence
import qualified Arena as Arena
import qualified Type as Type
import Control.Monad.Reader (Reader, runReader, reader, ask)
import Data.List.NonEmpty (toList)

type IRReader stage = Reader (SIR.SIR stage)

type DumpableConstraints stage =
    ( DumpableIdentifier stage (SIR.DIdenStart stage)
    , DumpableIdentifier stage (SIR.VIdenStart stage)
    , DumpableIdentifier stage (SIR.PIdenStart stage)
    , DumpableIdentifier stage (SIR.SplitIdentifier stage (SIR.VIdenStart stage), SIR.VIdenResolved stage)
    , DumpableIdentifier stage (SIR.SplitIdentifier stage (SIR.PIdenStart stage), SIR.PIdenResolved stage)
    )

dump_main_module :: DumpableConstraints stage => SIR.SIR stage -> String
dump_main_module ir@(SIR.SIR modules _ _ _ _ (SIR.CU root_module _)) = PP.render $ runReader (define_module $ Arena.get modules root_module) ir

get_adt_arena :: IRReader stage (Arena.Arena (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.ADTKey)
get_adt_arena = reader (\ (SIR.SIR _ adts _ _ _ _) -> adts)
get_type_synonym_arena :: IRReader stage (Arena.Arena (Type.TypeSynonym (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage)) Type.TypeSynonymKey)
get_type_synonym_arena = reader (\ (SIR.SIR _ _ syns _ _ _) -> syns)
get_quant_var_arena :: IRReader stage (Arena.Arena Type.QuantVar Type.QuantVarKey)
get_quant_var_arena = reader (\ (SIR.SIR _ _ _ vars _ _) -> vars)

get_var :: SIR.VariableKey -> IRReader stage (SIR.Variable stage)
get_var k = reader (\ (SIR.SIR _ _ _ _ vars _) -> Arena.get vars k)
get_module :: SIR.ModuleKey -> IRReader stage (SIR.Module stage)
get_module k = reader (\ (SIR.SIR modules _ _ _ _ _) -> Arena.get modules k)
get_adt :: Type.ADTKey -> IRReader stage (Type.ADT (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage))
get_adt k = reader (\ (SIR.SIR _ adts _ _ _ _) -> Arena.get adts k)
get_type_syn :: Type.TypeSynonymKey -> IRReader stage (Type.TypeSynonym (SIR.TypeExpr stage, SIR.TypeExprEvaledAsType stage))
get_type_syn k = reader (\ (SIR.SIR _ _ syns _ _ _) -> Arena.get syns k)
get_quant_var :: Type.QuantVarKey -> IRReader stage Type.QuantVar
get_quant_var k = reader (\ (SIR.SIR _ _ _ quant_vars _ _) -> Arena.get quant_vars k)

define_module :: DumpableConstraints stage => SIR.Module stage -> IRReader stage PP.Token
define_module (SIR.Module bindings adts type_synonyms) =
    ask >>= \ sir ->
    get_quant_var_arena >>= \ quant_var_arena ->
    mapM (\ k -> get_adt k >>= \ adt -> pure (define_adt quant_var_arena (\ (ty, _) -> runReader (type_expr ty) sir) adt)) adts >>= \ adts_defined ->
    mapM (\ k -> get_type_syn k >>= \ ts -> pure (define_type_synonym (\ (ty, _) -> runReader (type_expr ty) sir) ts)) type_synonyms >>= \ type_synonyms_defined ->
    mapM define_binding bindings >>= \ bindings_defined ->
    pure (PP.flat_block $ adts_defined <> type_synonyms_defined <> bindings_defined)

define_binding :: DumpableConstraints stage => SIR.Binding stage -> IRReader stage PP.Token
define_binding (SIR.Binding pat init) = pattern pat >>= \ pat -> expr init >>= \ init -> pure $ PP.List [pat, " = ", init, ";"]

refer_var :: SIR.VariableKey -> IRReader stage PP.Token
refer_var k = get_var k >>= \case
    SIR.Variable _ name -> pure $ PP.String name

refer_bv :: SIR.BoundValue -> IRReader stage PP.Token
refer_bv (SIR.BoundValue'Variable v) = refer_var v
refer_bv (SIR.BoundValue'ADTVariant var) = refer_iden var
-- refer_bv (SIR.BoundValue'Intrinsic i) = pure $ PP.String $ Intrinsics.intrinsic_bv_name i

refer_decl :: DumpableType stage t => SIR.Decl t -> IRReader stage PP.Token
refer_decl d = case d of
    SIR.Decl'Module m ->
        get_module m >>= \ (SIR.Module _ _ _) ->
        pure (PP.String "module")
    SIR.Decl'Type ty -> refer_type ty
    SIR.Decl'ExternPackage _ -> pure "extern package"

refer_adt_variant :: Type.VariantIndex -> IRReader stage PP.Token
refer_adt_variant variant_index@(Type.VariantIndex _ adt_key _) =
    refer_adt <$> get_adt adt_key >>= \ adt_referred ->
    Type.get_variant <$> get_adt_arena <*> pure variant_index >>= \ variant ->
    let variant_name = Type.variant_name variant
    in pure $ PP.List [adt_referred, "::", PP.String variant_name]

class DumpableType stage ty where
    refer_type :: ty -> IRReader stage PP.Token

instance DumpableType stage Type.Type where
    refer_type t = do
        adt_arena <- get_adt_arena
        type_synonym_arena <- get_type_synonym_arena
        quant_var_arena <- get_quant_var_arena
        pure (refer_type' adt_arena type_synonym_arena quant_var_arena t)

class DumpableIdentifier stage i where
    refer_iden :: i -> IRReader stage PP.Token

instance DumpableIdentifier stage t => DumpableIdentifier stage (Maybe t) where -- TODO: remove this
    refer_iden (Just t) = refer_iden t
    refer_iden Nothing = pure $ PP.String "<name resolution error>"

instance (DumpableConstraints stage, DumpableIdentifier stage start) => DumpableIdentifier stage (SIR.SplitIdentifier stage start) where
    refer_iden (SIR.SplitIdentifier'Get texpr next) = type_expr texpr >>= \ texpr -> pure (PP.List [texpr, "::", PP.String $ next])
    refer_iden (SIR.SplitIdentifier'Single start) = refer_iden start

instance (DumpableConstraints stage, DumpableIdentifier stage start) => DumpableIdentifier stage (SIR.SplitIdentifier stage start, resolved) where
    refer_iden (a, _) = refer_iden a -- TODO: figure out how to use resolved but only if it is not ()

instance DumpableIdentifier stage String where
    refer_iden = pure . PP.String

instance DumpableType stage t => DumpableIdentifier stage (SIR.Decl t) where
    refer_iden = refer_decl
instance DumpableIdentifier stage SIR.BoundValue where
    refer_iden = refer_bv
instance DumpableIdentifier stage Type.VariantIndex where
    refer_iden = refer_adt_variant

-- TODO: dump type info too

quant_var :: Type.QuantVarKey -> IRReader stage PP.Token
quant_var k = get_quant_var k >>= \ (Type.QuantVar (name)) -> pure $ PP.String name

type_expr :: DumpableConstraints stage => SIR.TypeExpr stage -> IRReader stage PP.Token
type_expr = PP.Precedence.pp_precedence_m levels PP.Precedence.parenthesize
    where
        levels (SIR.TypeExpr'Forall _ vars ty) = (1, \ cur _ -> mapM quant_var vars >>= \ vars -> cur ty >>= \ ty -> pure (PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList vars, " ", ty]))
        levels (SIR.TypeExpr'Function _ arg res) = (2, \ cur next -> next arg >>= \ arg -> cur res >>= \ res -> pure (PP.List [arg, " -> ", res]))
        levels (SIR.TypeExpr'Apply _ ty arg) = (3, \ cur _ -> cur ty >>= \ ty -> type_expr arg >>= \ arg -> pure (PP.List [ty, "#(", arg, ")"]))
        levels (SIR.TypeExpr'Get _ parent name) = (3, \ cur _ -> cur parent >>= \ parent -> pure (PP.List [parent, "::", PP.String $ name]))
        levels (SIR.TypeExpr'Refer _ iden) = (4, \ _ _ -> refer_iden iden)
        levels (SIR.TypeExpr'Tuple _ a b) = (4, \ _ _ -> type_expr a >>= \ a -> type_expr b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b]))
        levels (SIR.TypeExpr'Hole _ _ hid) = (4, \ _ _ -> pure $ PP.List ["?", PP.String $ hid])
        levels (SIR.TypeExpr'Wild _) = (4, \ _ _ -> pure $ PP.String "_")
        levels (SIR.TypeExpr'Poison _) = (4, \ _ _ -> pure $ PP.String "poison")

expr :: DumpableConstraints stage => SIR.Expr stage -> IRReader stage PP.Token
expr = PP.Precedence.pp_precedence_m levels PP.Precedence.parenthesize
    where
        levels (SIR.Expr'BinaryOps _ _ first ops) =
            (0, \ _ next ->
                next first >>= \ first ->
                mapM
                    (\ (op_split_iden, op_resolved, rhs) ->
                        refer_iden (op_split_iden, op_resolved) >>= \ op ->
                        next rhs >>= \ rhs ->
                        pure (PP.List [op, " ", rhs]))
                    ops >>= \ ops ->
                pure (PP.List [first, PP.Block PP.Inconsistent Nothing (Just " ") Nothing ops]))

        levels (SIR.Expr'Call _ callee arg) = (1, \ cur _ -> cur callee >>= \ callee -> expr arg >>= \ arg -> pure (PP.FirstOnLineIfMultiline $ PP.List [callee, "(", arg, ")"]))
        levels (SIR.Expr'TypeApply _ e (arg, _)) = (1, \ cur _ -> cur e >>= \ e -> type_expr arg >>= \ arg -> pure (PP.List [e, "#(", arg, ")"]))

        levels (SIR.Expr'Identifier _ split resolved) = (2, \ _ _ -> PP.FirstOnLineIfMultiline <$> refer_iden (split, resolved))
        levels (SIR.Expr'Hole _ hid) = (2, \ _ _ -> pure $ PP.List ["?", PP.String $ hid])
        levels (SIR.Expr'Poison _) = (2, \ _ _ -> pure $ PP.String "poison")
        levels (SIR.Expr'Char _ c) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show c)
        levels (SIR.Expr'String _ s) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show s)
        levels (SIR.Expr'Int _ i) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show i)
        levels (SIR.Expr'Float _ f) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ show f)
        levels (SIR.Expr'Bool _ b) = (2, \ _ _ -> pure $ PP.FirstOnLineIfMultiline $ PP.String $ if b then "true" else "false")
        levels (SIR.Expr'Tuple _ a b) = (2, \ _ _ -> expr a >>= \ a -> expr b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b]))
        levels (SIR.Expr'Lambda _ param body) = (2, \ _ _ -> PP.FirstOnLineIfMultiline <$> (pattern param >>= \ param -> expr body >>= \ body -> pure (PP.List ["\\ ", param, " -> ", body]))) -- TODO: decide if this should be \ (x) -> or \ x ->

        levels (SIR.Expr'Let _ bindings adts type_synonyms body) = (2, \ _ _ -> pp_let "let" bindings adts type_synonyms body)
        levels (SIR.Expr'LetRec _ bindings adts type_synonyms body) = (2, \ _ _ -> pp_let "letrec" bindings adts type_synonyms body)

        levels (SIR.Expr'If _ cond t f) = (2, \ _ _ -> expr cond >>= \ cond -> expr t >>= \ t -> expr f >>= \ f -> pure (PP.FirstOnLineIfMultiline $ PP.List ["if ", cond, " then ", t, " else ", f]))
        levels (SIR.Expr'Match _ e arms) = (2, \ _ _ -> expr e >>= \ e -> mapM (\ (p, e) -> pattern p >>= \ p -> expr e >>= \ e -> pure (PP.List [p, " -> ", e, ";"])) arms >>= \ arms -> pure (PP.List ["match ", e, " ", PP.braced_block arms]))

        levels (SIR.Expr'TypeAnnotation _ (ty, _) e) = (2, \ _ _ -> type_expr ty >>= \ ty -> expr e >>= \ e -> pure (PP.List [":", ty, ": ", e]))

        levels (SIR.Expr'Forall _ tys e) = (2, \ _ _ -> mapM quant_var tys >>= \ tys -> expr e >>= \ e -> pure (PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ toList tys, " ", e]))

pp_let :: DumpableConstraints stage => String -> [SIR.Binding stage] -> [Type.ADTKey] -> [Type.TypeSynonymKey] -> SIR.Expr stage -> IRReader stage PP.Token
pp_let let_kw bindings adts type_synonyms body = do
    sir <- ask

    bindings <- mapM define_binding bindings
    quant_var_arena <- get_quant_var_arena
    adts <- mapM (\ k -> get_adt k >>= \ adt -> pure (define_adt quant_var_arena (\ (ty, _) -> runReader (type_expr ty) sir) adt)) adts
    type_synonyms <- mapM (\ k -> get_type_syn k >>= \ ts -> pure (define_type_synonym (\ (ty, _) -> runReader (type_expr ty) sir) ts)) type_synonyms

    body <- expr body

    let all_decls = adts ++ type_synonyms ++ bindings
    pure
        $ case all_decls of
            [decl] -> PP.FirstOnLineIfMultiline $ PP.List [PP.String let_kw, " ", decl, "\n", body]
            _ -> PP.FirstOnLineIfMultiline $ PP.List [PP.String let_kw, " ", PP.braced_block all_decls, "\n", body]

pattern :: DumpableConstraints stage => SIR.Pattern stage -> IRReader stage PP.Token
pattern (SIR.Pattern'Identifier _ var_key) = refer_var var_key
pattern (SIR.Pattern'Wildcard _) = pure $ PP.String "_"
pattern (SIR.Pattern'Tuple _ a b) = pattern a >>= \ a -> pattern b >>= \ b -> pure (PP.parenthesized_comma_list PP.Inconsistent [a, b])
pattern (SIR.Pattern'Named _ var_key subpat) = refer_var (var_key) >>= \ var_key -> pattern subpat >>= \ subpat -> pure (PP.List ["@", var_key, " ", subpat])
pattern (SIR.Pattern'AnonADTVariant _ variant_split_iden variant_resolved_iden _ fields) = refer_iden (variant_split_iden, variant_resolved_iden) >>= \ variant -> mapM pattern fields >>= \ fields -> pure (PP.List [variant, PP.parenthesized_comma_list PP.Inconsistent fields])
pattern (SIR.Pattern'NamedADTVariant _ variant_split_iden variant_resolved_iden _ fields) = refer_iden (variant_split_iden, variant_resolved_iden) >>= \ variant -> mapM (\ (field_name, field_pat) -> pattern field_pat >>= \ field_pat -> pure (PP.List [PP.String $ field_name, " = ", field_pat, ";"])) fields >>= \ fields -> pure (PP.List [variant, PP.braced_block fields])
pattern (SIR.Pattern'Poison _) = pure $ PP.String "poison"

define_adt :: Arena.Arena Type.QuantVar Type.QuantVarKey -> (ty -> PP.Token) -> Type.ADT ty -> PP.Token
define_adt quant_vars show_ty (Type.ADT name vars variants) =
    let variants' = PP.braced_block $ map pp_variant variants
        vars'
            | null vars = PP.List [""]
            | otherwise = PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent (map (define_quant_var quant_vars) vars)]
    in PP.List ["data ", PP.String name, vars', " ", variants', ";"]
    where
        pp_variant (Type.Variant'Anon name fields) = PP.List [PP.String name, PP.parenthesized_comma_list PP.Inconsistent $ map show_ty fields, ";"]
        pp_variant (Type.Variant'Named name fields) = PP.List [PP.String name, " ", PP.braced_block $ map (\ (name, ty) -> PP.List [PP.String name, ": ", show_ty ty, ";"]) fields, ";"]

define_type_synonym :: (ty -> PP.Token) -> Type.TypeSynonym ty -> PP.Token
define_type_synonym show_ty (Type.TypeSynonym name expansion) = PP.List ["typesyn ", PP.String name, " = ", show_ty expansion, ";"]

refer_adt :: Type.ADT ty -> PP.Token
refer_adt (Type.ADT name _ _) = PP.String name

refer_type_synonym :: Type.TypeSynonym ty -> PP.Token
refer_type_synonym (Type.TypeSynonym name _) = PP.String name

-- TODO: construct an ast and print it
-- TODO: precedence for this
refer_type' :: Arena.Arena (Type.ADT ty) Type.ADTKey -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Arena.Arena Type.QuantVar Type.QuantVarKey -> Type.Type -> PP.Token
refer_type' adts type_synonyms vars = go
    where
        go ty =
            case ty of
                Type.Type'ADT k params ->
                    let params' = map go params
                        params''
                            | null params' = ""
                            | otherwise = PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent params']
                    in PP.List [refer_adt (Arena.get adts k), params'']
                Type.Type'Synonym k -> refer_type_synonym $ Arena.get type_synonyms k
                Type.Type'Int -> "int"
                Type.Type'Float -> "float"
                Type.Type'Char -> "char"
                Type.Type'String -> "string"
                Type.Type'Bool -> "bool"
                Type.Type'Function a r ->
                    let a_shown = go a
                        r_shown = go r
                    in PP.List [a_shown, " -> ", r_shown]
                Type.Type'Tuple a b ->
                    let a_shown = go a
                        b_shown = go b
                    in PP.parenthesized_comma_list PP.Inconsistent [a_shown, b_shown]
                Type.Type'QuantVar var ->
                    let (Type.QuantVar name) = Arena.get vars var
                    in PP.String name -- TODO: write id
                Type.Type'Forall new_vars ty ->
                    let ty' = go ty
                    in PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent (map (\ vk -> let (Type.QuantVar name) = Arena.get vars vk in PP.String name) (toList new_vars)), " ", ty']
                -- TODO: do kinds correctly
                Type.Type'Kind'Type -> PP.String "*" -- TODO: this does not seem right
                Type.Type'Kind'Arrow a b -> PP.List [refer_type' adts type_synonyms vars a, PP.String " -># ", refer_type' adts type_synonyms vars b] -- TODO: precedence
                Type.Type'Kind'Kind -> PP.String "<kind>" -- TODO: this is most definitely not correct

define_quant_var :: Arena.Arena Type.QuantVar Type.QuantVarKey -> Type.QuantVarKey -> PP.Token
define_quant_var vars var =
    let (Type.QuantVar name) = Arena.get vars var
    in PP.String name -- TODO: write id
