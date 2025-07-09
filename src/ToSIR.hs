module ToSIR (convert) where

import qualified AST
import qualified Arena
import Control.Monad.State (StateT, lift, runStateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Writer (Writer, tell)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import qualified SIR
import qualified Type

data Error
    = Tuple1
    | Tuple0
    | NoMain
    | MultipleMains
    deriving Show

data DeclAt = DeclAt | ImplicitPrim deriving Show

type SIRStage = (String, (), (), String, (), String, (), (), ())

type SIR = SIR.SIR SIRStage

type Module = SIR.Module SIRStage
type Binding = SIR.Binding SIRStage
type ADT = Type.ADT (TypeExpr, ())
type TypeSynonym = Type.TypeSynonym (TypeExpr, ())
type TypeExpr = SIR.TypeExpr SIRStage
type Expr = SIR.Expr SIRStage
type Pattern = SIR.Pattern SIRStage
type Variable = SIR.Variable SIRStage

type ModuleArena = Arena.Arena Module SIR.ModuleKey
type ADTArena = Arena.Arena ADT Type.ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym Type.TypeSynonymKey
type VariableArena = Arena.Arena Variable SIR.VariableKey
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

type MakeIRState = StateT (ModuleArena, ADTArena, TypeSynonymArena, QuantVarArena, VariableArena) (Writer [Error])

new_module :: Module -> MakeIRState SIR.ModuleKey
new_module m =
    State.state $ \(mods, adts, type_synonyms, type_vars, variables) ->
        let (key, mods') = Arena.put m mods
        in (key, (mods', adts, type_synonyms, type_vars, variables))

new_adt :: ADT -> MakeIRState Type.ADTKey
new_adt adt =
    State.state $ \(mods, adts, type_synonyms, type_vars, variables) ->
        let (key, adts') = Arena.put adt adts
        in (key, (mods, adts', type_synonyms, type_vars, variables))

new_type_synonym :: TypeSynonym -> MakeIRState Type.TypeSynonymKey
new_type_synonym ts =
    State.state $ \(mods, adts, type_synonyms, type_vars, variables) ->
        let (key, type_synonyms') = Arena.put ts type_synonyms
        in (key, (mods, adts, type_synonyms', type_vars, variables))

new_type_var :: String -> MakeIRState Type.QuantVarKey
new_type_var name =
    State.state $ \(mods, adts, type_synonyms, type_vars, variables) ->
        let (key, type_vars') = Arena.put (Type.QuantVar name) type_vars
        in (key, (mods, adts, type_synonyms, type_vars', variables))

new_variable :: Variable -> MakeIRState SIR.VariableKey
new_variable var =
    State.state $ \(mods, adts, type_synonyms, type_vars, variables) ->
        let (key, variables') = Arena.put var variables
        in (key, (mods, adts, type_synonyms, type_vars, variables'))

tell_error :: Error -> MakeIRState ()
tell_error = lift . tell . (: [])

convert :: [AST.Decl] -> Writer [Error] SIR
convert decls = do
    (root_module, (mods, adts, type_synonyms, type_vars, variables)) <-
        runStateT
            ( convert_decls decls >>= \(bindings, adts, type_synonyms) ->
                new_module (SIR.Module bindings adts type_synonyms)
            )
            (Arena.new, Arena.new, Arena.new, Arena.new, Arena.new)
    main_function <- search_for_main_function mods variables root_module
    pure (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function))

search_for_main_function :: ModuleArena -> VariableArena -> SIR.ModuleKey -> Writer [Error] (Maybe SIR.VariableKey)
search_for_main_function mods variables mod =
    let (SIR.Module bindings _ _) = Arena.get mods mod
        variables_called_main =
            bindings
                & concatMap (\(SIR.Binding pat _) -> go_pat pat)
    in case variables_called_main of
        [] -> do
            tell [NoMain]
            pure Nothing
        [main] -> pure $ Just main
        _ -> do
            tell [MultipleMains]
            pure Nothing
    where
        go_pat :: SIR.Pattern stage -> [SIR.VariableKey]
        go_pat (SIR.Pattern'Identifier _ vk) = go_var vk
        go_pat (SIR.Pattern'Wildcard _) = []
        go_pat (SIR.Pattern'Tuple _ a b) = go_pat a ++ go_pat b
        go_pat (SIR.Pattern'Named _ vk subpat) = go_var vk ++ go_pat subpat
        go_pat (SIR.Pattern'AnonADTVariant _ _ _ _ field_pats) = concatMap go_pat field_pats
        go_pat (SIR.Pattern'NamedADTVariant _ _ _ _ field_pats) = concatMap (go_pat . snd) field_pats
        go_pat (SIR.Pattern'Poison _) = []

        go_var vk =
            let (SIR.Variable _ name) = Arena.get variables vk
            in if name == "main" then [vk] else []

convert_decls :: [AST.Decl] -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey])
convert_decls decls =
    unzip3 <$> mapM convert_decl decls >>= \(bindings, adts, type_synonyms) ->
        pure (concat bindings, concat adts, concat type_synonyms)
    where
        convert_decl :: AST.Decl -> MakeIRState ([Binding], [Type.ADTKey], [Type.TypeSynonymKey])
        convert_decl (AST.Decl'Value target _ expr) =
            convert_expr expr >>= \expr' ->
                convert_pattern target >>= \target' ->
                    pure ([SIR.Binding target' expr'], [], [])
        convert_decl (AST.Decl'Data data_name type_params variants) =
            runMaybeT
                ( mapM (lift . new_type_var) type_params >>= \ty_param_vars ->
                    mapM convert_variant variants >>= \variants_converted ->
                        let adt = Type.ADT data_name ty_param_vars variants_converted
                        in lift (new_adt adt) >>= \adt_key ->
                            pure adt_key
                )
                >>= \case
                    Just adt_key -> pure ([], [adt_key], [])
                    Nothing -> pure ([], [], [])
        convert_decl (AST.Decl'TypeSyn name expansion) =
            runMaybeT
                ( lift (convert_type expansion) >>= \expansion' ->
                    lift (new_type_synonym (Type.TypeSynonym name (expansion', ())))
                )
                >>= \case
                    Just syn_key -> pure ([], [], [syn_key])
                    Nothing -> pure ([], [], [])

        convert_variant (AST.DataVariant'Anon variant_name fields) =
            Type.Variant'Anon variant_name
                <$> mapM
                    ( \ty_ast ->
                        lift (convert_type ty_ast) >>= \ty ->
                            pure (ty, ())
                    )
                    fields
        convert_variant (AST.DataVariant'Named variant_name fields) =
            Type.Variant'Named variant_name
                -- TODO: check no duplicate field names
                <$> mapM
                    ( \(field_name, ty_ast) ->
                        lift (convert_type ty_ast) >>= \ty ->
                            pure (field_name, (ty, ()))
                    )
                    fields

convert_type :: AST.Type -> MakeIRState TypeExpr
convert_type (AST.Type'Refer id) = pure $ SIR.TypeExpr'Refer () id
convert_type (AST.Type'Get prev name) = convert_type prev >>= \prev -> pure (SIR.TypeExpr'Get () prev name)
convert_type (AST.Type'Tuple items) = mapM convert_type items >>= group_items
    where
        -- TODO: better spans for this
        group_items [a, b] = pure $ SIR.TypeExpr'Tuple () a b
        group_items (a : b : more) = SIR.TypeExpr'Tuple () a <$> group_items (b : more)
        group_items [_] = tell_error Tuple1 >> pure (SIR.TypeExpr'Poison ())
        group_items [] = tell_error Tuple0 >> pure (SIR.TypeExpr'Poison ())
convert_type (AST.Type'Hole id) = pure $ SIR.TypeExpr'Hole () () id
convert_type (AST.Type'Function arg res) = SIR.TypeExpr'Function () <$> convert_type arg <*> convert_type res
convert_type (AST.Type'Forall tys ty) =
    mapM new_type_var tys >>= \case
        [] -> convert_type ty -- can happen if the user passed none
        tyv1 : tyv_more -> SIR.TypeExpr'Forall () (tyv1 :| tyv_more) <$> convert_type ty
convert_type (AST.Type'Apply ty args) =
    convert_type ty >>= \ty ->
        foldlM (\ty arg -> SIR.TypeExpr'Apply () ty <$> convert_type arg) ty args -- TODO: fix spans
convert_type AST.Type'Wild = pure $ SIR.TypeExpr'Wild ()

convert_expr :: AST.Expr -> MakeIRState Expr
convert_expr (AST.Expr'ReferAlpha t iden) = SIR.Expr'Identifier () <$> make_split_identifier t iden <*> pure ()
convert_expr (AST.Expr'Char c) = pure (SIR.Expr'Char () c)
convert_expr (AST.Expr'String s) = pure (SIR.Expr'String () s)
convert_expr (AST.Expr'Int i) = pure (SIR.Expr'Int () i)
convert_expr (AST.Expr'Float f) = pure (SIR.Expr'Float () f)
convert_expr (AST.Expr'Bool b) = pure (SIR.Expr'Bool () b)
convert_expr (AST.Expr'Tuple items) = group_items items
    where
        group_items [a, b] =
            convert_expr a >>= \a -> convert_expr b >>= \b -> pure (SIR.Expr'Tuple () a b)
        group_items (a : b : more) = convert_expr a >>= \a -> SIR.Expr'Tuple () a <$> group_items (b : more) -- TODO: properly do span of b:more because this just takes the span of the whole thing
        group_items [_] = tell_error Tuple1 >> pure (SIR.Expr'Poison ())
        group_items [] = tell_error Tuple0 >> pure (SIR.Expr'Poison ())
convert_expr (AST.Expr'Lambda params body) = convert_lambda params body
    where
        convert_lambda (param : more) body =
            convert_pattern param >>= \param ->
                SIR.Expr'Lambda () param <$> convert_lambda more body -- TODO: properly do spans of parts because this also just takes the whole span
        convert_lambda [] body = convert_expr body
convert_expr (AST.Expr'Let decls subexpr) = go decls
    where
        go [] = convert_expr subexpr
        go (first : more) =
            convert_decls [first] >>= \(bindings, adts, type_synonyms) ->
                SIR.Expr'Let () bindings adts type_synonyms <$> go more
convert_expr (AST.Expr'LetRec decls subexpr) =
    convert_decls decls >>= \(bindings, adts, type_synonyms) ->
        SIR.Expr'LetRec () bindings adts type_synonyms <$> convert_expr subexpr
convert_expr (AST.Expr'Where subexpr decls) =
    convert_decls decls >>= \(bindings, adts, type_synonyms) ->
        SIR.Expr'LetRec () bindings adts type_synonyms <$> convert_expr subexpr
convert_expr (AST.Expr'BinaryOps first ops) =
    SIR.Expr'BinaryOps () ()
        <$> convert_expr first
        <*> mapM
            ( \(op, right) -> do
                right' <- convert_expr right
                op_split_iden <- convert_operator op
                pure (op_split_iden, (), right')
            )
            ops
convert_expr (AST.Expr'Call callee args) =
    convert_expr callee >>= \callee ->
        foldlM
            ( \callee arg ->
                convert_expr arg >>= \arg ->
                    pure (SIR.Expr'Call () callee arg)
            )
            callee
            args
convert_expr (AST.Expr'If _ cond t f) =
    SIR.Expr'If ()
        <$> convert_expr cond
        <*> convert_expr t
        <*> convert_expr f
convert_expr (AST.Expr'Match _ e arms) =
    convert_expr e >>= \e ->
        mapM
            ( \(pat, choice) ->
                convert_pattern pat >>= \pat ->
                    convert_expr choice >>= \choice ->
                        pure (pat, choice)
            )
            arms
            >>= \arms ->
                pure (SIR.Expr'Match () e arms)
convert_expr (AST.Expr'TypeAnnotation ty e) = SIR.Expr'TypeAnnotation () <$> ((,()) <$> convert_type ty) <*> convert_expr e
convert_expr (AST.Expr'Forall tys e) =
    mapM new_type_var tys >>= \case
        [] -> convert_expr e
        tyv1 : tyv_more -> SIR.Expr'Forall () (tyv1 :| tyv_more) <$> convert_expr e
convert_expr (AST.Expr'TypeApply e args) =
    convert_expr e >>= \e ->
        foldlM
            ( \e arg ->
                convert_type arg >>= \arg ->
                    pure (SIR.Expr'TypeApply () e (arg, ()))
            )
            e
            args -- TODO: fix span for this
convert_expr (AST.Expr'Hole hid) = pure (SIR.Expr'Hole () hid)

convert_pattern :: AST.Pattern -> MakeIRState Pattern
convert_pattern (AST.Pattern'AlphaVar name) =
    new_variable (SIR.Variable () name) >>= \bn ->
        pure (SIR.Pattern'Identifier () bn)
convert_pattern (AST.Pattern'Wildcard _) = pure (SIR.Pattern'Wildcard ())
convert_pattern (AST.Pattern'Tuple subpats) = mapM convert_pattern subpats >>= go
    where
        go [a, b] = pure $ SIR.Pattern'Tuple () a b
        go (a : b : more) = SIR.Pattern'Tuple () a <$> go (b : more)
        go [_] = tell_error Tuple1 >> pure (SIR.Pattern'Poison ())
        go [] = tell_error Tuple0 >> pure (SIR.Pattern'Poison ())
convert_pattern (AST.Pattern'NamedAlpha name _ subpat) =
    convert_pattern subpat >>= \subpat' ->
        new_variable (SIR.Variable () name) >>= \bn ->
            pure (SIR.Pattern'Named () bn subpat')
convert_pattern (AST.Pattern'AnonADTVariant v_ty variant fields) =
    mapM convert_pattern fields >>= \fields ->
        make_split_identifier v_ty variant >>= \variant_split_iden ->
            pure (SIR.Pattern'AnonADTVariant () variant_split_iden () [] fields)
convert_pattern (AST.Pattern'NamedADTVariant v_ty variant fields) =
    mapM
        ( \(field_name, field_pat) ->
            convert_pattern field_pat >>= \field_pat ->
                pure (field_name, field_pat)
        )
        fields
        >>= \fields ->
            make_split_identifier v_ty variant >>= \variant_split_iden ->
                pure (SIR.Pattern'NamedADTVariant () variant_split_iden () [] fields)

convert_operator :: AST.Operator -> MakeIRState (SIR.SplitIdentifier SIRStage String)
convert_operator (AST.Operator'Path t i) = make_split_identifier (Just t) i
convert_operator (AST.Operator'Single i) = make_split_identifier Nothing i

make_split_identifier :: Maybe AST.Type -> String -> MakeIRState (SIR.SplitIdentifier SIRStage String)
make_split_identifier Nothing i = pure $ SIR.SplitIdentifier'Single i
make_split_identifier (Just ty) i = do
    ty <- convert_type ty
    pure $ SIR.SplitIdentifier'Get ty i
