{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -fshow-error-context #-}

module Parser
    ( parse
    , Error.Error
    ) where

import AST
import Lexer (Token (..), TokenType (..))
import qualified Parser.Error as Error
import qualified Parser.Generate as Generate
import Parser.Grammar (GrammarMonad, Nonterminal (..), Symbol, ToSymbol, get_nt_ty, make_grammar, nt, prod_join, to_symbol, toplevel, (-->))

$( let unwrap_right :: Show a => Either a b -> b
       unwrap_right (Left a) = error $ "unwrap_right on Left " ++ show a
       unwrap_right (Right a) = a
   in Generate.make_parse_fn
        "parse'"
        [t|[AST.Decl]|]
        ( Generate.generate_table $ unwrap_right $ make_grammar $ do
            let (.) :: (ToSymbol a, ToSymbol b) => a -> b -> [Symbol]
                (.) = prod_join
                infixr 6 .

                empty :: [Symbol]
                empty = []

                p |> m = (to_symbol p, m)
                infix 4 |>

                -- TODO: memoize these functions?
                list_star :: Nonterminal -> GrammarMonad Nonterminal
                list_star Augment = error "cannot make list of augment"
                list_star thing@(Nonterminal name) = do
                    thing_ty <- get_nt_ty thing

                    thing_list <- nt ("list of " <> name) [t|[$thing_ty]|]

                    thing_list --> thing_list . thing |> [|\l t -> l ++ [t]|]
                    thing_list --> empty |> [|[]|]

                    pure thing_list

                list_plus :: Nonterminal -> GrammarMonad Nonterminal
                list_plus Augment = error "cannot make list of augment"
                list_plus thing@(Nonterminal name) = do
                    thing_ty <- get_nt_ty thing

                    thing_list <- nt ("list of " <> name) [t|[$thing_ty]|]

                    thing_list --> thing_list . thing |> [|\l t -> l ++ [t]|]
                    thing_list --> thing |> [|(: [])|]

                    pure thing_list

                optional :: Nonterminal -> GrammarMonad Nonterminal
                optional Augment = error "cannot make optional augment"
                optional thing@(Nonterminal name) = do
                    thing_ty <- get_nt_ty thing

                    opt <- nt ("optional " <> name) [t|Maybe $thing_ty|]

                    opt --> thing |> [|Just|]
                    opt --> empty |> [|Nothing|]

                    pure opt

                list_sep_allow_trailing :: ToSymbol sep => sep -> Nonterminal -> GrammarMonad Nonterminal
                list_sep_allow_trailing _ Augment = error "cannot make separated list of augment"
                list_sep_allow_trailing sep thing@(Nonterminal name) = do
                    thing_ty <- get_nt_ty thing

                    list <- nt ("delimited list of " <> name) [t|[$thing_ty]|]
                    helper_list <- nt ("delimited list of " <> name <> " helper") [t|[$thing_ty]|]

                    list --> helper_list |> [|id|]
                    list --> helper_list . sep |> [|\l _ -> l|]
                    list --> empty |> [|[]|]

                    helper_list --> helper_list . sep . thing |> [|\l _ t -> l ++ [t]|]
                    helper_list --> thing |> [|(: [])|]

                    pure list

            -- TODO: rename all nonterminals
            decl <- nt "decl" [t|AST.Decl|]
            decl_list <- list_star decl >>= toplevel

            decl_data <- nt "decl_data" [t|AST.Decl|]

            decl_typesyn <- nt "decl_typesyn" [t|AST.Decl|]
            decl_binding <- nt "decl_binding" [t|AST.Decl|]

            type_ <- nt "type" [t|AST.Type|]
            type_forall <- nt "forall type" [t|AST.Type|]
            type_function <- nt "function type" [t|AST.Type|]
            type_apply <- nt "apply type" [t|AST.Type|]
            type_get <- nt "get type" [t|AST.Type|]
            type_primary <- nt "primary type" [t|AST.Type|]

            expr <- nt "expr" [t|AST.Expr|]
            expr_toplevel <- nt "toplevel expr" [t|AST.Expr|]
            expr_where <- nt "where expr" [t|AST.Expr|]
            expr_forall <- nt "forall expr" [t|AST.Expr|]
            expr_let <- nt "let expr" [t|AST.Expr|]
            expr_if <- nt "if expression" [t|AST.Expr|]
            expr_type_annotation <- nt "type annotation expression" [t|AST.Expr|]
            expr_lambda <- nt "lambda expression" [t|AST.Expr|]
            expr_keyword_call <- nt "keyword call expression" [t|AST.Expr|]
            expr_binary_ops <- nt "binary ops expression" [t|AST.Expr|]
            expr_call <- nt "call expression" [t|AST.Expr|]
            expr_primary <- nt "primary expression" [t|AST.Expr|]
            expr_refer <- nt "identifier expression" [t|AST.Expr|]
            expr_hole <- nt "hole expression" [t|AST.Expr|]
            expr_literal <- nt "literal expression" [t|AST.Expr|]
            expr_match <- nt "match expression" [t|AST.Expr|]
            expr_tuple <- nt "tuple expression" [t|AST.Expr|]

            operator <- nt "operator" [t|AST.Operator|]

            pattern <- nt "pattern" [t|AST.Pattern|]
            pattern_anon_variant <- nt "anonymous variant pattern" [t|AST.Pattern|]
            pattern_alpha_var <- nt "alpha variable pattern" [t|AST.Pattern|]
            pattern_named <- nt "named pattern" [t|AST.Pattern|] -- TODO: rename this to at-pattern?
            pattern_named_variant <- nt "named variant pattern" [t|AST.Pattern|]
            pattern_primary <- nt "primary pattern" [t|AST.Pattern|]
            pattern_toplevel <- nt "toplevel pattern" [t|AST.Pattern|]
            pattern_tuple <- nt "tuple pattern" [t|AST.Pattern|]
            pattern_wild <- nt "wild pattern" [t|AST.Pattern|]

            type_param_list <- nt "type parameter list" [t|[String]|] -- TODO: rename this?
            comma_sep_expr_list <- list_sep_allow_trailing TT'Comma expr
            comma_sep_type_list <- list_sep_allow_trailing TT'Comma type_
            comma_sep_pattern_list <- list_sep_allow_trailing TT'Comma pattern

            comma_sep_expr_list_at_least_one_comma <- nt "comma separated expression list with at least one comma" [t|[AST.Expr]|]
            comma_sep_type_list_at_least_one_comma <- nt "comma separated type list with at least one comma" [t|[AST.Type]|]
            comma_sep_pattern_list_at_least_one_comma <- nt "comma separated pattern list with at least one comma" [t|[AST.Pattern]|]

            -- alpha_iden_path <- nt "alpha identifier path" [t|AST.PathOrSingleIden|] -- TODO: remove this?
            -- symbol_iden_path <- nt "symbol identifier path" [t|AST.PathOrSingleIden|] -- TODO: remove this?
            -- keyword_iden_path <- nt "keyword identifier path" [t|AST.PathOrSingleIden|] -- TODO: remove this?

            pattern_named_list_at_least_once <- nt "named pattern list" [t|[AST.Pattern]|]

            decl --> decl_data |> [|id|]
            decl --> decl_typesyn |> [|id|]
            decl --> decl_binding |> [|id|]

            do
                data_variant <- nt "data_variant" [t|AST.DataVariant|]
                data_variant_list <- list_star data_variant

                do
                    anon_field <- nt "data declaration anonymous field" [t|AST.Type|]
                    field_list <- list_star anon_field

                    anon_field --> type_get |> [|id|]
                    data_variant
                        --> (TT'AlphaIdentifier . field_list . TT'Semicolon)
                        |> [|\(Lexer.AlphaIdentifier name) fields _ -> AST.DataVariant'Anon name fields|]

                do
                    named_field <- nt "data declaration named field" [t|(String, AST.Type)|]
                    field_list <- list_sep_allow_trailing TT'Comma named_field

                    named_field --> TT'AlphaIdentifier . TT'Colon . type_ |> [|\(Lexer.AlphaIdentifier a) _ t -> (a, t)|]
                    data_variant
                        --> (TT'AlphaIdentifier . TT'OBrace . field_list . TT'CBrace . TT'Semicolon)
                        |> [|\(Lexer.AlphaIdentifier name) _ fields _ _ -> AST.DataVariant'Named name fields|]

                decl_data
                    --> (TT'Data . TT'AlphaIdentifier . type_param_list . TT'OBrace . data_variant_list . TT'CBrace . TT'Semicolon)
                    |> [|\data_ (Lexer.AlphaIdentifier name) typarams _ variants _ semi -> AST.Decl'Data name typarams variants|]

            decl_typesyn
                --> (TT'TypeSyn . TT'AlphaIdentifier . TT'Equal . type_ . TT'Semicolon)
                |> [|\ts (Lexer.AlphaIdentifier name) _ ty semi -> AST.Decl'TypeSyn name ty|]

            decl_binding
                --> (pattern . TT'Equal . expr . TT'Semicolon)
                |> [|\p eq e semi -> AST.Decl'Value p AST.Token e|]

            expr --> expr_where |> [|id|]

            expr_where --> expr_toplevel |> [|id|]
            expr_where --> (expr_toplevel . TT'Where . decl) |> [|\e _ d -> AST.Expr'Where e [d]|]
            expr_where --> (expr_toplevel . TT'Where . TT'OBrace . decl_list . TT'CBrace) |> [|\e _ _ d (_) -> AST.Expr'Where e d|]

            expr_toplevel --> expr_forall |> [|id|]
            expr_toplevel --> expr_let |> [|id|]
            expr_toplevel --> expr_if |> [|id|]
            expr_toplevel --> expr_type_annotation |> [|id|]
            expr_toplevel --> expr_lambda |> [|id|]
            expr_toplevel --> expr_keyword_call |> [|id|]

            expr_forall --> (TT'Hash . TT'AlphaIdentifier . expr_toplevel) |> [|\_ (Lexer.AlphaIdentifier tv) e -> AST.Expr'Forall [tv] e|] -- TODO: remove the list around tv
            expr_let --> (TT'Let . TT'OBrace . decl_list . TT'CBrace . expr_toplevel) |> [|\_ _ ds _ e -> AST.Expr'Let ds e|]
            expr_let --> (TT'Let . decl . expr_toplevel) |> [|\_ d e -> AST.Expr'Let [d] e|]
            expr_let
                --> (TT'LetRec . TT'OBrace . decl_list . TT'CBrace . expr_toplevel)
                |> [|\_ _ ds _ e -> AST.Expr'LetRec ds e|]
            expr_let --> (TT'LetRec . decl . expr_toplevel) |> [|\_ d e -> AST.Expr'LetRec [d] e|]
            expr_type_annotation
                --> (TT'Colon . type_ . TT'Colon . expr_toplevel)
                |> [|\_ t _ e -> AST.Expr'TypeAnnotation t e|]
            expr_if
                --> (TT'If . expr_toplevel . TT'Then . expr_toplevel . TT'Else . expr_toplevel)
                |> [|\if_ c _ t _ f -> AST.Expr'If AST.Token c t f|]
            expr_lambda
                --> (TT'Backslash . pattern_named_list_at_least_once . TT'Arrow . expr_toplevel)
                |> [|\_ pats _ e -> AST.Expr'Lambda pats e|]

            do
                kw_iden_path <- nt "keyword identifier path" [t|AST.KeywordRef|]
                kw_iden_paths <- nt "keyword identifer paths" [t|[AST.KeywordRef]|]
                optional_kw_iden_paths <- optional kw_iden_paths

                kw_iden_path --> (TT'Caret . TT'KeywordIdentifier) |> [|\_ (Lexer.KeywordIdentifier ki) -> AST.KeywordRef'Single ki|]
                kw_iden_path
                    --> (TT'Caret . type_primary . TT'DoubleColon . TT'KeywordIdentifier)
                    |> [|\_ t _ (Lexer.KeywordIdentifier ki) -> AST.KeywordRef'Path t ki|]
                kw_iden_paths --> (kw_iden_path . kw_iden_paths) |> [|(:)|]
                kw_iden_paths --> kw_iden_path |> [|(: [])|]

                kw_call_middle <- nt "middle of keyword call" [t|(AST.Expr, [([AST.KeywordRef], AST.Expr)])|]
                kw_call_middle
                    --> (kw_call_middle . kw_iden_paths . expr_binary_ops)
                    |> [|\(first_arg, prev_args) paths arg -> (first_arg, prev_args ++ [(paths, arg)])|]
                kw_call_middle --> expr_binary_ops |> [|(,[])|]

                expr_keyword_call --> (kw_iden_paths . kw_call_middle . optional_kw_iden_paths) |> [|\first_paths args more_path -> undefined|]

            expr_keyword_call --> expr_binary_ops |> [|id|]

            do
                expr_binary_ops_helper <- nt "binary ops expression helper" [t|(AST.Expr, [(AST.Operator, AST.Expr)])|]

                expr_binary_ops
                    --> expr_binary_ops_helper
                    |> [|\(first, more) -> if null more then first else AST.Expr'BinaryOps first more|]

                expr_binary_ops_helper
                    --> (expr_call . operator . expr_binary_ops_helper)
                    |> [|\left operator (first_of_more, more) -> (left, (operator, first_of_more) : more)|]
                expr_binary_ops_helper --> expr_call |> [|(,[])|]

                -- TODO: operator --> TT'Backtick . alpha_iden_path . TT'Backtick |> [|todo|]
                operator --> TT'SymbolIdentifier |> [|\(Lexer.SymbolIdentifier s) -> AST.Operator'Single s|]
                operator
                    --> (TT'Backtick . type_primary . TT'DoubleColon . TT'SymbolIdentifier)
                    |> [|\_ t _ (Lexer.SymbolIdentifier s) -> AST.Operator'Path t s|]

            expr_call --> expr_primary |> [|id|]
            expr_call --> (expr_call . expr_primary) |> [|\c a -> AST.Expr'Call c [a]|] -- TODO: remove the list around a
            expr_call --> (expr_call . TT'Hash . type_get) |> [|\c _ t -> AST.Expr'TypeApply c [t]|] -- TODO: remove the list around t
            expr_primary --> expr_refer |> [|id|]
            expr_primary --> expr_hole |> [|id|]
            expr_primary --> expr_literal |> [|id|]
            expr_primary --> expr_match |> [|id|]
            expr_primary --> expr_tuple |> [|id|]
            expr_primary --> TT'OParen . expr_toplevel . TT'CParen |> [|\_ e _ -> e|]
            expr_refer --> TT'AlphaIdentifier |> [|\(Lexer.AlphaIdentifier a) -> AST.Expr'ReferAlpha Nothing a|]
            expr_refer
                --> (TT'OBrack . TT'OBrack . type_ . TT'CBrack . TT'CBrack . TT'DoubleColon . TT'AlphaIdentifier)
                |> [|\_ _ t _ _ _ (Lexer.AlphaIdentifier a) -> AST.Expr'ReferAlpha (Just t) a|]
            expr_hole --> (TT'Question . TT'AlphaIdentifier) |> [|\_ (Lexer.AlphaIdentifier i) -> AST.Expr'Hole i|]
            expr_literal --> TT'Char |> [|\((Lexer.Char c)) -> AST.Expr'Char c|]
            expr_literal --> TT'String |> [|\((Lexer.String s)) -> AST.Expr'String s|]
            expr_literal --> TT'Int |> [|\((Lexer.Int i)) -> AST.Expr'Int i|]
            expr_literal --> TT'Float |> [|\((Lexer.Float f)) -> AST.Expr'Float f|]
            expr_literal --> TT'Bool |> [|\((Lexer.Bool b)) -> AST.Expr'Bool b|]
            expr_tuple
                --> (TT'OParen . comma_sep_expr_list_at_least_one_comma . TT'CParen)
                |> [|\_ parts _ -> AST.Expr'Tuple parts|]
            do
                match_arm <- nt "match arm" [t|(AST.Pattern, AST.Expr)|]
                match_arm_list <- list_star match_arm
                match_arm --> (pattern . TT'Arrow . expr . TT'Semicolon) |> [|\p _ e _ -> (p, e)|]
                expr_match
                    --> (TT'Match . expr . TT'OBrace . match_arm_list . TT'CBrace)
                    |> [|\match scr _ arms _ -> AST.Expr'Match (AST.Token) scr arms|]

            type_ --> type_forall |> [|id|]

            type_forall --> type_function |> [|id|]
            type_forall --> TT'Hash . TT'AlphaIdentifier . type_forall |> [|\_ (Lexer.AlphaIdentifier iden) t -> AST.Type'Forall [iden] t|] -- TODO: remove this list around iden
            type_function --> type_apply . TT'Arrow . type_function |> [|\a _ b -> AST.Type'Function a b|]
            type_function --> type_apply |> [|id|]

            type_apply --> type_get |> [|id|]
            type_apply --> (type_apply . TT'Hash . type_get) |> [|\a _ b -> AST.Type'Apply a [b]|] -- TODO: remove the list around b
            type_get --> type_primary |> [|id|]
            type_get --> (type_get . TT'DoubleColon . TT'AlphaIdentifier) |> [|\t _ (Lexer.AlphaIdentifier i) -> AST.Type'Get t i|]

            type_primary --> TT'OParen . type_ . TT'CParen |> [|\_ t _ -> t|]
            type_primary --> TT'AlphaIdentifier |> [|\(Lexer.AlphaIdentifier i) -> AST.Type'Refer i|]
            type_primary --> TT'Underscore |> [|\_ -> AST.Type'Wild|]
            type_primary --> TT'Question . TT'AlphaIdentifier |> [|\_ (Lexer.AlphaIdentifier i) -> AST.Type'Hole i|]
            type_primary
                --> (TT'OParen . comma_sep_type_list_at_least_one_comma . TT'CParen)
                |> [|\_ parts _ -> AST.Type'Tuple parts|]

            pattern --> pattern_toplevel |> [|id|]

            pattern_toplevel --> pattern_anon_variant |> [|id|]
            pattern_toplevel --> pattern_named_variant |> [|id|]
            pattern_toplevel --> pattern_named |> [|id|]

            -- TODO: add support for paths in variant names
            pattern_anon_variant
                --> (TT'AlphaIdentifier . pattern_named_list_at_least_once)
                |> [|\(Lexer.AlphaIdentifier v) p -> AST.Pattern'AnonADTVariant Nothing v p|]
            do
                field_pattern <- nt "named variant pattern field" [t|(String, AST.Pattern)|]
                field_pattern --> (TT'AlphaIdentifier . TT'Equal . pattern) |> [|\(Lexer.AlphaIdentifier i) _ p -> (i, p)|]
                field_list <- list_sep_allow_trailing TT'Comma field_pattern

                -- TODO: add support for paths in variant names
                pattern_named_variant
                    --> (TT'AlphaIdentifier . TT'OBrace . field_list . TT'CBrace)
                    |> [|\(Lexer.AlphaIdentifier v) _ p _ -> AST.Pattern'NamedADTVariant Nothing v p|]

            pattern_named
                --> (TT'AlphaIdentifier . TT'At . pattern_named)
                |> [|\(Lexer.AlphaIdentifier name) at n -> AST.Pattern'NamedAlpha name AST.Token n|]
            pattern_named --> pattern_primary |> [|id|]

            pattern_primary --> pattern_alpha_var |> [|id|]
            pattern_primary --> pattern_wild |> [|id|]
            pattern_primary --> (TT'OParen . pattern . TT'CParen) |> [|\_ a _ -> a|]
            pattern_primary --> pattern_tuple |> [|id|]
            pattern_alpha_var --> TT'AlphaIdentifier |> [|\(Lexer.AlphaIdentifier i) -> AST.Pattern'AlphaVar i|]
            pattern_wild --> TT'Underscore |> [|\_ -> AST.Pattern'Wildcard AST.Token|]
            pattern_tuple
                --> (TT'OParen . comma_sep_pattern_list_at_least_one_comma . TT'CParen)
                |> [|\_ parts _ -> AST.Pattern'Tuple parts|]

            -- TODO: remove this? this is only kept because it might be used in fixing issue #30
            -- alpha_iden_path --> TT'AlphaIdentifier |> [|AST.PathOrSingleIden'Single|]
            -- alpha_iden_path --> TT'Root |> [|_|] TODO
            -- alpha_iden_path --> (alpha_iden_path . TT'DoubleColon . TT'AlphaIdentifier) |> [|todo|]
            -- alpha_iden_path --> (alpha_iden_path . TT'DoubleColon . TT'Hash . type_primary) |> [|todo|]
            -- alpha_iden_path --> (TT'OBrack . TT'OBrack . type_ . TT'CBrack . TT'CBrack . TT'DoubleColon . TT'AlphaIdentifier) |> [|todo|]

            type_param_list --> (type_param_list . TT'Hash . TT'AlphaIdentifier) |> [|\last _ (Lexer.AlphaIdentifier i) -> last ++ [i]|]
            type_param_list --> empty |> [|[]|]

            pattern_named_list_at_least_once --> (pattern_named_list_at_least_once . pattern_named) |> [|\ps p -> ps ++ [p]|]
            pattern_named_list_at_least_once --> pattern_named |> [|(: [])|]

            comma_sep_expr_list_at_least_one_comma --> (expr . TT'Comma . comma_sep_expr_list) |> [|\e _ more -> e : more|]
            comma_sep_type_list_at_least_one_comma --> (type_ . TT'Comma . comma_sep_type_list) |> [|\t _ more -> t : more|]
            comma_sep_pattern_list_at_least_one_comma --> (pattern . TT'Comma . comma_sep_pattern_list) |> [|\p _ more -> p : more|]

            pure ()
        )
 )

parse :: [Lexer.Token] -> Either Error.Error [AST.Decl]
parse = parse'
