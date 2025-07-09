{
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Lexer (Lexer.lex, Token (..), TokenType (..), to_token_type) where

import qualified Language.Haskell.TH.Syntax as TH.Syntax (Lift)
import Data.Data (Data)
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z_]       -- alphabetic characters
-- $sym = [~!@#\$\%\^&\*\+\`-=\|:\.\/\<\>\?\\]
$sym = [\*]

tokens :-

    "//".* ;
  $white+                        ;
    \( { \_ -> OParen }
    \) { \_ -> CParen }
    \[ { \_ -> OBrack }
    \] { \_ -> CBrack }
    \{ { \_ -> OBrace }
    \} { \_ -> CBrace }
    ";" { \_ -> Semicolon }
    \, { \_ -> Comma }
    '.' { \s -> Char (s !! 1 ) }
    \".*\" { \s -> String s } -- this is not actually correct because it includes the quotes
    $digit+ { \s -> Int (read s) }
    $digit+\.$digit+ { \s -> Float (read s) }
    true { \_ -> Bool True }
    false { \_ -> Bool False }
    = { \ _ -> Equal }
    : { const Colon }
    \-> { const Arrow }
    \# { const Hash }
    \@ { const At }
    \? { const At }
    \\ { const Backslash }
    :: { const DoubleColon }
    \^ { const Caret }
    \` { const Backtick }
    _ { const Underscore }
    root { const Root }
    let { const Let }
    letrec { const LetRec }
    where { const Where }
    typesyn { const TypeSyn }
    data { const Data }
    impl { const Impl }
    if { const If }
    then { const Then }
    else { const Else }
    match { const Match }
    $sym+ { SymbolIdentifier }
    $alpha [$alpha $digit]* { AlphaIdentifier }
    $alpha [$alpha $digit]*: { KeywordIdentifier }

{
data Token
    = OParen
    | CParen
    | OBrack
    | CBrack
    | OBrace
    | CBrace
    | Semicolon
    | Comma
    | Char Char
    | String String
    | Int Integer
    | Float Float
    | Bool Bool
    | SymbolIdentifier String
    | AlphaIdentifier String
    | KeywordIdentifier String
    | Equal
    | Colon
    | Arrow
    | Hash
    | At
    | Question
    | Backslash
    | DoubleColon
    | Caret
    | Backtick
    | Underscore
    | Root
    | Let
    | LetRec
    | Where
    | TypeSyn
    | Data
    | Impl
    | If
    | Then
    | Else
    | Match
    | EOF
  deriving (Eq, Ord, Show, TH.Syntax.Lift, Data)

data TokenType
    = TT'OParen
    | TT'CParen
    | TT'OBrack
    | TT'CBrack
    | TT'OBrace
    | TT'CBrace
    | TT'Semicolon
    | TT'Comma
    | TT'Char
    | TT'String
    | TT'Int
    | TT'Float
    | TT'Bool
    | TT'SymbolIdentifier
    | TT'AlphaIdentifier
    | TT'KeywordIdentifier
    | TT'Equal
    | TT'Colon
    | TT'Arrow
    | TT'Hash
    | TT'At
    | TT'Question
    | TT'Backslash
    | TT'DoubleColon
    | TT'Caret
    | TT'Backtick
    | TT'Underscore
    | TT'Root
    | TT'Let
    | TT'LetRec
    | TT'Where
    | TT'TypeSyn
    | TT'Data
    | TT'Impl
    | TT'If
    | TT'Then
    | TT'Else
    | TT'Match
    | TT'EOF
  deriving (Eq, Ord, Show, TH.Syntax.Lift, Data)

to_token_type OParen = TT'OParen
to_token_type CParen = TT'CParen
to_token_type OBrack = TT'OBrack
to_token_type CBrack = TT'CBrack
to_token_type OBrace = TT'OBrace
to_token_type CBrace = TT'CBrace
to_token_type Semicolon = TT'Semicolon
to_token_type Comma = TT'Comma
to_token_type (Char _) = TT'Char
to_token_type (String _) = TT'String
to_token_type (Int _) = TT'Int
to_token_type (Float _) = TT'Float
to_token_type (Bool _) = TT'Bool
to_token_type (SymbolIdentifier _) = TT'SymbolIdentifier
to_token_type (AlphaIdentifier _) = TT'AlphaIdentifier
to_token_type (KeywordIdentifier _) = TT'KeywordIdentifier
to_token_type Equal = TT'Equal
to_token_type Colon = TT'Colon
to_token_type Arrow = TT'Arrow
to_token_type Hash = TT'Hash
to_token_type At = TT'At
to_token_type Question = TT'Question
to_token_type Backslash = TT'Backslash
to_token_type DoubleColon = TT'DoubleColon
to_token_type Caret = TT'Caret
to_token_type Backtick = TT'Backtick
to_token_type Underscore = TT'Underscore
to_token_type Root = TT'Root
to_token_type Let = TT'Let
to_token_type LetRec = TT'LetRec
to_token_type Where = TT'Where
to_token_type TypeSyn = TT'TypeSyn
to_token_type Data = TT'Data
to_token_type Impl = TT'Impl
to_token_type If = TT'If
to_token_type Then = TT'Then
to_token_type Else = TT'Else
to_token_type Match = TT'Match
to_token_type EOF = TT'EOF

lex = alexScanTokens
}
