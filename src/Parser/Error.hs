
module Parser.Error
    ( Error (..)
    ) where

import qualified Lexer (Token (..))

data Error = BadToken Int Lexer.Token
    deriving (Show, Eq)
