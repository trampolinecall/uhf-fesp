{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PP.IndentationMonad
    ( PP
    , exec_pp
    , first_on_line
    , write
    , indent
    , dedent
    ) where

-- TODO: write tests

import Control.Monad.State.Strict
import Control.Monad.Writer.CPS
import Data.List (intersperse)

data IndentState = IndentState Int Bool
newtype PP a = PP (StateT IndentState (Writer String) a) deriving (Functor, Applicative, Monad)

first_on_line :: PP Bool
first_on_line = PP $ get >>= \(IndentState _ is_first) -> pure is_first

exec_pp :: PP a -> String
exec_pp (PP d) = execWriter $ evalStateT d (IndentState 0 True)

write :: String -> PP ()
write t =
    let chunks = split (== '\n') t
    in mapM_ put_chunk $ intersperse Nothing (map Just chunks)
    where
        put_chunk Nothing =
            PP $
                lift (tell "\n")
                    >> modify (\(IndentState cur_indent _) -> IndentState cur_indent True)
        put_chunk (Just c)
            | null c = pure ()
            | otherwise =
                PP $
                    get
                        >>= ( \case
                                IndentState _ True -> put_indent
                                _ -> pure ()
                            )
                        >> modify (\(IndentState cur_indent _) -> IndentState cur_indent False)
                        >> lift (tell c)

        put_indent = get >>= \(IndentState indent _) -> lift (tell $ replicate (indent * 4) ' ')

split :: (Char -> Bool) -> String -> [[Char]]
split pred = go [[]]
    where
        go acc (x : more)
            | pred x = go (acc ++ [[]]) more
            | otherwise =
                go (init acc ++ [last acc ++ [x]]) more
        go acc [] = acc

indent :: PP ()
indent = PP $ modify $ \(IndentState indent at_start) -> IndentState (indent + 1) at_start
dedent :: PP ()
dedent = PP $ modify $ \(IndentState indent at_start) -> IndentState (indent - 1) at_start
