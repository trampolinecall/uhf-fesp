{-# LANGUAGE TemplateHaskell #-}

-- utilities for dealing with precedence in pretty printers
module PP.Precedence
    ( PPGivenCurrentAndNextLevels
    , Levels
    , pp_precedence
    , PPGivenCurrentAndNextLevelsM
    , LevelsM
    , pp_precedence_m
    , parenthesize
    ) where

import Data.Functor.Identity (Identity (..), runIdentity)

import qualified PP

type PPGivenCurrentAndNextLevels thing = (thing -> PP.Token) -> (thing -> PP.Token) -> PP.Token
type PPGivenCurrentAndNextLevelsM m thing = (thing -> m PP.Token) -> (thing -> m PP.Token) -> m PP.Token
type LevelsM m thing = thing -> (Int, PPGivenCurrentAndNextLevelsM m thing)
type Levels thing = thing -> (Int, PPGivenCurrentAndNextLevels thing)

pp_precedence_m :: Functor m => LevelsM m thing -> (PP.Token -> PP.Token) -> thing -> m PP.Token
pp_precedence_m levels brackets = helper 0
    where
        helper current_precedence thing =
            let (level, pp) = levels thing
            in if level >= current_precedence
                then pp (helper level) (helper (level + 1))
                else brackets <$> pp (helper 0) (helper 1)

pp_precedence :: Levels thing -> (PP.Token -> PP.Token) -> thing -> PP.Token
pp_precedence levels brackets thing = runIdentity $ pp_precedence_m levels_m brackets thing
    where
        levels_m = (\(level, pp) -> (level, \cur next -> pure $ pp (runIdentity . cur) (runIdentity . next))) . levels

parenthesize :: PP.Token -> PP.Token
parenthesize x = PP.List ["(", x, ")"]
