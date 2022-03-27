{-# LANGUAGE OverloadedStrings #-}
-- |Formatter with a template
module Formatter where

import Data.Text (Text, unpack)
import Data.Map.Strict (Map, (!?), fromList)
import qualified Data.Map.Strict as M
import Control.Monad.Except

data Token = Plain Text      -- ^Plain text
           | Var Text [Text] -- ^Substitute variable
           deriving (Show)

newtype Substituter = Substituter (Text -> Maybe SubstAct)

newtype SubstAct = SubstAct ([Text] -> SubstituteMonad Text)

data Substitution = Substitution Text SubstAct

type SubstituteMonad = Either String

substitute :: Substituter -> [Token] -> SubstituteMonad Text
substitute s xs = mconcat <$> traverse (substituteOne s) xs

substituteOne :: Substituter -> Token -> SubstituteMonad Text
substituteOne _ (Plain a) = pure a
substituteOne (Substituter s) (Var name args) = case s name of
  Nothing  -> throwError $ "Unknown variable: " <> unpack name
  Just (SubstAct a) -> a args

toSubstituter :: [Substitution] -> Substituter
toSubstituter handlers = Substituter (m !?)
  where m = fromList $ map (\(Substitution k v) -> (k, v)) handlers

f0 :: Text -> Text -> Substitution
f0 key value = Substitution key (SubstAct f)
  where f :: [Text] -> SubstituteMonad Text
        f [] = pure value
        f _  = throwError $ "Too many arguments to " <> unpack key

f1 :: Text -> (Text -> SubstituteMonad Text) -> Substitution
f1 key func = Substitution key (SubstAct f)
  where f :: [Text] -> SubstituteMonad Text
        f [arg] = func arg
        f _  = throwError $ "Invalid number of arguments in " <> unpack key
