{-# LANGUAGE OverloadedStrings #-}
-- |Formatter with a template.
module Formatter ( substitute
                 , toSubstituter
                 , f0
                 , f1
                 ) where

import Control.Applicative
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Map.Strict (Map, (!?), fromList)
import Data.Text (Text, unpack)
import qualified Data.Text as T

newtype Substituter = Substituter (Text -> Maybe SubstAct)

newtype SubstAct = SubstAct ([Text] -> Parser Text)

data Substitution = Substitution Text SubstAct

-- |Try to substitute variables in a template. Returns Right with the
-- text when substitution succeeds or Left with the position where the
-- error occured.
substitute :: Substituter -> Text -> Either Int Text
substitute subst template = case feed (parse (everything subst) template) "" of
  Done i r -> if i == mempty then Right r else failed i
  Fail i _ _ -> failed i
  where failed i = Left (T.length template - T.length i)

-- |Parses and substitutes variables on the go.
everything :: Substituter -> Parser Text
everything subst = mconcat <$> p
  where
    p = many (plain <|> s) <* endOfInput
    s = var >>= varSubst subst

plain :: Parser Text
plain = takeText1before "{{"

var :: Parser [Text]
var = "{{" *> takeText1before (eitherP sep "}}") `sepBy1` sep <* "}}"
  where sep = char '|'

-- |Substitutes a variable from given list of arguments.
varSubst :: Substituter -> [Text] -> Parser Text
varSubst _ [] = fail "Empty variable name"
varSubst (Substituter s) (name:xs) = case s name of
  Nothing             -> fail $ "Unknown variable: " <> unpack name
  Just (SubstAct act) -> act xs

-- |This takes one or many chars as Text until it matches the given
-- "end" parser. Doesn't consume "end".
takeText1before :: Parser a -> Parser Text
takeText1before end = takeMany1text anyCharBefore
  where takeMany1text = fmap fst . match . skipMany1
        anyCharBefore = eitherP (lookAhead end) anyChar >>= either (const empty) pure

toSubstituter :: [Substitution] -> Substituter
toSubstituter handlers = Substituter (m !?)
  where m = fromList $ map (\(Substitution k v) -> (k, v)) handlers

-- |Argumentless replacement
f0 :: Text -> Text -> Substitution
f0 key value = Substitution key (SubstAct f)
  where f [] = pure value
        f _  = fail $ "No arguments were expected to " <> unpack key

-- |Replacement with one argument
f1 :: Text -> (Text -> Parser Text) -> Substitution
f1 key func = Substitution key (SubstAct f)
  where f [arg] = func arg
        f _     = fail $ "Expecting exactly one argument to " <> unpack key
