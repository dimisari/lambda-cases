module Experimental.Types2 where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.List (intercalate)
import Helpers ((==>))

-- Type

data Type =
  ProdToProd ProdType (Maybe ToProdType) |
  ParenToProd ParenCommaSepTs ToProdType

instance Show Type where
  show = \case
    ProdToProd prod_t maybe_prod_t ->
      show prod_t ++ case maybe_prod_t of
        Just to_prod_t -> show to_prod_t
        Nothing -> ""
    ParenToProd paren_type_types to_prod_t ->
      show paren_type_types ++ show to_prod_t

type_p =
  ProdToProd <$> prod_type_p <*> optionMaybe (to_prod_type_p) <|>
  ParenToProd <$> paren_comma_sep_ts_p <*> to_prod_type_p
  :: Parser Type

-- ProdType

data ProdType =
  ProdType TypeApp [ TypeApp ]

instance Show ProdType where
  show = \(ProdType type_app1 type_apps) ->
    intercalate " x " $ map show $ type_app1 : type_apps

prod_type_p =
  ProdType <$> type_app_p <*> many (try (string " x ") *> type_app_p)
  :: Parser ProdType

-- TypeApp

data TypeApp =
  TypeApp TypeAppBegin [ LeftOrRightApp ]

instance Show TypeApp where
  show = \(TypeApp type_app_begin left_or_right_apps) ->
    show type_app_begin ++ concatMap show left_or_right_apps

type_app_p = 
  TypeApp <$> type_app_begin_p <*> many left_or_right_app_p
  :: Parser TypeApp

-- TypeAppBegin

data TypeAppBegin =
  ParenTOrTName ParenTOrTName | PCSTsRTP ParenCommaSepTs RightTApp

instance Show TypeAppBegin where
  show = \case
    ParenTOrTName paren_t_or_t_name -> show paren_t_or_t_name
    PCSTsRTP paren_comma_set_ts right_t_app ->
      show paren_comma_set_ts ++ show right_t_app

type_app_begin_p = 
  ParenTOrTName <$> paren_t_or_t_name_p <|>
  PCSTsRTP <$> paren_comma_sep_ts_p <*> right_t_app_p
  :: Parser TypeAppBegin

-- LeftOrRightApp

data LeftOrRightApp =
  LeftApp LeftTApp | RightApp RightTApp

instance Show LeftOrRightApp where
  show = \case
    LeftApp left_t_app -> show left_t_app
    RightApp right_t_app -> show right_t_app

left_or_right_app_p =
  LeftApp <$> left_t_app_p <|> RightApp <$> right_t_app_p
  :: Parser LeftOrRightApp

-- LeftTApp

data LeftTApp =
  LeftTApp ParenTsOrTName

instance Show LeftTApp where
  show = \(LeftTApp paren_ts_or_t_name) ->
    "<==" ++ show paren_ts_or_t_name

left_t_app_p = 
  LeftTApp <$> (string "<==" *> paren_ts_or_t_name_p)
  :: Parser LeftTApp

-- RightTApp

data RightTApp =
  RightTApp ParenTOrTName

instance Show RightTApp where
  show = \(RightTApp paren_t_or_t_name) ->
    "==>" ++ show paren_t_or_t_name

right_t_app_p = 
  RightTApp <$> (string "==>" *> paren_t_or_t_name_p)
  :: Parser RightTApp

-- ParenTsOrTName

data ParenTsOrTName =
  ParenTs Types | TName1 TypeName

instance Show ParenTsOrTName where
  show = \case
    ParenTs types -> "(" ++ show types ++ ")"
    TName1 type_name -> show type_name

paren_ts_or_t_name_p = 
  ParenTs <$> (char '(' *> types_p <* char ')') <|> TName1 <$> type_name_p
  :: Parser ParenTsOrTName

-- ToProdType

data ToProdType =
  ToProdType ProdType

instance Show ToProdType where
  show = \(ToProdType prod_type) -> " -> " ++ show prod_type

to_prod_type_p =
  ToProdType <$> (try (string " -> ") *> prod_type_p)
  :: Parser ToProdType

-- ParenCommaSepTs

data ParenCommaSepTs =
  ParenCommaSepTs OpenParT Types

instance Show ParenCommaSepTs where
  show = \(ParenCommaSepTs open_par_t ts) ->
    show open_par_t ++ ", " ++ show ts ++ ")"

paren_comma_sep_ts_p =
  ParenCommaSepTs <$> open_par_t_p <*> (string ", " *> types_p <* char ')')
  :: Parser ParenCommaSepTs

parse_pcsts = parse paren_comma_sep_ts_p ""
  :: String -> Either ParseError ParenCommaSepTs

-- ParenTOrTName

data ParenTOrTName =
  ParenT OpenParT | TName2 TypeName

instance Show ParenTOrTName where
  show = \case
    ParenT open_par_t -> show open_par_t ++ ")"
    TName2 type_name -> show type_name

paren_t_or_t_name_p = 
  ParenT <$> (open_par_t_p <* char ')') <|> TName2 <$> type_name_p
  :: Parser ParenTOrTName

-- OpenParT

data OpenParT =
  OpenParT Type

instance Show OpenParT where
  show = \(OpenParT t) -> "(" ++ show t

open_par_t_p =
  OpenParT <$> (char '(' *> type_p)
  :: Parser OpenParT

-- Types

data Types =
  CommaSepTypes Type [ Type ]

instance Show Types where
  show = \(CommaSepTypes t ts) ->
    intercalate ", " $ map show $ t : ts 

types_p = 
  CommaSepTypes <$> type_p <*> many (string ", " *> type_p)
  :: Parser Types

-- TypeName

newtype TypeName =
  TN String deriving (Eq, Ord)

instance Show TypeName where
  show = \(TN name) -> name

type_name_p = TN <$> ((:) <$> upper <*> many (lower <|> upper))
  :: Parser TypeName

-- 

parse_type = parse type_p ""
  :: String -> Either ParseError Type
