module Conversions.Scheme where

import Text.Parsec (parse, ParseError)
import Parsing.Parsers.Types

import Control.Monad.State
import Data.Map as M

import Helpers ((==>), (.>))

import Parsing.Types.Types
import IntermediateTypes.Types

type VarMap = M.Map TypeName Int
   
data StateType =
  StateType { var_counter :: Int, var_map :: VarMap }

type Stateful = State StateType

get_v_map = var_map <$> get 
  :: Stateful VarMap

parse_to_scheme = ( \s -> to_scheme <$> parse value_type_p "" s )
  :: String -> Either ParseError TypeScheme

-- 

class ToStateValType a => ToScheme a where
  to_scheme :: a -> TypeScheme
  to_scheme = \a ->
    runState (to_s_val_t a) (StateType 0 M.empty)  ==> \(t, s) ->
    BoundVarsAndT [0..(var_counter s - 1)] t

instance ToScheme ValueType

-- ToStateValType

class ToStateValType a where
  to_s_val_t :: a -> Stateful ValType

instance ToStateValType ValueType where
  to_s_val_t = \case
    FunctionType func_t -> to_s_val_t func_t
    ProductType prod_t -> to_s_val_t prod_t
    TypeApplication type_app -> to_s_val_t type_app

instance ToStateValType FunctionType where
  to_s_val_t = \(InAndOutTypes input out_t) -> case input of 
    OneInputType in_t -> 
      FuncType <$> (InAndOutTs <$> to_s_val_t in_t <*> to_s_val_t out_t)
    MultipleInputTypes mult_ins -> 
      to_s_val_t (MultInTsType (ts_in_paren_to_value_types mult_ins) out_t)

data MultInTsType = MultInTsType [ ValueType ] OutputType

instance ToStateValType MultInTsType where
  to_s_val_t = \(MultInTsType in_ts out_t) -> case in_ts of
    [] -> to_s_val_t out_t
    in_t1 : in_ts_rest -> 
      FuncType <$>
      (InAndOutTs <$>
        (to_s_val_t in_t1) <*>
        (to_s_val_t $ MultInTsType in_ts_rest out_t)
      )

instance ToStateValType OutputType where
  to_s_val_t = \case
    OutputTypeApp type_app -> to_s_val_t type_app
    OutputProductType prod_t -> to_s_val_t prod_t

instance ToStateValType ProductType where
  to_s_val_t = \(ProductTypes val_t1 val_t2 other_val_ts) ->
    ProdType <$> (ProdTypes <$> mapM to_s_val_t (val_t1 : val_t2 : other_val_ts))

instance ToStateValType TypeApplication where
  to_s_val_t = \(TypeConsAndInputs cons_name left_t_ins right_t_ins) ->
    (++) <$> left_t_ins_conv left_t_ins <*> right_t_ins_conv right_t_ins >>= \case 
      [] -> to_s_val_t cons_name
      t_ins -> return $ TypeApp $ ConsAndTIns cons_name t_ins

instance ToStateValType TypeName where
  to_s_val_t = \cons_name@(TN t_name) -> case length t_name of
    1 ->
      M.lookup cons_name <$> get_v_map >>= \case
        Just i -> return $ TypeVar i
        Nothing ->
          get >>= \(StateType v_count v_map) ->
          put (StateType (v_count + 1) $ M.insert cons_name v_count v_map) >>
          return (TypeVar v_count)
    _ -> return $ TypeApp $ ConsAndTIns cons_name []

-- TypeApplication: type_app_conv

left_t_ins_conv = ( \case
  NoLeftTInputs -> return []
  OneLeftTInput type_input -> ( \x -> [x] ) <$> to_s_val_t type_input
  ManyLeftTInputs many_ts_in_paren -> ts_in_paren_to_val_types many_ts_in_paren
  ) :: LeftTInputs -> Stateful [ ValType ]

right_t_ins_conv = ( \case
  NoRightTInputs -> return []
  OneRightTInput type_input -> ( \x -> [x] ) <$> to_s_val_t type_input
  ManyRightTInputs many_ts_in_paren -> ts_in_paren_to_val_types many_ts_in_paren 
  ) :: RightTInputs -> Stateful [ ValType ]

ts_in_paren_to_val_types = ( \(TypesInParen t1 t2 ts) ->
  mapM to_s_val_t $ t1 : t2 : ts
  ) :: ManyTypesInParen -> Stateful [ ValType ]

ts_in_paren_to_value_types = ( \(TypesInParen t1 t2 ts) -> t1 : t2 : ts )
  :: ManyTypesInParen -> [ ValueType ]
