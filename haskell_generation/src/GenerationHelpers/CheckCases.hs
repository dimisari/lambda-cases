module GenerationHelpers.CheckCases where

import Control.Monad.Trans.Except (throwE, catchE)

import Helpers ((==>))
import ParsingTypes.LowLevel
import ParsingTypes.Types
import ParsingTypes.Values

import GenerationState.TypesAndOperations

import GenerationHelpers.ErrorMessages

-- Check cases for or_type

data SpecificOrDefaultCaseVN =
  SpecificValName ValueName | DefaultValName ValueName

check_lovns_or_type = ( \lovns or_t_name or_t_cases_names has_default ->
  mapM check_lovn_is_val_name lovns >>= \val_names -> 
  catch_and_complete_err
    (check_val_names_in_cases or_t_cases_names $ init val_names)
    (show or_t_name ++ "\n") >>
  catch_and_complete_err
    (check_last_val_name has_default or_t_cases_names $ last val_names)
    (show or_t_name ++ "\n") >>= \last_val_name ->
  check_no_dupl_val_name val_names >> 
  catch_and_complete_err
    (check_all_cases_covered has_default last_val_name val_names or_t_cases_names)
    (cases_not_covered_err_cont or_t_name) >>
  return (val_names, last_val_name)
  )
  ::
  [ LitOrValName ] -> TypeName -> [ ValueName ] -> Bool ->
  Stateful ([ ValueName ], SpecificOrDefaultCaseVN)

check_lovn_is_val_name = ( \case
  ValueName val_name -> return val_name
  Literal lit -> throwE $ lit_in_or_type_case_err lit
  ) :: LitOrValName -> Stateful ValueName

check_val_names_in_cases = ( \or_t_cases_names val_names ->
  mapM_ (check_val_name_in_cases or_t_cases_names) val_names
  ) :: [ ValueName ] -> [ ValueName ] -> Stateful ()

check_val_name_in_cases = ( \or_t_cases_names val_name ->
  case elem val_name or_t_cases_names of
    True -> return ()
    False -> throwE $ not_or_type_case_err_new val_name
  ) :: [ ValueName ] -> ValueName -> Stateful ()

check_last_val_name = ( \has_default or_t_cases_names val_name ->
  case has_default of
    True ->
      check_val_name_in_cases or_t_cases_names val_name >>
      return (SpecificValName val_name)
    False -> case elem val_name or_t_cases_names of
      True -> return $ SpecificValName val_name
      False -> return $ DefaultValName val_name
  ) :: Bool -> [ ValueName ] -> ValueName -> Stateful SpecificOrDefaultCaseVN

check_no_dupl_val_name = ( \case
  [] -> return ()
  val_name : val_names -> case elem val_name val_names of
    True -> throwE $ duplicate_case_err val_name
    False -> check_no_dupl_val_name val_names
  ) :: [ ValueName ] -> Stateful ()

check_all_cases_covered = (
  \has_default last_val_name val_names or_t_cases_names ->
  case has_default of
    True -> return ()
    False -> case last_val_name of
      DefaultValName _ -> return ()
      SpecificValName val_name ->
        check_all_cases_covered_specific (val_name : val_names) or_t_cases_names
  )
  ::
  Bool -> SpecificOrDefaultCaseVN ->[ ValueName ] -> [ ValueName ] -> Stateful ()

check_all_cases_covered_specific = ( \val_names or_t_cases_names ->
  filter (not . flip elem val_names) or_t_cases_names ==> \case
    [] -> return ()
    not_covered_cases -> throwE $ cases_not_covered_err not_covered_cases
  ) :: [ ValueName ] -> [ ValueName ] -> Stateful ()

-- 

catch_and_complete_err = ( \g str ->
  catchE g (append_to_err str)
  ) :: Stateful a -> String -> Stateful a

append_to_err = ( \str (b, e_t, err_msg) -> throwE (b, e_t, err_msg ++ str) )
  :: String -> Error -> Stateful a

--

data IntOrValName = 
  Int_ Int | ValName_ ValueName

check_lovns_int = ( \lovns has_default ->
  mapM check_lovn_is_int (init lovns) >>= \ints -> 
  check_last_lovn has_default (last lovns) >>= \int_or_val_name ->
  check_no_dupl_int_with_last ints int_or_val_name >>
  return (ints, int_or_val_name)
  ) :: [ LitOrValName ] -> Bool -> Stateful ([ Int ], IntOrValName)

check_lovn_is_int = ( \case
  ValueName val_name -> throwE $ wrong_int_case_err val_name
  Literal (Int i) -> return i
  ) :: LitOrValName -> Stateful Int

check_last_lovn = ( \has_default lovn -> case has_default of
  True -> check_lovn_is_int lovn >>= \i -> return $ Int_ i
  False -> check_last_lovn_is_val_name lovn
  ) :: Bool -> LitOrValName -> Stateful IntOrValName

check_last_lovn_is_val_name = ( \case
  ValueName val_name -> return $ ValName_ val_name
  Literal lit -> throwE $ last_int_case_err lit
  ) :: LitOrValName -> Stateful IntOrValName

check_no_dupl_int_with_last = ( \ints -> \case
  Int_ int -> check_no_dupl_int $ int : ints
  ValName_ _ -> check_no_dupl_int ints
  ) :: [ Int ] -> IntOrValName -> Stateful ()

check_no_dupl_int = ( \case
  [] -> return ()
  i : is -> case elem i is of
    True -> throwE $ duplicate_int_case_err i
    False -> check_no_dupl_int is
  ) :: [ Int ] -> Stateful ()

-- 

