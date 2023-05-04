module ParsingTypes.LowLevel where

import Text.Parsec.Pos (SourcePos, newPos)
import Data.List (intercalate)
import Helpers ((==>), (.>))

-- All (Types and Show instances):
-- ValueName, Literal, Abstraction, ManyAbstractions, Input

-- ValueName

newtype ValueName =
  VN String deriving (Eq, Ord)

instance Show ValueName where
  show = \(VN val_name_str) -> val_name_str

-- PosValueName

data PosValueName =
  PVN { pvn_pos :: SourcePos, pvn_to_vn :: ValueName }

instance Show PosValueName where
  show = \(PVN _ vn) -> show vn

vn_to_pvn = PVN (newPos "" 0 0)
  :: ValueName -> PosValueName

-- Literal

data Literal =
  Integer Integer | Char Char | String String

instance Show Literal where
  show = \case
    Integer integer -> show integer
    Char char -> show char
    String string -> show string

-- PosLiteral

data PosLiteral =
  PL SourcePos Literal

instance Show PosLiteral where
  show = \(PL _ lit) -> show lit

-- Abstraction

data Abstraction =
  AbstractionName PosValueName | UseFields

instance Show Abstraction where
  show = \case
    AbstractionName value_name -> show value_name
    UseFields -> "use_fields"

-- PosAbstraction

data PosAbstraction =
  PA { pa_pos :: SourcePos, pa_to_a :: Abstraction }

instance Show PosAbstraction where
  show = \(PA _ abs) -> show abs

-- ManyAbstractions

data ManyAbstractions =
  Abstractions PosAbstraction PosAbstraction [ PosAbstraction ]

instance Show ManyAbstractions where
  show = \(Abstractions abs1 abs2 rest_of_abs) ->
    "(" ++ map show (abs1 : abs2 : rest_of_abs)==>intercalate ", " ++ ")"

-- PosManyAbstractions

data PosManyAbstractions =
  PMA { pma_pos :: SourcePos, pma_to_ma :: ManyAbstractions }

instance Show PosManyAbstractions where
  show = \(PMA _ many_abs) -> show many_abs

-- Input

data Input =
  OneAbstraction PosAbstraction | ManyAbstractions PosManyAbstractions 

instance Show Input where
  show = show_input .> (++ " -> ") where
    show_input = \case
      OneAbstraction abstraction -> show abstraction
      ManyAbstractions abstractions -> show abstractions

-- PosInput

data PosInput =
  PI { pi_pos :: SourcePos, pi_to_i :: Input }

instance Show PosInput where
  show = \(PI _ input) -> show input
