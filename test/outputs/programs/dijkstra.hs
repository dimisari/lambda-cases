{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

type Graph = ListOf's Node

data Node =
  Node' { n_name :: P.Char, neighbors :: ListOf's Neighbor }

instance FromTuple2 P.Char (ListOf's Neighbor) Node where
  ft2 = \(x1, x2) -> Node' x1 x2

c0n_name :: P.Char -> Node -> Node
c0neighbors :: ListOf's Neighbor -> Node -> Node
c0n_name = \new x -> x { n_name = new }
c0neighbors = \new x -> x { neighbors = new }

type Dist = P.Integer

type Neighbor = NameAndDist

data NameAndDist =
  NameAndDist' { name :: P.Char, dist :: Dist }

instance FromTuple2 P.Char Dist NameAndDist where
  ft2 = \(x1, x2) -> NameAndDist' x1 x2

c0name :: P.Char -> NameAndDist -> NameAndDist
c0dist :: Dist -> NameAndDist -> NameAndDist
c0name = \new x -> x { name = new }
c0dist = \new x -> x { dist = new }

data State =
  State' { dns :: ListOf's DoneNode, rns :: ListOf's ReachableNode }

instance FromTuple2 (ListOf's DoneNode) (ListOf's ReachableNode) State where
  ft2 = \(x1, x2) -> State' x1 x2

c0dns :: ListOf's DoneNode -> State -> State
c0rns :: ListOf's ReachableNode -> State -> State
c0dns = \new x -> x { dns = new }
c0rns = \new x -> x { rns = new }

data InRNsPossibilities =
  Cin_rns (Dist, ListOf's ReachableNode) |
  Cnot_in_rns

type DoneNode = NameAndDist

type ReachableNode = NameAndDist

type A'FStateMan a1 = A'FState'Man a1 State

type StateMan = State'Man State

set_rns' :: ListOf's ReachableNode -> StateMan
set_rns' =
  \rns -> modify_state_with'(\s -> ft2(dns(s), rns))

dijkstra'' :: (P.Char, Graph) -> ListOf's DoneNode
dijkstra'' =
  \(source_name, g) ->
  let
  is :: State
  is =
    ft2([], [ft2(source_name, (0 :: P.Integer))])

  get_result :: A'FStateMan (ListOf's DoneNode)
  get_result =
    get_state !>>= \state ->
    rns(state) &> \pA0 ->
    case pA0 of
      [] -> wrap'(dns(state))
      rn1 : rns ->
        set_state'(ft2(dns(state) !+ rn1, rns)) !>> 
        add_new_rns''(rn1, get_neighbors'(rn1)) !>> 
        get_result

  get_neighbors' :: DoneNode -> ListOf's Neighbor
  get_neighbors' =
    \dn ->
    let
    get_neighbors_from_nodes' :: ListOf's Node -> ListOf's Neighbor
    get_neighbors_from_nodes' =
      \pA0 ->
      case pA0 of
        [] -> throw_err'("Should be impossible: I'm not in the nodes!")
        n : ns ->
          (name(dn) !== n_name(n)) &> \pA0 ->
          case pA0 of
            P.True -> neighbors(n)
            P.False -> get_neighbors_from_nodes'(ns)
    in
    get_neighbors_from_nodes'(g)
  in
  result_of'on_init_state'(get_result, is)

update_neighbor'of' :: (Neighbor, DoneNode) -> StateMan
update_neighbor'of' =
  \(ne, dn) ->
  let
  check_if_ne_in_rns :: A'FStateMan InRNsPossibilities
  check_if_ne_in_rns =
    let
    check_ne_in_rns_rec'' :: (ListOf's ReachableNode, ListOf's ReachableNode) -> A'FStateMan InRNsPossibilities
    check_ne_in_rns_rec'' =
      \(checked, pA0) ->
      case pA0 of
        [] -> wrap'(Cnot_in_rns)
        rn1 : rns ->
          (name(ne) !== name(rn1)) &> \pA0 ->
          case pA0 of
            P.True -> Cin_rns(ft2(dist(rn1), checked !+ rns)) &> (\pA0 -> wrap'(pA0))
            P.False -> check_ne_in_rns_rec''(checked !+ rn1, rns)
    in
    get_state !>>= (\x' -> rns(x')) .> (\pA0 -> check_ne_in_rns_rec''([], pA0))

  add_ne_to_rns_from_dn_if_better' :: (Dist, ListOf's ReachableNode) -> StateMan
  add_ne_to_rns_from_dn_if_better' =
    \(d, other_rns) ->
    let
    new_d :: Dist
    new_d =
      dist(dn) !+ dist(ne)
    in
    (new_d !< d) &> \pA0 ->
    case pA0 of
      P.True -> (\pA0 -> set_rns'(pA0)) <& add'to_rns'(ft2(name(ne), new_d), other_rns)
      P.False -> do_nothing

  check_if_ne_in_dns :: A'FStateMan P.Bool
  check_if_ne_in_dns =
    let
    check_ne_name_in' :: ListOf's DoneNode -> A'FStateMan P.Bool
    check_ne_name_in' =
      \pA0 ->
      case pA0 of
        [] -> wrap'(P.False)
        dn : dns ->
          (name(ne) !== name(dn)) &> \pA0 ->
          case pA0 of
            P.True -> wrap'(P.True)
            P.False -> check_ne_name_in'(dns)
    in
    get_state !>>= (\x' -> dns(x')) .> (\pA0 -> check_ne_name_in'(pA0))

  add_ne_to_rns_from_dn :: StateMan
  add_ne_to_rns_from_dn =
    get_state !>>= (\x' -> rns(x')) .> (\pA0 -> add'to_rns'(ft2(name(ne), dist(dn) !+ dist(ne)), pA0)) .> 
    (\pA0 -> set_rns'(pA0))
  in
  check_if_ne_in_rns !>>= \pA0 ->
  case pA0 of
    Cin_rns stuff -> add_ne_to_rns_from_dn_if_better'(stuff)
    Cnot_in_rns ->
      check_if_ne_in_dns !>>= \pA0 ->
      case pA0 of
        P.True -> do_nothing
        P.False -> add_ne_to_rns_from_dn

add_new_rns'' :: (DoneNode, ListOf's Neighbor) -> StateMan
add_new_rns'' =
  \(dn, pA0) ->
  case pA0 of
    [] -> do_nothing
    ne1 : nes -> update_neighbor'of'(ne1, dn) !>> add_new_rns''(dn, nes)

add'to_rns' :: (ReachableNode, ListOf's ReachableNode) -> ListOf's ReachableNode
add'to_rns' =
  \(rn, pA0) ->
  case pA0 of
    [] -> [rn]
    rn1 : rns ->
      (dist(rn) !< dist(rn1)) &> \pA0 ->
      case pA0 of
        P.True -> rn !+ (rn1 !+ rns)
        P.False -> rn1 !+ add'to_rns'(rn, rns)

test_graph :: Graph
test_graph =
  [ ft2('a', [ft2('b', (2 :: P.Integer)), ft2('d', (6 :: P.Integer))])
  , ft2('b', [ft2('a', (2 :: P.Integer)), ft2('c', (3 :: P.Integer))])
  , ft2('c', [ft2('b', (3 :: P.Integer)), ft2('d', (5 :: P.Integer)), ft2('e', (1 :: P.Integer))])
  , ft2('d', [ft2('a', (6 :: P.Integer)), ft2('c', (5 :: P.Integer)), ft2('e', (4 :: P.Integer))])
  , ft2('e', [ft2('c', (1 :: P.Integer)), ft2('d', (4 :: P.Integer)), ft2('f', (3 :: P.Integer))])
  , ft2('f', [ft2('c', (7 :: P.Integer)), ft2('e', (3 :: P.Integer))])
  ]

instance P.Show NameAndDist where
  show = \nam -> "(name = " !+ name(nam) !+ ", dist = " !+ dist(nam) !+ ")"

main :: IO
main =
  dijkstra''('a', test_graph) &> P.print