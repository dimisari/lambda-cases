{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

type Graph = ListOf's Node

data Node =
  Node' { n_name :: P.Char, n_children :: Children }

instance FromTuple2 P.Char Children Node where
  ft2 = \(x1, x2) -> Node' x1 x2

c0n_name :: P.Char -> Node -> Node
c0n_children :: Children -> Node -> Node
c0n_name = \new x -> x { n_name = new }
c0n_children = \new x -> x { n_children = new }

type Children = ListOf's P.Char

data State =
  State' { vns :: ListOf's VisitedNode, dns :: ListOf's DoneNode }

instance FromTuple2 (ListOf's VisitedNode) (ListOf's DoneNode) State where
  ft2 = \(x1, x2) -> State' x1 x2

c0vns :: ListOf's VisitedNode -> State -> State
c0dns :: ListOf's DoneNode -> State -> State
c0vns = \new x -> x { vns = new }
c0dns = \new x -> x { dns = new }

type StateMan = State'Man State

type VisitedNode = P.Char

type DoneNode = P.Char

data AllDoneOutcome =
  Cnot_all_done P.Char |
  Call_done

topo_sort_on_graph' :: Graph -> ListOf's DoneNode
topo_sort_on_graph' =
  \g ->
  let
  is :: State
  is =
    ft2([], [])

  topo_sort_from_state :: StateMan
  topo_sort_from_state =
    let
    check_all_done :: ListOf's DoneNode -> AllDoneOutcome
    check_all_done =
      \dns ->
      let
      check_all_in_dns' :: ListOf's P.Char -> AllDoneOutcome
      check_all_in_dns' =
        \pA0 ->
        case pA0 of
          [] -> Call_done
          n1 : ns ->
            a'is_in'(n1, dns) &> \pA0 ->
            case pA0 of
              P.True -> check_all_in_dns'(ns)
              P.False -> Cnot_all_done(n1)
      in
      check_all_in_dns'(node_names)

    node_names :: ListOf's P.Char
    node_names =
      apply'to_all_in'((\x' -> n_name(x')), g)
    in
    get_state !>>= (\x' -> dns(x')) .> check_all_done .> \pA0 ->
    case pA0 of
      Call_done -> do_nothing
      Cnot_all_done node_name ->
        topo_sort_on_node'(node_name) !>> topo_sort_from_state

  topo_sort_on_node' :: P.Char -> StateMan
  topo_sort_on_node' =
    \node_name ->
    let
    children :: Children
    children =
      let
      get_children_from_list' :: ListOf's Node -> Children
      get_children_from_list' =
        \pA0 ->
        case pA0 of
          [] -> throw_err'("Should be impossible: I'm not in the nodes!")
          n : ns ->
            (node_name !== n_name(n)) &> \pA0 ->
            case pA0 of
              P.True -> n_children(n)
              P.False -> get_children_from_list'(ns)
      in
      get_children_from_list'(g)

    add_to_done :: StateMan
    add_to_done =
      modify_state_with'(\s -> ft2(vns(s), node_name !+ dns(s)))

    add_to_visited :: StateMan
    add_to_visited =
      modify_state_with'(\s -> ft2(node_name !+ vns(s), dns(s)))

    remove_from_visited :: StateMan
    remove_from_visited =
      let
      remove_from' :: ListOf's VisitedNode -> ListOf's VisitedNode
      remove_from' =
        \pA0 ->
        case pA0 of
          [] -> throw_err'("Should be impossible: I'm not in visited")
          vn : vns ->
            (vn !== node_name) &> \pA0 ->
            case pA0 of
              P.True -> vns
              P.False -> vn !+ remove_from'(vns)
      in
      modify_state_with'(\s -> ft2(remove_from'(vns(s)), dns(s)))
    in
    children &> \pA0 ->
    case pA0 of
      [] -> add_to_done
      cs ->
        add_to_visited !>> topo_sort_on_children'(cs) !>> remove_from_visited !>> 
        add_to_done

  topo_sort_on_children' :: Children -> StateMan
  topo_sort_on_children' =
    \pA0 ->
    case pA0 of
      [] -> do_nothing
      c : cs ->
        let
        topo_sort_on_child :: StateMan
        topo_sort_on_child =
          check_not_in_visited_or_done !>>= \pA0 ->
          case pA0 of
            P.True -> topo_sort_on_node'(c)
            P.False -> do_nothing

        check_not_in_visited_or_done :: A'FState'Man P.Bool State
        check_not_in_visited_or_done =
          let
          f :: State -> P.Bool
          f =
            \s -> not'(a'is_in'(c, vns(s)) !| a'is_in'(c, dns(s)))
          in
          apply'inside'(f, get_state)
        in
        topo_sort_on_child !>> topo_sort_on_children'(cs)
  in
  dns(final_state_of'on_init_state'(topo_sort_from_state, is))

test_graph :: Graph
test_graph =
  [ ft2('4', ['6'])
  , ft2('3', ['4', '2', '1', '5'])
  , ft2('1', ['8', '9'])
  , ft2('2', ['4'])
  , ft2('5', ['7'])
  , ft2('6', [])
  , ft2('7', [])
  , ft2('8', [])
  , ft2('9', [])
  ]

main :: IO
main =
  topo_sort_on_graph'(test_graph) &> P.print