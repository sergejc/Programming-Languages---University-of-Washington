(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str: string, xs: string list)=
        case xs of
          [] => NONE
        | x :: xs' => if str = x then SOME(xs') else               
                case all_except_option(str, xs') of
                          NONE   => NONE
                        | SOME y => SOME (x::y)

fun get_substitutions1 ([], s) = []
	| get_substitutions1 (x :: xs', s) =
	  case all_except_option(s, x) of
		  NONE   => get_substitutions1(xs', s)
		| SOME y => y @ get_substitutions1(xs', s)


fun get_substitutions2 (xs, s) = 
    let
        fun get_subs(xs, acc) =
            case xs of       
                  [] => acc
                | x::xs' => case all_except_option(s, x) of
                        NONE => get_subs(xs', acc)
                      | SOME y => get_subs(xs', (y @ acc))
    in
        get_subs(xs, [])                                                                                                                            end

fun similar_names (xs, full_name: {first:string,middle:string,last:string}) =
    let
        val {first = f ,middle = m, last = l} = full_name
        val subst = get_substitutions1(xs, f)
        fun get_names (xs, acc) =
            case xs of
                  [] => acc
                | x::xs' => get_names (xs', {first = x, middle = m, last = l} ::  acc) 
    in
        get_names(get_substitutions1(xs, f), full_name :: [])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
(* put your solutions for problem 2 here *)
fun card_color card =
    case card of
          (Diamonds, _) => Red 
        | (Clubs, _) => Black
        | (Hearts, _) => Red
        | (Spades, _) => Black 
        
fun card_value card = 
    case card of
          (_, Ace) => 11
        | (_, Num a) => a  
        | (_, _) => 10
        
fun remove_card ([], c, e) = raise IllegalMove
    | remove_card (x::xs', c, e) =
        if x = c then xs' else x :: remove_card (xs', c, e)
            
fun all_same_color xs =
    case xs of
          [] => true
        | x::[] => true
        | x::(xs':: xs'') => card_color x = card_color xs' andalso all_same_color(xs'::xs'')

fun sum_cards cards =
    let
        fun sum ([], s) = s
            | sum( x::xs', s) =
                sum(xs', card_value(x) + s)                
    in
        sum(cards, 0)
    end

fun score (cards, goal) =
    let
        val sum = sum_cards cards
        val greater = 3 * (sum - goal)
        val lower = goal - sum
    in
        if all_same_color cards then if sum > goal then greater div 2 else lower div 2
        else if sum > goal then greater else lower
    end

fun officiate (cards, moves, goal) = 
    let
        fun action(cards, moves, held_cards)  =
            if cards = [] orelse moves = [] orelse sum_cards(held_cards) > goal then score(held_cards, goal)            
            else case (cards, moves, held_cards) of
                  (x :: xs, (Discard c ):: ys, _) => action(xs, ys, remove_card (held_cards, c, IllegalMove))
                | (x :: xs, (Draw) :: ys, _) => action(xs, ys, x :: held_cards)
    in
        action(cards, moves, [])
    end
