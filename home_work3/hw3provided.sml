(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals lst =
    List.filter(fn str => Char.isUpper(String.sub(str, 0))) lst

(* 2 *)
fun longest_string1 lst =
    List.foldl(fn (str,init) => if String.size(str) > String.size(init) then str else init) "" lst

(* 3 *)
fun longest_string2 lst =
    List.foldl(fn (str,init) => if String.size(str) >= String.size(init) then str else init) "" lst


(* 4-1 *)
fun longest_string_helper f lst =
    List.foldl(fn (str,init) => if f(String.size(str), String.size(init)) then str else init) "" lst
    

(* 4-2 *)
fun longest_string3 lst =
    longest_string_helper(fn (str, init) => str > init) lst


(* 4-3 *)
fun longest_string4 lst =
    longest_string_helper(fn (str, init) => str >= init) lst


(* 5 *)
fun longest_capitalized lst =
    longest_string2(only_capitals lst)

(* 6 *)
fun rev_string str =
    String.implode(List.rev(String.explode(str)))

(* 7 *)
fun first_answer f lst =
    case lst of
          [] => raise NoAnswer
        | x::xs => case f(x) of
                  NONE => first_answer f xs
                | SOME y => y

(* 8 *)
fun all_answers f lst =
    let
        fun res([], acc) = SOME acc
            | res(x::xs, acc) = 
                case f(x) of
                      NONE => NONE
                    | SOME y => res(xs, y @ acc)

    in
        res(lst, [])
    end

(* 9-1 *)
fun count_wildcards ptr =
    g (fn () => 1) (fn x => 0) ptr

(* 9-2 *)

fun count_wild_and_variable_lengths ptr =
    g(fn () => 1) (fn x => String.size(x) ) ptr


(* 9-3 *)
fun count_some_var (str, ptr) =
    g(fn () => 0) (fn x => if str = x then 1 else 0) ptr

(* 10 *)
fun check_pat ptr =
    let
        fun get_list (p, acc) =
            case p of
                  Variable x        => x :: acc
                | TupleP ps         => (List.foldl(fn (p, i) =>  get_list (p, i)) acc ps) @ acc
                | ConstructorP(_,p) => get_list (p, acc)
                | _                 => acc

        fun check_rep ([]) = true
                | check_rep(x::xs) = not(List.exists (fn y => x = y) xs) andalso (check_rep xs)


    in
        check_rep(get_list (ptr,[]))
    end


(* 11 *)
fun match (valu, ptr) =
        case  (valu, ptr) of
              (_, Wildcard)                                 => SOME []
            | (Unit, UnitP)                                 => SOME []
            | (Const x, ConstP y)                           => if x = y then SOME [] else NONE
            | (_, Variable x)                               => SOME [(x, valu)]
            | (Constructor(s1, v), ConstructorP(s2, p))     => if s1 = s2 then match(v, p) else NONE
            | (Tuple v, TupleP p)                           => if List.length(v) = List.length(p) then all_answers match (ListPair.zip(v,p))  else NONE
            | _                                             => NONE
(* 12 *)
fun first_match v lp =
    case lp of
        []  => NONE
        | _ => SOME (first_answer (fn x => match(v, x)) lp) handle NoAnswer => NONE

