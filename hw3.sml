(* Assign 03 Provided Code *)

(*  Version 1.0 *)

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

(* Description of g: take 3 parameters: function f1 and f2, and pattern p
    Based on the given rules of matching, g returns a list of bindings
*)

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


(**** put all your code after this line ****)
fun only_capitals(lst) =
    List.filter(fn x => Char.isUpper(String.sub(x,0))) lst

fun longest_string1(lst) =
   foldl(fn (x,y) => if String.size(x) > String.size(y) then x else y) "" lst

fun longest_string2(lst) =
    foldl(fn (x,y) => if String.size(x) < String.size(y) then y else x) "" lst

fun longest_string_helper f lst =
    List.foldl(fn (x,y) => if f(String.size(x), String.size(y)) then x else y) "" lst

val longest_string3 = longest_string_helper(fn(x,y) => x > y);
val longest_string4 = longest_string_helper(fn(x,y) => x >= y);

val longest_capitalized = longest_string1 o only_capitals;

val rev_string = String.implode o List.rev o String.explode;

fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
        | x::xs' => (case f x of
                    NONE => first_answer f xs'
                    | SOME v => v)

fun all_answers f xs =
    let
        fun all_answers_helper f xs acc =
            case xs of
                [] => SOME acc
                | x::xs' => (case f x of
                                NONE => NONE
                                | SOME lst => all_answers_helper f xs' (acc@lst))
    in
        all_answers_helper f xs []
    end


val count_wildcards = g (fn x => 1) (fn x => 0);

val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size(x));

fun count_some_var(str: string, p: pattern) =
    g (fn x => 0) (fn x => if x = str then 1 else 0) p

fun check_pat(p) =
    let
        fun check_pat_strlst p =
            case p of
                Wildcard => []
                | Variable str => [str]
                | TupleP xs => foldl (fn (lst, acc) => acc@(check_pat_strlst lst)) [] xs
                | ConstructorP(_,p) => check_pat_strlst p
                | _ => []
        fun check_pat_repeat xs =
            case xs of
                [] => true
                |x::xs => (not(List.exists (fn str => x = str) xs))
                            andalso (check_pat_repeat xs)
    in
        check_pat_repeat(check_pat_strlst(p))
    end

fun match (value, pat) =
    let
        fun tuple_helper(xs) =
            case xs of
                (v,p)::xs' => (match(v,p)::tuple_helper(xs'))
                |_ => []

    in
        case (value, pat) of
            (_, Wildcard) => SOME []
            | (_, Variable s) => SOME [(s, value)]
            | (Unit, UnitP) => SOME []
            | (Const x, ConstP y) => if x = y then SOME [] else NONE
            | (Tuple vs, TupleP ps) => (if (List.length(vs) = List.length(ps))
                                        then all_answers (fn x => x) (tuple_helper(ListPair.zip(vs,ps)))
                                        else NONE)
            | (Constructor(s2,v), ConstructorP(s1,p)) => if (s2 = s1) then match(v, p) else NONE
            | _ => NONE
    end

fun first_match v pats =
    SOME (first_answer (fn p => match(v, p)) pats)
    handle NoAnswer => NONE

