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


val only_capitals = List.filter (fn s => Char.isUpper (String.sub (s, 0)))

fun longest_string1 (xs: string list) = 
	List.foldl (fn (s, acc) => 
		if String.size acc >= String.size s then acc else s) "" xs

fun longest_string2 (xs: string list) = 
	List.foldl (fn (s, acc) => 
		if String.size acc > String.size s then acc else s) "" xs

fun longest_string_helper (f: int * int -> bool) (xs: string list) = 
	List.foldl (fn (s, acc) => 
		if f ((String.size acc), (String.size s)) then acc else s) "" xs

val longest_string3 = fn xs => longest_string_helper (fn (acc, s) => acc >= s) xs

val longest_string4 = fn xs => longest_string_helper (fn (acc, s) => acc > s) xs

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode 

fun first_answer f xs = 
	case List.foldr (fn (s, acc) => 
			case f s of SOME v => SOME v | NONE => acc ) NONE xs of 
			SOME v => v 
			| NONE => raise NoAnswer

(* fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case f x of NONE => first_answer f xs'
			                      | SOME y => y *)

fun all_answers f xs =
	let
		val ys = List.map f xs  
		fun helper zs acc = 
			case zs of 
			[] => acc 
			| (SOME z)::zs' => helper zs' (acc @ z) 
			| _ => acc
	in
	  	 if List.all (fn s => isSome s) ys
		 then SOME (helper ys [])
		 else NONE 
	end 

(* fun all_answers f xs =
    let fun loop (acc,xs) =
        case xs of
		        [] => SOME acc
	        | x::xs' => case f x of 
                          NONE => NONE
              			    | SOME y => loop((y @ acc), xs')
    in loop ([],xs) end *)

fun count_wildcards p = g (fn () => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) (fn s => String.size s ) p

fun count_some_var (t, p) = g (fn () => 0) (fn s => if s = t then 1 else 0) p

fun check_pat p = 
	let
		fun fold_var_strings p = 
			case p of 
			Variable v => [v] 
			| TupleP ps => List.foldl (fn (v, acc) => (fold_var_strings v) @ acc) [] ps
			| ConstructorP (_, v) => fold_var_strings v
			| _ => []

		fun is_no_duplicates ps = 
			case ps of 
			[] => true 
			| x::[] => true
			| x::xs' => List.exists (fn y => y <> x) xs'
	in
	  is_no_duplicates (fold_var_strings p)
	end

fun match (v, p) = 
	case (v, p) of
	(Const i, ConstP j) => if i = j then SOME [] else NONE
	| (Unit, UnitP) => SOME []
	| (Tuple vs, TupleP ps) => 
		if List.length vs = List.length ps
		then all_answers match (ListPair.zip (vs, ps))
		else NONE
	| (Constructor (s, v), ConstructorP (t, w)) =>
		if s = t andalso isSome (match (v, w))
		then SOME [] 
		else NONE 
	| (_, Wildcard) => SOME []
	| (v, Variable s) => SOME [(s, v)]
	| _ => NONE

fun first_match v ps = 
	(case first_answer (fn p => match (v, p)) ps of
	p => SOME p) handle NoAnswer => NONE