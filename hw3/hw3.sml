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

val longest_string1 = 
	fn (xs: string list) => 
		List.foldl (fn (acc, s) => 
						if String.size s >= String.size acc
						then s 
						else acc) "" xs

val longest_string2 = 
	fn (xs: string list) => 
		List.foldl (fn (acc, s) => 
						if String.size s > String.size acc 
						then s 
						else acc) "" xs

fun longest_string_helper f xs = List.foldl f  "" xs

val longest_string3 = 
	longest_string_helper (fn (acc, s) => 
							if String.size s >= String.size acc 
							then s 
							else acc)  
val longest_string4 = 
	longest_string_helper (fn (acc, s) => 
							if String.size s > String.size acc 
							then s 
							else acc)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode 

fun first_answer f xs = 
	let
		val ys = List.find (fn s => isSome (f s)) xs
	in
		case ys of NONE => raise NoAnswer | SOME v => v 
	end

fun all_answers f xs =
	let
		val ys = List.map f xs  
		fun helper zs acc = 
			case zs of 
			[] => acc 
			| (SOME z)::zs' => helper zs' (z @ acc) 
	in
	  	 if List.all (fn s => isSome s) ys
		 then SOME (helper ys [])
		 else NONE 
	end 
