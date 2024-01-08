(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* Takes a name to be substituted and a list of substitute name (which might include the name to be substituted)
   and returns a list option of substitute names excluding the name to be substituted *)
fun all_except_option (str: string, sl: string list) =
   case sl of
      [] => NONE
     | x::xs => 
	 	if same_string(str, x) 
		then SOME xs 
		else let val xs' = all_except_option(str, xs)
			 in 
				case xs' of
				SOME e => SOME (x::e)
				| NONE => NONE
			 end

(* Takes a list containing lists of substitute names and a name to be substituted and 
   returns a list of substitute names *)
fun get_substitutions1 (sl: string list list, str: string) =
   case sl of
   [] => []
   | x::xs => 
		case all_except_option(str, x) of
		NONE => get_substitutions1(xs, str) 
		| SOME s => s @ get_substitutions1(xs, str) 

(* Tail recursive version of get_substitutions1 *)
fun get_substitutions2 (sl: string list list, str: string) =
   let
	 fun append (xs: string list, ys: string list) =
	 	case xs of
		[] => ys
		| z::zs => z :: append(zs, ys)

     fun tail_substitute (sl: string list list, acc: string list) =
         case sl of
         [] => acc
         | xs::ys => 
		 	case all_except_option(str, xs) of
				NONE => tail_substitute(ys, acc)
			  | SOME e => tail_substitute(ys, append(e, acc))
   in
     tail_substitute(sl, [])
   end

(* Takes a list containing lists of substitute names and a name record with first, middle and last name fields
   and returns a list containing records of similar names (substitutes of the original name record) *)
fun similar_names (subs: string list list, full_name: {first: string, middle: string, last: string }) =
   let
     val { first, middle, last } = full_name
     val substitutions = get_substitutions1(subs, first)

     fun produce_names (names: string list, name: { first: string, middle: string, last: string }) =
         case names of 
         [] => name::[]
         | x::xs => 
		 	{ first = x, middle = middle, last = last } :: produce_names(xs, name)

   in
     produce_names(substitutions, full_name)
   end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color card =
	case card of
	(Spades, _) => Black
  | (Clubs, _) => Black
  | _ => Red

fun card_value (_, Ace) = 11
	| card_value (_, Num num) = num
    | card_value _ = 10

(* Takes a list of cards, a card to be removed from the list and an exception
   then returns a list of cards without the unwanted card or raises an exception if 
   the unwanted card is not found in the list *)
fun remove_card (cs: card list, c: card, e: exn) = 
   case cs of
   [] => []
   | x::[] => if x <> c then raise e else []
   | x::xs => 
      let val xs' = remove_card (xs, c, e)
	  in 
	  	if x = c 
		then xs'
		else x :: xs'
	  end

(* Takes a list of cards and returns true if they all have the same color else false *)
fun all_same_color (cs: card list) =
	case cs of
	[] => true 
	| x::[] => true
	| head::(neck::xs) => 
		card_color (head) = card_color (neck) andalso all_same_color (xs)
   
(* Tail recursive sum of numbers associated with all cards in the list *)
fun sum_cards (cs: card list) =
   let
     fun sum_cs(cs: card list, acc: int) =
         case cs of 
		 [] => acc
		| x::xs => sum_cs(xs, acc+card_value(x))
   in
     sum_cs(cs, 0)
   end

(* determines the score of the card game at any point in time *)
fun score (cs: card list, goal: int) =
	let
	  val sum = sum_cards (cs)
	  val is_held_greater = sum > goal
	  val prem_score = 
	  		if is_held_greater
			then 3 * (sum - goal)
			else (goal - sum)
	in
	  if all_same_color(cs)
	  then prem_score div 2
	  else prem_score
	end


fun officiate (card_list: card list, moves: move list, goal: int) =
	let
		val held_cards = []

		fun current_state (held_cards: card list, card_list: card list, moves: move list, goal: int) =
			case moves of
			[] => score(held_cards, goal)
			| (Discard c)::ms => current_state(remove_card(held_cards, c, IllegalMove), card_list, ms, goal)
			| (Draw)::ms => 
				case card_list of 
				[] => score(held_cards, goal)
				| cd::cds => 
					if sum_cards(held_cards) > goal
					then score(held_cards, goal)
					else current_state((cd::held_cards), cds, ms, goal)
	in
	  current_state(held_cards, card_list, moves, goal)
	end