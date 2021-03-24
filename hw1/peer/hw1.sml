

(*int * int * int , int * int * int -> bool *)
fun is_older(x: int * int * int, y : int * int * int) =
    if(#1 x) = (#1 y)
    then if (#2 x) = (#2 y)
	 then (#3 x) < (#3 y)
	 else (#2 x) < (#2 y)
    else (#1 x) < (#1 y)
    
(* (int * int * int) list, int -> int *)
fun number_in_month (xs : (int * int * int) list, month: int) =
    
    if null xs
    then 0
    else
	if #2 (hd xs) = month
	then 1 + number_in_month(tl xs, month)
	else 0 + number_in_month(tl xs, month)
				
(* (int * int * int) list, int list -> int *)
fun number_in_months (xs : (int * int * int) list, months: int list) =
    
    if null months
    then 0
    else number_in_month(xs, hd months) + number_in_months(xs, tl months)

(*( int * int * int) list, int -> ( int * int * int) list *)
fun dates_in_month (xs:( int * int * int) list, month : int ) =
    if null xs
    then []
    else
	if #2(hd xs)= month
	then hd xs :: dates_in_month(tl xs, month)
	else dates_in_month(tl xs, month)

(*( int * int * int) list, int list -> ( int * int * int) list *)
fun dates_in_months (xs: ( int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month(xs, hd months) @ dates_in_months(xs, tl months)

(*string list, int -> string *)
fun get_nth (textos: string list, n: int) =
    if n = 1
    then hd textos
    else get_nth(tl textos, n-1)

(*int * int * int -> string *)
fun date_to_string(date: (int * int * int))=
    let
	val months = ["January", "February", "March",
		      "April", "May", "June", "July",
		      "August", "September", "October",
		      "November", "December"]
    in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date)
       ^ ", " ^ Int.toString(#1 date)
  end

(*int , int list -> int *)
fun number_before_reaching_sum (sum: int, lista: int list)=
    let
	fun sum_list (xs : int list, soma_elementos: int, n: int) =
	    if soma_elementos + hd xs >= sum 
	    then n
	    else sum_list(tl xs, soma_elementos + hd xs, n + 1)
    in
	sum_list (lista, 0, 0 )
    end

(*int -> int *)
fun what_month(day: int) =
    let
	val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, days_in_month) + 1
    end

(*int, int -> list *)
fun month_range (day1: int, day2: int) =
    if day1 = day2
    then [what_month(day2)]
    else what_month(day1) :: month_range(day1 + 1, day2)

(* (int * int * int) list -> (int * int * int) option *)
fun oldest(xs:(int * int * int) list) =
    if null xs
    then NONE
    else
	let
	    val oldest_ans = oldest(tl xs)
	in
	    if isSome oldest_ans andalso
	       is_older(valOf oldest_ans, hd xs)
	    then oldest_ans
	    else SOME (hd xs)
	end
    
