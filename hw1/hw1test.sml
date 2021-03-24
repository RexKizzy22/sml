(* Contains tests for all functions in "hw1.sml" *)

(* Constants *)
val test_dates1 = [(20, 2, 11), (60, 12, 31), (40, 2, 10), (50, 2, 14), (80, 12, 3), (83, 4, 31), (76, 7, 11), (34, 4, 30)]
val test_dates2 = [(90, 7, 14), (60, 11, 31), (99, 8, 8), (73, 7, 10), (30, 11, 31), (93, 4, 3), (76, 5, 12), (43, 7, 21)]

val test_months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
val test_months1 = [2, 12, 6]
val test_months2 = [10, 5, 6, 7, 11, 1, 8, 4, 12]
val last_day_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 31, 31, 30, 31]
		       
val emt_list = []


(* Tests *)			     

(* Tests for is_older *)
val older1 = is_older((20, 12, 20), (20, 12, 20))     (* Same date *)                                     (* false *)
		     
val older2 = is_older((39, 2, 20), (40, 2, 20))       (* Same month and day, different year *)            (* true *)
val older3 = is_older((40, 2, 20), (39, 2, 20))                                                           (* false *)

val older4 = is_older((25, 5, 22), (25, 9, 22))       (* Same year and day, different month *)            (* true *)
val older5 = is_older((25, 9, 22), (25, 5, 22))                                                           (* false *)
		     
val older6 = is_older((55, 10, 22), (55, 10, 20))     (* Same year and month, different day *)            (* false *)
val older7 = is_older((55, 10, 20), (55, 10, 22))                                                         (* true *)

val older8 = is_older((45, 9, 1), (22, 12, 15))       (* Different year, month and day *)                 (* false *)

(* Tests for number_in_month *)
val test_nim0 = number_in_month(test_dates1, 8)       (* 0 *)
val test_nim1 = number_in_month(test_dates1, 4)       (* 2 *)
val test_nim2 = number_in_month(test_dates2, 7)       (* 3 *)

(* Tests for number_in_months *)
val test_number_in_months0 = number_in_months(test_dates1, emt_list)     (* 0 *)
val test_number_in_months1 = number_in_months(emt_list, test_months1)     (* 0 *)
val test_number_in_months2 = number_in_months(test_dates1, test_months1) (* 5 *)
val test_number_in_months3 = number_in_months(test_dates2, test_months2) (* 8 *)

(* Tests for dates_in_month *)
val test_dates_in_month0 = dates_in_month(test_dates1, 6)  (* [] *)
val test_dates_in_month1 = dates_in_month(test_dates1, 2)  (* [(20, 2, 11),(40, 2, 10),(50, 2, 14)] *)
val test_dates_in_month2 = dates_in_month(test_dates2, 11) (* [(60, 11, 31),(30, 11, 31)] *)

(* Tests for dates_in_months *)
val test_dims0 = dates_in_months(test_dates1, emt_list)     (* [] *)
val test_dims1 = dates_in_months(emt_list, test_months1)    (* [] *)
val test_dims2 = dates_in_months(test_dates1, test_months1) (*[(20, 2, 11),(40, 2, 10),(50, 2, 14),(60, 12, 31),(80, 12, 3)]*)
val test_dims3 = dates_in_months(test_dates2, test_months2) (*[(76, 5, 12),(90, 7, 14),(73, 7, 10),(43, 7, 21),(60, 11, 31),
                                                               (30, 11, 31),(99, 8, 8),(93, 4, 3)]*)

(* Tests for get_nth *) 
val test_nth = get_nth(test_months, 1)     (* January *)     
val test_nth2 = get_nth(test_months, 8)    (* August *)
val test_nth3 = get_nth(test_months, 12)   (* December *)


(* Tests for date_to_string *)
val test_date_to_string1 = date_to_string(hd test_dates1)          (* February 11, 20 *)
val test_date_to_string2 = date_to_string(hd (tl test_dates2))	   (* November 31, 60 *)				 

(* Tests for number_before_reaching_sum *)					 
val test_nbrs0 = number_before_reaching_sum(4, [1,4,1,1])                (* 1 *)
val test_nbrs1 = number_before_reaching_sum(5, [3,1,2])                  (* 2 *)	       
val test_nbrs2 = number_before_reaching_sum(5, [3,2,2])                  (* 1 *)
val test_nbrs3 = number_before_reaching_sum(6, [4,1,1,1])                (* 2 *)
val test_nbrs4 = number_before_reaching_sum(10, [1,2,3,4,5])             (* 3 *)					   	 
					 
(* Tests for what_month *)
val january = what_month(1)	  (* 1 *)				 
val may = what_month(144)         (* 5 *)
val june = what_month(180)        (* 6 *)
val december = what_month(365)    (* 12 *)			 

(* Tests for month_range *)
val test_month_range0 = month_range(5,3)         (* [] *)
val test_month_range1 = month_range(20,31)       (* [0,0,0,0,0,0,0,0,0,0,0,0] *)
val test_month_range2 = month_range(330,340)	 (* [10,10,10,10,10,10,11,11,11,11,11] *)			  

(* Tests for oldest *)
val test_oldest_date0 = oldest(emt_list)      (* NONE *)					 
val test_oldest_date1 = oldest(test_dates1)   (* SOME (20, 2, 11) *)
val test_oldest_date2 = oldest(test_dates2)   (* SOME (30, 11, 31) *)			      
