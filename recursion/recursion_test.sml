(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test_isOlder1 = is_older((1,2,3),(2,3,4)) = true

val test_isOlder2 = is_older((1978,6,12),(1983,1,15)) = true  

val test_isOlder3 = is_older((1988,6,12),(1983,1,15)) = false

val test_number_in_month1 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1

val test_number_in_month2 = number_in_month([(2012,2,28),(2013,12,1)],3) = 0

val test_number_in_month3 = number_in_month([],2) = 0

val test_number_in_months = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test_dates_in_month1 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test_dates_in_month2 = dates_in_month([(2012,2,28),(2013,12,1),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]

val test_dates_in_months = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test_get_nth = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"

val test_date_to_string = date_to_string((2013, 6, 1)) = "June 1, 2013"

val test_number_before_reaching_sum = number_before_reaching_sum(10, [1,2,3,4,5]) = 3

val test_what_month = what_month(70) = 3

val test_month_range = month_range(31, 34) = [1,2,2,2]
	
val test_oldest = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test_number_in_months_challenge = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,3,3,4,3]) = 3

val test_dates_in_months_challenge =  dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,3,3,4,3]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test_reasonable_date1 = reasonable_date((2012,2,29)) = true

val test_reasonable_date2 = reasonable_date((2012,2,28)) = true

val test_reasonable_date3 = reasonable_date((0,2,28)) = false

val test_reasonable_date4 = reasonable_date((2012,14,28)) = false

val test_reasonable_date5 = reasonable_date((2012,2,45)) = false








