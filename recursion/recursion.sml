(* Function is_older takes two dates and evaluates to true if the first argument is a date that comes before 
   the second argument. *)
fun is_older (date1: int*int*int, date2: int*int*int): bool =
    (#1 date1 < #1 date2) orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2)
     orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)    
     
(* Function number_in_month takes a list of dates and a month (i.e., an int) and returns how many dates in the 
   list are in the given month. *)
fun number_in_month (dates: (int*int*int) list, month: int): int =
    if null dates
    then 0
    else 
        let
            fun count_months(dates: (int*int*int) list, count: int): int =
                if null dates
                then count
                else
                    if (#2 (hd dates) = month)
                    then count_months((tl dates), count+1)
                    else count_months((tl dates), count)
        in
            count_months(dates, 0)

        end

(* Function number_in_months takes a list of dates and a list of months and returns the number of dates in the list   of dates that are in any of the months in the list of months. *)
fun number_in_months (dates: (int*int*int) list, months: int list): int = 
    if null months
    then 0
    else
        let
            fun count_months(months: int list, count: int): int =
                if null months
                then count
                else
                     count_months((tl months), count +  number_in_month(dates, (hd months)))
        
        in
            count_months(months, 0)

        end

(* Function dates_in_month takes a list of dates and a month and returns a list holding the dates from the argument   list of dates that are in the month. The returned list contains dates in the order they were originally given. 
*)
fun dates_in_month (dates: (int*int*int) list, month: int): (int*int*int) list = 
    if null dates
    then []
    else
        let
            fun dates_list(dates: (int*int*int) list, dates_in_month: (int*int*int) list): (int*int*int) list = 
                if null dates
                then 
                    let
                        fun reverse_list(dates_in_month: (int*int*int) list, reversed_list: (int*int*int) list): (int*int*int) list =
                            if (null dates_in_month)
                            then reversed_list
                            else reverse_list((tl dates_in_month), (hd dates_in_month)::reversed_list)            
                    
                    in
                        reverse_list(dates_in_month, [])

                    end
                else
                    if (#2 (hd dates) = month)
                    then dates_list((tl dates), (hd dates)::dates_in_month)
                    else dates_list((tl dates), dates_in_month)
         
        in
            dates_list(dates, [])

        end 

(* Function dates_in_months takes a list of dates and a list of months and returns a list holding the dates from 
   the argument list of dates that are in any of the months in the list of months. *)
fun dates_in_months (dates: (int*int*int) list, months: int list): (int*int*int) list =
    if null months
    then []
    else
        let
            fun dates_in_month_list (months: int list, dates_in_months_result: (int*int*int) list): (int*int*int) list =
                if null months
                then dates_in_months_result
                else dates_in_month_list((tl months), dates_in_months_result @ dates_in_month(dates, (hd months))) 

        in
            dates_in_month_list(months, [])

        end

(* Function get_nth takes a list of strings and an int n and returns the nth element of the list where the head of    the list is 1st. *)
fun get_nth (strings: string list, n: int): string =
    if null strings 
    then ""
    else
        if (n = 1)
        then (hd strings)
        else get_nth((tl strings), n - 1)

(* Function date_to_string takes a date and returns a string of the form January 20, 2013. *)
fun date_to_string (date: int*int*int): string =
   let
       val months: string list = ["January", "February", "March", "April", "May", "June", "July", "August", 
                                  "September", "October", "November", "December"]

   in get_nth(months, #2 date) ^ " " ^ Int.toString((#3 date)) ^ "," ^ " " ^ Int.toString((#1 date))

   end

(* Function number_before_reaching_sum takes an int called sum, which is  assumed to be positive, and an int list,    which is  assumed to  contain all positive numbers, and returns an int n such that the first n elements of the
   list add to less than sum, but the first n+1 elements of the list add to sum or more. *)
fun number_before_reaching_sum (sum: int, numbers: int list): int =
    if null numbers
    then 0
    else
        let
            fun index_before_reaching_sum(numbers: int list, sum_till_now: int, count: int): int =
                let
                   val sum_till_now = (hd numbers) + sum_till_now
                in
                    if (sum_till_now < sum) 
                    then index_before_reaching_sum((tl numbers), sum_till_now, count + 1)
                    else count
                end
        in
            index_before_reaching_sum(numbers, 0, 0)

        end

(* Function what_month that takes a day of year and returns what month that day is in. *)
fun what_month (day: int): int =
    let
        val months: int list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, months) + 1
    end

(* Function month_range that takes two days of the year day1 and day2 and returns an int list [m1,m2,...,mn ] 
   where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2. *)
fun month_range (day1: int, day2: int): int list =
    if (day1 > day2)
    then []
    else
        let
            fun months_in_days (day1: int, day2: int, months: int list): int list =
                if (day2 < day1)
                then months
                else
                    months_in_days(day1, day2 - 1, (what_month(day2)::months))       
        in
            months_in_days(day1, day2, [])
      
        end

(* Function oldest takes a list of dates and evaluates to an option. It evaluates to NONE if the list has no dates    and SOME d if the date d is the oldest date in the list. *)
fun oldest (dates: (int*int*int) list): (int*int*int) option =
    if null dates
    then NONE
    else
        let
            fun oldest_nonempty (dates: (int*int*int) list): int*int*int =
                if null (tl dates)
                then hd dates
                else
                    let
                        val tl_oldest = oldest_nonempty(tl dates)
                    in
                        if is_older(hd dates, tl_oldest)
                        then hd dates
                        else tl_oldest
                    end
         in
             SOME(oldest_nonempty dates)

         end

(* Functions number_in_months_challenge and dates_in_months_challenge that are like functions number_in_months and    dates_in_months except having a month in the second argument multiple times has no more effect than having it 
   once. *)
 fun remove_duplicates(months: int list, no_duplicate_months: int list): int list =
     if null months
     then no_duplicate_months
     else
         if null no_duplicate_months
         then remove_duplicates(tl months, hd months::no_duplicate_months)
         else
             let
                 fun elem_in_list(month, no_duplicate_months: int list): bool =
                     if null no_duplicate_months
                     then false
                     else 
                         if (month = hd no_duplicate_months)
                         then true
                         else elem_in_list(month, tl no_duplicate_months)
             in
                 if (elem_in_list(hd months, no_duplicate_months))
                 then remove_duplicates(tl months, no_duplicate_months)
                  else remove_duplicates(tl months, hd months::no_duplicate_months)

             end

fun number_in_months_challenge (dates: (int*int*int) list, months: int list): int =
    if null months
    then 0
    else
        number_in_months(dates, remove_duplicates(months, [])) 

fun dates_in_months_challenge (dates: (int*int*int) list, months: int list): (int*int*int) list = 
    if null months
    then []   
    else
        dates_in_months(dates, List.rev(remove_duplicates(months, [])))        
                    

(* Function reasonable_date takes a date and determines if it describes a real date in the common era. A 
   "real date" has a positive year (year 0 did not exist), a month between 1 and 12, and a day appropriate for 
   the month. The fucntion also handles leap years. Leap years are years that are either divisible by 400 or  
   divisible by 4 but not divisible by 100. *)
fun reasonable_date(date: int*int*int): bool =
    let 
        fun valid_date(date: int*int*int): bool =
            let
                val months: int list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                fun is_leap_year(year: int): bool =
                    ((year mod 400) = 0 orelse (year mod 4) = 0) andalso (year mod 100 <> 0)    
            in
                if ((#2 date = 2) andalso (is_leap_year(#1 date)))
                then (#3 date >= 1 andalso #3 date <= 29)
                else (#3 date >=1 andalso #3 date <= List.nth(months, (#2 date) - 1))     
               
            end
    
     in 
         (#1 date > 0) andalso (#2 date >= 1 andalso #2 date <= 12) andalso (valid_date(date))

     end

