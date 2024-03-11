fun is_older(date1 : int*int*int, date2 : int*int*int) =
  if (#1 date1 ) <> (#1 date2)
  then (#1 date1) < (#1 date2)
  else if (#2 date1) <> (#2 date2)
  then (#2 date1) < (#2 date2)
  else (#3 date1) < (#3 date2) 

fun number_in_month (date_list: (int*int*int) list, month: int) = 
    if null date_list then 0
    else if (#2 (hd date_list)) = month then 1 + number_in_month (tl date_list, month)
    else number_in_month (tl date_list, month)

fun number_in_months (date_list: (int*int*int) list, months: int list) = 
    if null months then 0
    else number_in_month (date_list, hd months) + number_in_months (date_list, tl months)

fun dates_in_month (date_list: (int*int*int) list, month: int) =
    if null date_list then []
    else if (#2 (hd date_list)) = month then (hd date_list) :: dates_in_month (tl date_list, month)
    else dates_in_month (tl date_list, month)

fun dates_in_months (date_list: (int*int*int) list, month: int list) = 
    if null month then []
    else dates_in_month (date_list, hd month) @ dates_in_months (date_list, tl month)

fun get_nth (str_list: string list, n: int) = 
    if (n=1) then hd str_list
    else get_nth (tl str_list, n-1)

fun date_to_string (date: int*int*int) = 
    let val months = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December"];
        val month = get_nth (months, (#2 date));
    in 
    month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end 

fun number_before_reaching_sum (sum: int, n_list: int list) = 
    let fun helper(sum, n_list, acc, nth) = 
        if sum > (hd n_list + acc) then helper(sum, tl n_list, hd n_list + acc, nth +1)
        else nth 
    in helper(sum, n_list, 0, 0)
    end 

fun what_month (day) = 
    let val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    in 
    number_before_reaching_sum(day, month_days) + 1
    end

fun month_range (day1, day2) = 
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1+1, day2)

fun oldest (date_list: (int*int*int) list) = 
    if null date_list then NONE 
    else 
    let fun helper(date_list) = 
        if null (tl date_list) then 
        hd date_list
        else let val tl_ans = helper(tl date_list)
            in 
            if is_older(hd date_list, tl_ans) then hd date_list
            else tl_ans
            end
    in 
    SOME (helper date_list)
    end 
