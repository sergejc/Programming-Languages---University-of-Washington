fun clear_dups(orig_list: int list, clear_list: int list) =
        let
                fun filter_dups(l: int list, item: int) =
                        if null l then [item]
                        else if hd l <> item then [hd l] @ filter_dups(tl l, item)
                        else filter_dups(tl l, item)
        in
                        if null orig_list then clear_list
                        else clear_dups(tl orig_list, filter_dups(clear_list, hd orig_list))
        end



fun is_older(pr1 : int * int * int, pr2: int * int * int) =
	if (#1 pr1) < (#1 pr2) then true	
	else if ((#2 pr1) < (#2 pr2)) then true
	else if ((#3 pr1) < (#3 pr2)) then true
	else false


fun number_in_month(l: (int * int *int)list, m: int) =
	if null l then 0
	else if ( #2 (hd l)) = m then 1 + number_in_month(tl l, m)	
 	else number_in_month(tl l, m)


fun number_in_months(l: (int * int *int)list, m: int list) =
	if null m then 0
	else number_in_month(l, hd m) + number_in_months(l, tl m)

fun dates_in_month(l: (int * int * int) list, m: int) =
	if null l then []
	else if(#2 (hd l)) = m then [hd l] @ dates_in_month(tl l, m)
	else dates_in_month(tl l, m)

fun dates_in_months(l: (int * int *int)list, m: int list) =
	if null m then []
	else dates_in_month(l, hd m) @ dates_in_months(l, tl m)
	
fun get_nth(l: string list, n: int) =
	if n = 1 then hd l
	else get_nth(tl l, n-1)

fun date_to_string(d: int * int * int) =
	let
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
		get_nth(months, #2 d) ^" " ^ Int.toString (#3 d)^", "^ Int.toString (#1 d)
	end

fun number_before_reaching_sum(s: int, l: int list) =
	if s - hd l <= 0  then 0
	else 1 + number_before_reaching_sum( s - hd l, tl l)

fun what_month(d: int) =
	let
		val dates = [31,28,31,30,31,30,31,31,30,31,30,31]
	in
		number_before_reaching_sum(d, dates)+1
	end

fun month_range(p: int * int) =
	if #1 p > (#2 p) then []
	else if #1 p = (#2 p) + 1  then []
	else what_month(#1 p) ::  month_range((#1 p) + 1, (#2 p))


fun oldest(l: (int * int * int) list) =
	if null l then NONE
	else
	let
		fun search_max(cur_max: int * int * int, l: (int * int * int) list) =
			if null l then cur_max
			else if (is_older(cur_max, hd l)) then search_max(cur_max, tl l)
			else search_max(hd l, tl l)
	in
		SOME( search_max (hd l, tl l))
	end


fun number_in_months_challenge(l: (int * int *int)list, m: int list) =
        number_in_months(l, clear_dups(m,m))

fun dates_in_months_challenge(l: (int * int *int)list, m: int list) =
        dates_in_months(l, clear_dups(m,m))

fun reasonable_date(d: int*int*int) =
	let
		val dates = [31,28,31,30,31,30,31,31,30,31,30,31]
		val leap_dates = [31,29,31,30,31,30,31,31,30,31,30,31]
		fun check_date(dates:int list) =
			if (#3 d <= List.nth(dates, (#2 d)-1)) then true
			else false
	
		fun is_leap(year: int) =
			if (year mod 4) = 0 then 
				if (year mod 100) <> 0 orelse (year mod 400) = 0 then true
				else false
			else 
				if (year mod 4) <> 0 orelse  (year mod 100) = 0 andalso (year mod 400) <> 0 then false
				else true
					
	in

		if (#1 d) <= 0 orelse (#2 d) <= 0 orelse (#2 d) > 12 orelse (#3 d) <= 0 orelse (#3 d) > 31  then false
		else
			if is_leap(#1 d) then check_date(leap_dates)
			else check_date(dates)
	end
