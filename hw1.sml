(*  Assignment #1 *)

type DATE = (int * int * int)
exception InvalidParameter

(* This file is where your solutions go *)

fun is_older(d1: DATE, d2: DATE): bool =
	if ((#1 d1) < (#1 d2)) 
		orelse (((#1 d1) = (#1 d2)) andalso ((#2 d1) < (#2 d2))) 
		orelse (((#1 d1) = (#1 d2)) andalso ((#2 d1) = (#2 d2)) andalso ((#3 d1)< (#3 d2)))
    	then true
	else false
	

fun number_in_month(d: DATE list, m: int): int =
	if null d
	then 0
	else 
		if #2 (hd d) = m 
		then 1 + number_in_month(tl d, m)
		else number_in_month(tl d, m)

fun number_in_months(d: DATE list, m: int list): int =
	if null m
	then 0
	else number_in_month(d, hd m) + number_in_months(d, tl m)

fun dates_in_month(d: DATE list, m: int) =
    if null d
    then []
    else
        if #2 (hd d) = m
        then hd d :: dates_in_month(tl d, m)
        else dates_in_month(tl d, m)

fun dates_in_months(d: DATE list, m: int list) =
    if null m
    then []
    else dates_in_month(d, hd m) @ dates_in_months(d, tl m)

fun nth(str: string list, n: int) =
    if n < 1
    then raise InvalidParameter
    else if n = 1
    then hd str
    else nth(tl str, n - 1)

fun get_nth(str: string list, n: int) = (nth(str, n)) handle InvalidParamenter => "never"

fun date_to_string(d: DATE) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 d) ^" "^ Int.toString(#3 d) ^", "^ Int.toString(#1 d)
    end

fun number_before_reaching_sum(sum: int, pos: int list) =
    let
        fun count_upto_n(pos: int list, total: int, n: int) =
            if total + hd(pos) >= sum then n
            else count_upto_n(tl(pos), total + hd(pos), n + 1)
    in
        count_upto_n(pos, 0, 0)
    end


fun what_month(day: int) =
    let
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(day, months)
    end

fun month_range(day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: DATE list) =
    if null dates
    then NONE
    else if null (tl dates)
    then SOME (hd dates)
    else
        let
            val tl_ans = oldest(tl dates)
        in
            if is_older(valOf tl_ans, hd dates)
            then tl_ans
            else SOME (hd dates)
        end


fun reasonable_date(d: DATE) =
    let
        val leap_year = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val year = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 , 31]

        fun valid_year(y: int) = y > 0
        fun valid_month(m: int) = m > 0 andalso m < 13

        fun is_leap_year(y: int) = y mod 400 = 0 orelse y mod 4 = 0 andalso y mod 100 <> 0

        fun get_day_month(days: int list, month: int) =
            if month = 1
            then hd days
            else get_day_month(tl days, month - 1)

    in
            if valid_year(#1 d) andalso valid_month(#2 d)
            then
                if is_leap_year(#1 d)
                then (#3 d) > 0 andalso (#3 d) <= get_day_month(leap_year, #2 d)
                else (#3 d) > 0 andalso (#3 d) <= get_day_month(year, #2 d)
            else false
    end


