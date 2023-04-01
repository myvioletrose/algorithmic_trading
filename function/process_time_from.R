# operation: when should we pull or delete data from? 
process_time_from <- function(tbl, num_of_day = 0, print_start_date = TRUE){
        t = tbl
	max_date = d(glue("
	with sub 
	as (
	    select symbol, 
	    max(date) as max_date 
	    from {t}
	    group by 1
	)
	select min(max_date) as max_date from sub
	")) %>% .$max_date
	start_date = max_date + num_of_day
	if(print_start_date){print(glue("the minimum max date from all symbols from {t} is {max_date}, and the start date returned by this function is {start_date}"))}
	return(start_date)
}