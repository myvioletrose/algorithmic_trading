#date | symbol | open | high | low | close | volume | adjusted

write.table(output, "clipboard-16384", sep = "\t", row.names = FALSE, col.names = TRUE)

# get vector of symbols
get_symbols_query = glue::glue("
with sub
as (
	select symbol, count(1)
	, min(date) as min_date
	, max(date) as max_date
	from stock.stock_fact
	group by symbol
)

select symbol
from sub
where extract(year from min_date) <=2013
and extract(year from max_date) = 2022
and symbol != 'HLT'
order by symbol
")

get_symbols_df = dbGetQuery(con, get_symbols_query)

##################
query2 = glue::glue("
with sub
as (
	select symbol, count(1) as flag
	, min(date) as min_date
	, max(date) as max_date
	from stock.stock_fact
	group by symbol
)

select *
from sub
order by flag desc
")

checkSize = dbGetQuery(con, query2)
