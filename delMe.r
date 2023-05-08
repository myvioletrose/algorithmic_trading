library(quantmod)
getSymbols(Symbols = "SPY", 
           env = parent.frame(),
           reload.Symbols = FALSE,
           verbose = FALSE,
           warnings = TRUE,
           src = "yahoo",
           symbol.lookup = TRUE,
           auto.assign = getOption('getSymbols.auto.assign',TRUE))

spy = SPY %>% as.data.frame() %>% tibble::rownames_to_column() %>%
        dplyr::select(date = rowname, everything())

names(spy) = qc(date, open, high, low, close, volume, adjusted)

library(sqldf)

test = sqldf("
with sub
as (
        select date, adjusted as close
        from spy
),

sub2
as (
        select date, close
        , lag(close, 4) over(order by date) as close_4
        from sub
),

sub3
as (
select date, close
, case when close < close_4 then -1
        when close > close_4 then 1
        else 0 
        end as flag
        from sub2
)

select date, close, flag
, sum(flag) over(order by date ROWS BETWEEN 9 preceding AND 1 preceding) as dem9
from sub3
order by date
")
