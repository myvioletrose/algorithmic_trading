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

##############################################################
test = sqldf("
with sub
as (
        select symbol, date, adj_close as close
        from df3
),

sub2
as (
        select symbol, date, close
        , lag(close, 4) over(partition by symbol order by date) as close_4
        from sub
),

sub3
as (
        select symbol, date, close
        , case when close < close_4 then -1
        when close > close_4 then 1
        else 0 
        end as flag
        from sub2
),

sub4
as (
        select symbol, date, close, flag
        , sum(flag) over(partition by symbol order by date ROWS BETWEEN 8 preceding AND 0 preceding) as magic9
        from sub3
)

select symbol
, date
, close
, flag
, case when magic9 >=7 then -1 
        when magic9 <=-7 then 1
        else 0 end as dem7
, case when magic9 >=8 then -1 
        when magic9 <=-8 then 1
        else 0 end as dem8
, case when magic9 =9 then -1 
        when magic9 =-9 then 1
        else 0 end as dem9
from sub4
order by symbol, date
")

test2 <- test %>%
        select(-flag) %>%
        dplyr::mutate(year = lubridate::year(date)) %>%
        dplyr::select(symbol, year, date, everything()) %>%
        tidyr::gather("dem", "signal", dem7:dem9)

test2 %>% write_clip()
