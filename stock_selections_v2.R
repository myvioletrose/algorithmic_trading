#poc <- d(glue::glue("select * from {tbl}"))

tic()

stock_selections = sqldf("
with base 
as (
	select symbol, date, year
	, open, high, low, close
	, lag(close, 1) over(partition by symbol order by date) as close_lag1

	, dmi_p, dmi_n, adx
	, message
	, sma50, sma200
	, ema100
	, ema100 * 1.1 as ema111

	, rsi
	, lag(rsi, 1) over(partition by symbol order by date) as rsi_lag1
	, lag(rsi, 2) over(partition by symbol order by date) as rsi_lag2

	, cci
	, lag(cci, 1) over(partition by symbol order by date) as cci_lag1
	, lag(cci, 2) over(partition by symbol order by date) as cci_lag2

	from poc
	where year >= 2007
),

 	--rsi signals that the asset is moving up from the bottom
rsi_flag1 
as (
	select symbol, date
	, case when rsi < 35
		and rsi > rsi_lag1 
		and rsi_lag1 > rsi_lag2
		then 1 
		else 0
		end as rsi_oversold_yn1
	from base b
),

rsi_flag2
as (
	select symbol,
	date,		
	case when rsi_oversold_temp_flag >0 then 1 else 0 end as rsi_oversold_yn2
	from (
		select symbol,
		date,				
		sum(rsi_oversold_yn2) over(partition by symbol order by date ROWS BETWEEN 2 preceding AND 0 preceding) as rsi_oversold_temp_flag
		from (
			select symbol, date			
			, case when rsi > 35 and rsi_lag1 <= 35 and rsi_lag2 <= 35 then 1
				when rsi > 35 and (rsi > rsi_lag1) and rsi_lag1 > 35 and rsi_lag2 <= 35 then 1
				else 0 
				end as rsi_oversold_yn2
			from base
		) x
	) y
),

rsi_flag
as (
	select r1.symbol, r1.date
	, case when (r1.rsi_oversold_yn1 + r2.rsi_oversold_yn2) >0 then 1 else 0 end as rsi_oversold_flag
	from rsi_flag1 r1
	join rsi_flag2 r2 on r1.symbol = r2.symbol and r1.date = r2.date
),

--cci signals that the asset is moving up from the bottom
cci_flag1 
as (
	select symbol, date
	, case when cci < -100
		and cci > cci_lag1 
		and cci_lag1 > cci_lag2
		then 1 
		else 0
		end as cci_oversold_yn1
	from base b
),

cci_flag2
as (
	select symbol,
	date,		
	case when cci_oversold_temp_flag >0 then 1 else 0 end as cci_oversold_yn2
	from (
		select symbol,
		date,				
		sum(cci_oversold_yn2) over(partition by symbol order by date ROWS BETWEEN 2 preceding AND 0 preceding) as cci_oversold_temp_flag
		from (
			select symbol, date			
			, case when cci > -100 and cci_lag1 <= -100 and cci_lag2 <= -100 then 1
				when cci > -100 and (cci > cci_lag1) and cci_lag1 > -100 and cci_lag2 <= -100 then 1
				else 0 
				end as cci_oversold_yn2
			from base
		) x
	) y
),

cci_flag
as (
	select c1.symbol, c1.date
	, case when (c1.cci_oversold_yn1 + c2.cci_oversold_yn2) >0 then 1 else 0 end as cci_oversold_flag 
	from cci_flag1 c1
	join cci_flag2 c2 on c1.symbol = c2.symbol and c1.date = c2.date
),

interim
as (

	select b.symbol, 
	b.date, 
	b.year,
	b.open,
	b.high,
	b.low,
	b.close,
	b.close_lag1,

    b.dmi_p,
    b.dmi_n,
    b.adx,

    b.sma50, 
    b.sma200,
	b.ema100,
	b.ema111,

	b.message,
	
	b.rsi,
	b.rsi_lag1,
	b.rsi_lag2,	
	r.rsi_oversold_flag,

	b.cci,
	b.cci_lag1,
	b.cci_lag2,	
	c.cci_oversold_flag

	from base b 	
	join rsi_flag r on b.symbol = r.symbol and b.date = r.date
	join cci_flag c on b.symbol = c.symbol and b.date = c.date

)

select symbol, 
date, 
year,
open,
high,
low,
close,
close_lag1,

dmi_p,
dmi_n,
adx,

sma50, 
sma200,
ema100,
ema111,

message,

rsi,
rsi_lag1,
rsi_lag2,
rsi_oversold_flag,

cci,
cci_lag1,
cci_lag2,
cci_oversold_flag

from interim
where message = 1
and close > close_lag1
and ( rsi_oversold_flag = 1 or cci_oversold_flag = 1 )
and rsi < 70
and cci < 100 

order by symbol, date
")

toc()

dim(stock_selections)


