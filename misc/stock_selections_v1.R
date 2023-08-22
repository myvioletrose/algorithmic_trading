#poc <- d(glue::glue("select * from {tbl}"))

tic()

stock_selections = sqldf("
with base 
as (
	select symbol, date, year
	, open, high, low, close
	, dmi_p, dmi_n, adx
	, message
	, sma50, sma200
	, ema100
	, ema100 * 1.1 as ema111
	, case when message = 1 then 1 
		when message = -1 then -10
		else message 
		end as message_buy_flag

	, macd_diff
	, lag(macd_diff, 1) over(partition by symbol order by date) as macd_diff_lag1
	, lag(macd_diff, 2) over(partition by symbol order by date) as macd_diff_lag2
	, lag(macd_diff, 3) over(partition by symbol order by date) as macd_diff_lag3
	, lag(macd_diff, 4) over(partition by symbol order by date) as macd_diff_lag4

	, macd_flag
	, case when macd_flag = 1 then 1
		when macd_flag = -1 then -10
		else macd_flag
		end as macd_temp_flag

	, ha_flag
	, case when ha_flag = 1 then 1
		when ha_flag = -1 then -10
		else ha_flag
		end as ha_temp_flag

	, rsi
	, lag(rsi, 1) over(partition by symbol order by date) as rsi_lag1
	, lag(rsi, 2) over(partition by symbol order by date) as rsi_lag2

	, cci
	, lag(cci, 1) over(partition by symbol order by date) as cci_lag1
	, lag(cci, 2) over(partition by symbol order by date) as cci_lag2

	from poc
	where year >= 2007

),

--must be first 'buy' between now and past X days, i.e., today equal 'buy' and there must be 'hold' (not 'sell') in day_lag1, and day_lag2, etc.
message_flag
as (
	select symbol, 
	date, 	
	message_ct,
	case when message = 1 and message_ct = 1 then 1 else 0 end as message_flag
	from (
		select symbol, date		
		, message
		, sum(message_buy_flag) over(partition by symbol order by date ROWS BETWEEN 4 preceding AND 0 preceding) as message_ct
		from base
	) x
), 

--must be ascending, diff > diff_lag1 > diff_lag2 > diff_lag3 > diff_lag4
macd_diff_flag
as (
	select symbol,
	date,
	case when (macd_diff > macd_diff_lag1) and 
		(macd_diff_lag1 > macd_diff_lag2) and 
		(macd_diff_lag2 > macd_diff_lag3) and
		(macd_diff_lag3 > macd_diff_lag4) 
		then 1 
		else 0 
		end as macd_diff_flag
	from base	
),

--must have at least 1 'buy' (and no 'sell') signal flagged by macd between now and past 4 days, i.e., today, day_lag1, day_lag2, day_lag3, and day_lag4
macd_flag
as (
	select symbol, 
	date,	
	macd_ct,
	case when macd_ct >0 then 1 else 0 end as macd_just_flag
	from (
		select symbol, date		
		, sum(macd_temp_flag) over(partition by symbol order by date ROWS BETWEEN 4 preceding AND 0 preceding) as macd_ct
		from base
	) x
),

--must have at least 1 'buy' (and no 'sell') signal flagged by ha between now and past 4 days, i.e., today, day_lag1, day_lag2, day_lag3, and day_lag4
ha_flag
as (
	select symbol, 
	date,	
	ha_ct,
	case when ha_ct >0 then 1 else 0 end as ha_just_flag
	from (
		select symbol, date		
		, sum(ha_temp_flag) over(partition by symbol order by date ROWS BETWEEN 4 preceding AND 0 preceding) as ha_ct
		from base
	) x
),

--rsi signals that the asset is just moving up from the 'oversold' area between now and past 9 days
rsi_flag
as (
	select symbol,
	date,	
	rsi_oversold_yn,
	case when rsi_oversold_temp_flag >0 then 1 else 0 end as rsi_oversold_flag
	from (
		select symbol,
		date,		
		rsi_oversold_yn,
		sum(rsi_oversold_yn) over(partition by symbol order by date ROWS BETWEEN 9 preceding AND 0 preceding) as rsi_oversold_temp_flag
		from (
			select symbol, date			
			, case when rsi > 35 and rsi_lag1 <= 35 and rsi_lag2 <= 35 then 1
				when rsi > 35 and (rsi > rsi_lag1) and rsi_lag1 > 35 and rsi_lag2 <= 35 then 1
				else 0 
				end as rsi_oversold_yn
			from base
		) x
	) y
),

--cci signals that the asset is just moving up from the 'oversold' area between now and past 9 days
cci_flag
as (
	select symbol,
	date,	
	cci_oversold_yn,
	case when cci_oversold_temp_flag >0 then 1 else 0 end as cci_oversold_flag
	from (
		select symbol,
		date,		
		cci_oversold_yn,
		sum(cci_oversold_yn) over(partition by symbol order by date ROWS BETWEEN 9 preceding AND 0 preceding) as cci_oversold_temp_flag
		from (
			select symbol, date			
			, case when cci > -100 and cci_lag1 <= -100 and cci_lag2 <= -100 then 1
				when cci > -100 and (cci > cci_lag1) and cci_lag1 > -100 and cci_lag2 <= -100 then 1
				else 0 
				end as cci_oversold_yn
			from base
		) x
	) y
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

        b.dmi_p,
        b.dmi_n,
        b.adx,

        b.sma50, 
        b.sma200,
	b.ema100,
	b.ema111,

	b.message,
	m.message_ct,
	m.message_flag,

	b.macd_diff,
	mdf.macd_diff_flag,

	b.macd_flag,
	ma.macd_ct,
	ma.macd_just_flag,

	b.ha_flag,
	ha.ha_ct,
	ha.ha_just_flag,

	b.rsi,
	b.rsi_lag1,
	b.rsi_lag2,
	r.rsi_oversold_yn,
	r.rsi_oversold_flag,

	b.cci,
	b.cci_lag1,
	b.cci_lag2,
	c.cci_oversold_yn,
	c.cci_oversold_flag

	from base b 
	join message_flag m on b.symbol = m.symbol and b.date = m.date
	join macd_diff_flag mdf on b.symbol = mdf.symbol and b.date = mdf.date
	join macd_flag ma on b.symbol = ma.symbol and b.date = ma.date
	join ha_flag ha on b.symbol = ha.symbol and b.date = ha.date
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

dmi_p,
dmi_n,
adx,

sma50, 
sma200,
ema100,
ema111,

message,
message_ct,
message_flag,

macd_diff,
macd_diff_flag,

macd_flag,
macd_ct,
macd_just_flag,

ha_flag,
ha_ct,
ha_just_flag,

rsi,
rsi_lag1,
rsi_lag2,
rsi_oversold_yn,
rsi_oversold_flag,

cci,
cci_lag1,
cci_lag2,
cci_oversold_yn,
cci_oversold_flag

from interim
where message = 1
and message_flag = 1
and close > open
and close > sma50
and macd_diff_flag = 1
and ( macd_just_flag = 1 or ha_just_flag = 1 )
and ( rsi_oversold_flag = 1 or cci_oversold_flag = 1 )
and ( rsi > 35 and rsi < 60 ) 
and ( cci > -100 and cci < 90 )
--and close > ema111
--and sma50 > sma200

order by symbol, date
")

toc()

dim(stock_selections)


