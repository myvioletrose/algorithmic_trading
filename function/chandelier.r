#Install the latest version of this package by entering the following in R:
#install.packages("remotes")
#remotes::install_github("pverspeelt/Quantfunctions")
#https://rdrr.io/github/pverspeelt/Quantfunctions/

#' Chandelier Exit
#' 
#' Calculates the chandelier exits in an up or down trend. The calculated exit 
#' points are used as a stop loss in a trend following trading strategy.
#' 
#' In an up-trend the default formula is:
#' Highest High in last 22 days - 3 * ATR for 22 days
#' 
#' In a down-trend the formula is reversed:
#' Lowest Low in last 22 days + 3 * ATR for 22 days
#'
#' @param x Object that is coercible to xts or matrix and contains High-Low-Close prices.
#' @param n Number of periods for chandelier period. Default is 22.
#' @param coef ATR coefficient. Default is 3
#' @param trend Indicates if chandelier should be calculated for the up-trend or down-trend. 
#' Default is up. Possible values are "up" or "down".
#' 
#' @family protective stops
#' @return Returns the chandelier exit points which can be used as stop loss in a trend following strategy.
#' @export
#'
#' @examples
#' \dontrun{
#' libary(quantmod)
#' ADM <- getSymbols("ADM", 
#'                   from = "2018-01-01", 
#'                   to = "2018-07-01",
#'                   auto.assign = FALSE)
#' chartSeries(ADM)
#' addTA(chandelier(ADM), on = 1)
#' }
#' 
#' 
chandelier <- function(x, n = 22, coef = 3, trend = "up"){
  
  # input tests
  x <- xts::try.xts(x, error = as.matrix)
  
  if(n < 1 || n > NROW(x)) 
    stop(glue("n = {n} is outside valid range: [1, {NROW(x)}]"), 
         call. = FALSE)
  
  if(coef <= 0) 
    stop("ATR coefficient should have a positive value", 
         call. = FALSE)
  
  if(!trend %in% c("up", "down")) 
    stop(glue('Trend should be "up" or "down".
              You supplied: {trend}.'), 
              call. = FALSE)
  
  if(trend == "down"){  
    chandelier <- TTR::runMin(quantmod::Lo(x), n) + coef * TTR::ATR(quantmod::HLC(x), n)[,"atr"]  
  } else {
    chandelier <- TTR::runMax(quantmod::Hi(x), n) - coef * TTR::ATR(quantmod::HLC(x), n)[,"atr"]
  }
  
  names(chandelier) <- "chandelier_stop"
  
  return(chandelier)
}