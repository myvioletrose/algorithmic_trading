fibonacci <- function(high, low, trend = "down", extension = FALSE){
        
        diff = high - low
        pct = c(0.236, 0.382, 0.5, 0.618)
        ext_pct = c(0.618, 1, 1.382, 1.618, 2, 2.618)
        
        UpTrendRetracement = high - (diff * pct)
        UpTrendRetracement = UpTrendRetracement[UpTrendRetracement>0]
        UpTrendExtension = high + (diff * ext_pct)
        UpTrendExtension = UpTrendExtension[UpTrendExtension>0]
        
        DownTrendRetracement = low + (diff * pct)
        DownTrendRetracement = DownTrendRetracement[DownTrendRetracement>0]
        DownTrendExtension = low - (diff * ext_pct)
        DownTrendExtension = DownTrendExtension[DownTrendExtension>0]
        
        result = list(UR = sort(UpTrendRetracement), 
                      UE = UpTrendExtension,
                      DR = sort(DownTrendRetracement, decreasing = TRUE),
                      DE = DownTrendExtension)
        
        return(result)
        
}

# > fibonacci(high = 60, low = 40)
# $UR
# [1] 47.64 50.00 52.36 55.28
# 
# $UE
# [1]  72.36  80.00  87.64  92.36 100.00 112.36
# 
# $DR
# [1] 52.36 50.00 47.64 44.72
# 
# $DE
# [1] 27.64 20.00 12.36  7.64