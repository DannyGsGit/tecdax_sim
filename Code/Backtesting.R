library(quantmod)
library(dplyr)
library(lubridate)





#~~~~~~~~~~~~~~~~~~
#### Functions ####
#~~~~~~~~~~~~~~~~~~

# Generate lagging indicators
generate_lag <- function(stock.df, max.lag) {
  # Create temp column of lagging indicators
  high.temp <- embed(c(stock.df$Close, rep(NA, max.lag-1)),max.lag) %>% as.data.frame()
  high.temp <- apply(high.temp, 1, max)
  return(high.temp)
}




# Lookup stock histories and return a dataframe
get_stock_history <- function(target.symbol) {
  # Get stock histories from a target symbol
  constituent <- as.data.frame(getSymbols(Symbols = target.symbol, src = "google", env = NULL))  # Get history
  constituent$Date <- row.names(constituent)  # Move dates into column, from the row name
  constituent$Symbol <- target.symbol  # Place symbol in a column
  colnames(constituent) <- c("Open", "High", "Low", "Close", "Volume", "Date", "Symbol")  # Re-label columns with generic names
  
  # Format dates, and re-order
  constituent$Date <- ymd(constituent$Date)
  constituent <- constituent %>% arrange(desc(Date))
  
  return(constituent)
}



# Get the stock history and decorate with summary stats
build_decorated_stock <- function(target.symbol) {
  # Get stock history
  constituent <- get_stock_history(target.symbol)
  
  # Add lagging indicators
  constituent <- constituent %>% mutate(hi.12mo = generate_lag(constituent, max.lag = 251),
                                        hi.9mo = generate_lag(constituent, max.lag = 190),
                                        hi.6mo = generate_lag(constituent, max.lag = 130)) %>%
    mutate(pct.of.12mo.hi = 100 * round(Close / hi.12mo, 4),
           pct.of.9mo.hi = 100 * round(Close / hi.9mo, 4),
           pct.of.6mo.hi = 100 * round(Close / hi.6mo, 4))
  
  return(constituent)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Get stock histories ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tecdax.index <- read.csv("./Data/Tecdax.csv", stringsAsFactors = FALSE)

tecdax <- lapply(1:nrow(tecdax.index), function(x) build_decorated_stock(tecdax.index$Symbol[x]))
tecdax <- as.data.frame(do.call(rbind, tecdax))













# #### Set Buy/Sell Rules ####
# 
# tecdax$Own <- NA
# buy.threshold <- -9
# sell.threshold <- -15
# stop.loss.threshold <- -15
# 
# tecdax <- tecdax %>% group_by(Name) %>%
#   arrange(Date) %>%
#   mutate(Own = ifelse(pct.9mo >= buy.threshold, TRUE, Own)) %>%
#   mutate(Own = ifelse(pct.9mo < sell.threshold, FALSE, Own)) %>%
#   mutate(Own = ifelse(pct.9mo < buy.threshold & pct.9mo >= sell.threshold, lag(Own), Own)) %>%
#   mutate(Own = zoo::na.locf(Own, na.rm = FALSE)) %>%
#   ungroup()
# 
# 
# 
# 
# 
# #### Track Buy/Sell Cycles, w/ cumulative summing ####
# 
# tecdax <- tecdax %>% group_by(Name) %>%
#   arrange(Date) %>%
#   mutate(Buy.Price = ifelse(Own == TRUE & lag(Own) == FALSE, Close, NA)) %>%
#   mutate(Sell.Price = ifelse(Own == FALSE & lag(Own) == TRUE, Close, NA)) %>%
#   ungroup()
# 
# own.at.end <- tecdax %>% group_by(Name) %>%
#   arrange(Date) %>%
#   summarise(last.own = last(Own),
#             last.close = last(Close)) %>%
#   filter(last.own == TRUE) %>%
#   ungroup()
# 
# bought <- sum(tecdax$Buy.Price, na.rm = TRUE)
# sold <- sum(tecdax$Sell.Price, na.rm = TRUE) + sum(own.at.end$last.close)
# profit <- sold - bought




#~~~~~~~~~~~~~~
#### Chart ####
#~~~~~~~~~~~~~~

# chartSeries(`COPMX,A:GER`)
# barChart(`FRA:EVT`)
# candleChart(`FRA:EVT`, multi.col = TRUE, theme = "white")
# 
# chartSeries(to.weekly(`FRA:EVT`),up.col='green',dn.col='red')
# addBBands()
# addMACD()



