library(quantmod)
library(dplyr)
library(lubridate)





#~~~~~~~~~~~~~~~~~~
#### Functions ####
#~~~~~~~~~~~~~~~~~~

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


# Generate lagging indicators
generate_lag <- function(stock.df, max.lag) {
  # Create temp column of lagging indicators
  high.temp <- embed(c(stock.df$Close, rep(NA, max.lag-1)),max.lag) %>% as.data.frame()
  high.temp <- apply(high.temp, 1, max)
  return(high.temp)
}


# generate_stock_df <- function(endpoint) {
#   # Get data from JSON API
#   temp.stock <- fromJSON(endpoint)
#   stock.df <- temp.stock$dataset$data %>% as.data.frame()
#   colnames(stock.df) <- temp.stock$dataset$column_names
#   stock.df$Name <- temp.stock$dataset$name
#   
#   # Format columns
#   stock.df$Date <- ymd(stock.df$Date)
#   
#   numeric.cols <- c("Open", "High", "Low", "Close", "Volume")
#   stock.df[,numeric.cols] <- sapply(stock.df[,numeric.cols], as.character)
#   stock.df[,numeric.cols] <- sapply(stock.df[,numeric.cols], as.numeric)
#   
#   # Add rolling days column
#   stock.df$hi.12mo <- generate_lag(stock.df, max.lag = 251)
#   stock.df$hi.9mo <- generate_lag(stock.df, max.lag = 190)
#   stock.df$hi.6mo <- generate_lag(stock.df, max.lag = 130)
#   
#   return(stock.df)
# } 




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Get stock histories ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tecdax.index <- read.csv("./Data/Tecdax.csv", stringsAsFactors = FALSE)

tecdax <- lapply(1:nrow(tecdax.index), function(x) get_stock_history(tecdax.index$Symbol[x]))
tecdax <- as.data.frame(do.call(rbind, tecdax))

# Get lagging indicators
zz <- get_stock_history(tecdax.index$Symbol[1])
zz$hi.52w <- generate_lag(zz, max.lag = 251)




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



