library(quantmod)
library(dplyr)






#~~~~~~~~~~~~~~~~~~
#### Functions ####
#~~~~~~~~~~~~~~~~~~

# Lookup stock histories and return a dataframe
get_stock_history <- function(target.symbol) {
  constituent <- as.data.frame(getSymbols(Symbols = target.symbol, src = "google", env = NULL))
  
  constituent$Date <- row.names(constituent)
  
  constituent$Symbol <- target.symbol
  
  colnames(constituent) <- c("Open", "High", "Low", "Close", "Volume", "Date", "Symbol")
  
  return(constituent)
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Get stock histories ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tecdax.index <- read.csv("./Data/Tecdax.csv", stringsAsFactors = FALSE)

tecdax <- lapply(1:nrow(tecdax.index), function(x) get_stock_history(tecdax.index$Symbol[x]))
tecdax <- as.data.frame(do.call(rbind, tecdax))






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



