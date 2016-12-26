library(quantmod)
library(dplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Get stock histories ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tecdax.index <- read.csv("./Data/Tecdax.csv", stringsAsFactors = FALSE)

for (i in 1:nrow(tecdax.index)) {
  quantmod::getSymbols(tecdax.index$Symbol[i], src = 'google')
}



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
