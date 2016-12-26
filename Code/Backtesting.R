library(quantmod)
library(dplyr)

#### Get stock histories ####
getSymbols('FRA:EVT', src = 'google')
getSymbols('FRA:ADV', src = 'google')
getSymbols('FRA:BC8', src = 'google')
getSymbols('FRA:COK', src = 'google')
getSymbols('FRA:AFX', src = 'google')

#### Chart ####
chartSeries(`FRA:EVT`)
barChart(`FRA:EVT`)
candleChart(`FRA:EVT`, multi.col = TRUE, theme = "white")

chartSeries(to.weekly(`FRA:EVT`),up.col='green',dn.col='red')
addBBands()
addMACD()
