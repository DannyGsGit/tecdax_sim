library(curl)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

#### Import Datasets ####

generate_lag <- function(stock.df, max.lag) {
  high.temp <- embed(c(stock.df$Close, rep(NA, max.lag-1)),max.lag) %>% as.data.frame()
  high.temp <- apply(high.temp, 1, max)
  return(high.temp)
}

generate_stock_df <- function(endpoint) {
  # Get data from JSON API
  temp.stock <- fromJSON(endpoint)
  stock.df <- temp.stock$dataset$data %>% as.data.frame()
  colnames(stock.df) <- temp.stock$dataset$column_names
  stock.df$Name <- temp.stock$dataset$name
  
  # Format columns
  stock.df$Date <- ymd(stock.df$Date)
  
  numeric.cols <- c("Open", "High", "Low", "Close", "Volume")
  stock.df[,numeric.cols] <- sapply(stock.df[,numeric.cols], as.character)
  stock.df[,numeric.cols] <- sapply(stock.df[,numeric.cols], as.numeric)
  
  # Add rolling days column
  stock.df$hi.12mo <- generate_lag(stock.df, max.lag = 251)
  stock.df$hi.9mo <- generate_lag(stock.df, max.lag = 190)
  stock.df$hi.6mo <- generate_lag(stock.df, max.lag = 130)
  
  return(stock.df)
} 

stock.index <- c("https://www.quandl.com/api/v3/datasets/GOOG/FRA_EVT.json?api_key=3i5uyzas2RA9C9tyH3BS",
                 "https://www.quandl.com/api/v3/datasets/GOOG/FRA_ADV.json?api_key=3i5uyzas2RA9C9tyH3BS",
                 "https://www.quandl.com/api/v3/datasets/GOOG/FRA_JEN.json?api_key=3i5uyzas2RA9C9tyH3BS",
                 "https://www.quandl.com/api/v3/datasets/GOOG/FRA_AIXA.json?api_key=3i5uyzas2RA9C9tyH3BS",
                 "https://www.quandl.com/api/v3/datasets/GOOG/FRA_SMHN.json?api_key=3i5uyzas2RA9C9tyH3BS",
                 "https://www.quandl.com/api/v3/datasets/GOOG/FRA_FNTN.json?api_key=3i5uyzas2RA9C9tyH3BS",
                 "https://www.quandl.com/api/v3/datasets/GOOG/FRA_DLG.json?api_key=3i5uyzas2RA9C9tyH3BS",
                 "https://www.quandl.com/api/v3/datasets/GOOG/FRA_QIA.json?api_key=3i5uyzas2RA9C9tyH3BS",
                 "https://www.quandl.com/api/v3/datasets/GOOG/FRA_COK.json?api_key=3i5uyzas2RA9C9tyH3BS")

tecdax <- lapply(stock.index, generate_stock_df)
tecdax <- data.table::rbindlist(tecdax) %>% as.data.frame()


## Format columns
factor.cols <- c("Name")
tecdax[,factor.cols] <- sapply(tecdax[,factor.cols], as.factor)







#### Generate summary statistics ####

tecdax <- tecdax %>% group_by(Name) %>%
  mutate(pct.12mo = 100 * (round(Close / hi.12mo, 4) - 1),
         pct.9mo= 100 * (round(Close / hi.9mo, 4) - 1)) %>%
  ungroup()






#### Set Buy/Sell Rules ####

tecdax$Own <- NA
buy.threshold <- -9
sell.threshold <- -15
stop.loss.threshold <- -15

tecdax <- tecdax %>% group_by(Name) %>%
  arrange(Date) %>%
  mutate(Own = ifelse(pct.9mo >= buy.threshold, TRUE, Own)) %>%
  mutate(Own = ifelse(pct.9mo < sell.threshold, FALSE, Own)) %>%
  mutate(Own = ifelse(pct.9mo < buy.threshold & pct.9mo >= sell.threshold, lag(Own), Own)) %>%
  mutate(Own = zoo::na.locf(Own, na.rm = FALSE)) %>%
  ungroup()





#### Track Buy/Sell Cycles, w/ cumulative summing ####

tecdax <- tecdax %>% group_by(Name) %>%
  arrange(Date) %>%
  mutate(Buy.Price = ifelse(Own == TRUE & lag(Own) == FALSE, Close, NA)) %>%
  mutate(Sell.Price = ifelse(Own == FALSE & lag(Own) == TRUE, Close, NA)) %>%
  ungroup()

own.at.end <- tecdax %>% group_by(Name) %>%
  arrange(Date) %>%
  summarise(last.own = last(Own),
            last.close = last(Close)) %>%
  filter(last.own == TRUE) %>%
  ungroup()

bought <- sum(tecdax$Buy.Price, na.rm = TRUE)
sold <- sum(tecdax$Sell.Price, na.rm = TRUE) + sum(own.at.end$last.close)
profit <- sold - bought





#### Exploration plots ####

plot.data <- tecdax %>% filter(Name == "Evotec AG (EVT)")
tecdax.plot <- ggplot(plot.data, aes(Date, Close, colour = Own)) +
  geom_point()
tecdax.plot


