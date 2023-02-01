# creates matchingstocksHS92.csv file
#clear workspace
rm(list = ls())
graphics.off()
require(dplyr)

# set working directory
setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data") #LGE

### read data
# downloaded matching files
hs_to_hs = read.csv("1992_to_2012.csv") %>% rename(Commodity.HS2012code=X...Commodity.HS2012code) # HS1992 and HS2012
ct_to_fao = read.csv("ct_to_fao.csv") %>% rename(ID_FAOTrade=X...ID_FAOTrade) # ID_FAO trade and HS2012
# self produced matching files
faostocks = read.csv("stocks_match.csv") %>% rename(Stock_Id=X...Stock_Id,sci_name=sci_name_orig) # FAO Stock_ID, species, iso3, years, id_fao_trade, id_comtrade
fao_trade_to_stocks = read.csv("trade_to_stocks.csv") %>% rename(main_id_relation=X...main_id_relation) # FAO Stock_ID and ID_FAO trade
# data files
stocks = read.csv("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/fao_stock_status/1950_2017_FAO_bbmsy_timeseries_merge.csv") # stockid, iso3, year, comm_name, super  
trade = read.csv("fish_trade.csv")

# clean data: select columns
faostocks <- faostocks %>% select("Stock_Id", "fao_code", "fao_area", "iso3", "country", "comm_name","sci_name")
stocks <- stocks %>% select("fao_code", "fao_area", "iso3", "country", "comm_name","sci_name","iso3", "year", "super")

### merging data
CT92_to_fao_trade = ct_to_fao %>% left_join(hs_to_hs, by = c("Commodity.HS2012code", "Commodity.HS2012"))
CT92_to_stock_id = CT92_to_fao_trade %>% left_join(fao_trade_to_stocks, by = c("ID_FAOTrade", "Commodity")) %>% rename(Stock_Id=stockid.AG)
CT92_to_stock_match = CT92_to_stock_id %>% left_join(faostocks, by = "Stock_Id")
CT92_to_actual_stocks = CT92_to_stock_match %>% left_join(stocks, by = c("fao_code", "fao_area", "iso3", "country", "comm_name","sci_name"))

# select and save
min_match = CT92_to_actual_stocks %>% select(Shortdescription.HS1992, comm_name, sci_name) %>% distinct()
#write.csv(min_match, "matchingstocksHS92.csv")


