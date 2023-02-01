#script to get all trade data for all the stocks that we have stock data from 
setwd("C:/Users/Maartje Oostdijk/Dropbox/BACI/")
require(tidyverse)

trade <-data.frame()
for(x in c(1995:2016)){
  trade <- bind_rows(trade, read.csv(paste0('baci92_', x, '.csv'))) #read and bind each of the csv files
}

head(trade)

codes = read.csv("C:/Users/Maartje Oostdijk/Dropbox/BACI/hs_codes.csv")#fish HS codes that we've matched 1992 to 2012 
countries = read.csv("C:/Users/Maartje Oostdijk/Dropbox/BACI/countries.csv")#country codes
groups = read.csv("C:/Users/Maartje Oostdijk/Dropbox/Laura/groups.csv", header=T, encoding="UTF-8")%>%#commodity groups linked to HS codes
  rename(Shortdescription.HS1992=X.U.FEFF.Shortdescription.HS1992)%>%
  select(Shortdescription.HS1992, group_name)%>%
  distinct()

countries2 = countries%>%
  rename(j=i)

#left join all the trade to the codes we have selected (HS2012->HS1992)
fish_trade = codes%>%
  rename(hs6= Code.HS1992)%>%
  left_join(trade)%>%
  left_join(countries)%>%
  rename(exp_iso3 = iso3)%>%
  select(-X, -country,-iso2, -i)%>%
  left_join(countries2)%>%
  rename(imp_iso3 = iso3)%>%
  select(-X, -country,-iso2, -j)

#left join the grouping, only fish trade that has a match to a group at good enough resolution are selected
fish_trade = fish_trade %>%
  left_join(groups)
fish_trade_clean = fish_trade%>%
  filter(!is.na(group_name))
  
  
write.csv(fish_trade_clean, "C:/Users/Maartje Oostdijk/Dropbox/Laura/fish_trade.csv")
  


