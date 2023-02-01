# Script to calculate actual trade duration (sum up values for a continous trade so that each trade has the correct duration)
# Maartje Oostdijk, April 2022

#clear workspace
rm(list = ls())
graphics.off()

#directories
setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data") #LGE
datadir = "/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data" #LGE

# libraries
library(tidyverse)

# read data
duration <- read.csv(file.path(datadir, "exports_duration.csv"), as.is=T) # trade duration from first trade duration script

###################################################################################################################
######################################################   CLEAN DATA   #############################################
###################################################################################################################

### new names for columns
duration = duration %>%
  rename(new_export = new.export, value = total.value, 
         quantity = total.quantity, year = t)%>%
  select(-X, -value, -quantity)%>%
  mutate(imp_exp = paste0(imp_iso3, "_", exp_iso3))%>% #name for exporter - importer combination
  arrange(year)%>% 
  filter(!is.na(exp_iso3), !is.na(imp_iso3))


###################################################################################################################
####################################   CREATE INDEX FOR CONTINUOUS TRADE   ########################################
###################################################################################################################

### observations during all years of a continuous trade (unique exp-imp-spp group combinations) have the same trade duration. 

# vector of all groups
groups <- sort(unique(duration$group_name))

# loop for each species group
for (i in 1:length(groups)){ 
  data <- duration %>% 
    filter(group_name==groups[i]) 
  couples = sort(unique(data$imp_exp))
  
  # loop for each imp_exp couple  
  for (a in 1: length(couples)){ 
    gdata <- data%>% 
      filter(imp_exp==couples[a])
    
    # loop for each year
    years = sort(unique(gdata$year))
    index = 1
    for (t in c(1:length(years))){
      
      ### we create an index that assigns one value for all years of continuous trade (unique exp-imp-spp group combinations)
      #the subsetting here is done for the year t and t-1 is the year in the previous year that this trade is occuring
      #that does not have to be the previous year
      links0 <- gdata[gdata$year==years[t-1], ]
      links <- gdata[gdata$year==years[t], ]
      
      links = na.omit(links)
      links0 = na.omit(links0)
      
      #if the duration of trade in the previous time step was bigger than in the current THIS time step is a new trade, so index gets assigned a higher value
      
      if(length(links0$new_export) >0){
        if(links$duration < links0$duration | links0$duration ==  links$duration){index = index +1}
      }else{index = index}
      
      #we rename index to be the ID of the trade and in case there are multiple NEW trades between the same importer exporter couple, this ID takes on higher values
      #if it's the same trade all the way through this index just remains the same
      df = data.frame(trade_index = index, duration = links$duration, year = years[t], group= groups[i], imp_exp =  couples[a])
      print(df)
      
      out_put = if(a==1 & t == 1 & i == 1){out_put = df}else{out_put = bind_rows(out_put, df)}
    }
    
  }
}

###################################################################################################################
############################################   CREATE DURATION DATA   #############################################
###################################################################################################################

# merge index and duration data
gduration = out_put %>% rename(group_name = group)%>%
  left_join(duration) %>%
  distinct()

# for each commodity, exporter-importer couple and trade_index combination we identify maximum duration value
max_trade = gduration%>%group_by(group_name, imp_exp, trade_index)%>%
  summarise(max_trade_duration = max(duration))

# assign maxim duration value to all years of continuous trade. If a country is trading for three years in a row the trade duration is 3 for all those years and not 1,2,3. 
gduration2 = gduration%>% left_join(max_trade) %>%
  mutate(actual_duration = ifelse(duration<max_trade_duration, max_trade_duration, duration)) #replace the duration by max trade duration if it's different for the continuous trades

# write.csv(gduration2, "actual_trade_duration.csv")




