# this file calculates the number of stocks in the dataset, it builds on the 3_build_data.R script
# LGE; 10-Jul-22  

# clear workspace
rm(list = ls())
graphics.off()

# Packages
library(tidyverse)
library(igraph)
library(ineq)
library(sjstats)
library(glmmTMB)
library(data.table)
library(base)
require(dplyr)

# Directories
datadir2 = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data" #LGE

# Read data
trade <- read.csv(file.path(datadir2, "fish_trade.csv"), as.is=T)
stocks  = read.csv(file.path(datadir2, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)
match = read.csv(file.path(datadir2,"matchingstocksHS92.csv"), as.is=T) # contains NA values for sci_name
trade_duration <- read.csv(file.path(datadir2, "actual_trade_duration.csv"), as.is=T)#newest trade duration file


######################################  # original stocks in data  ##########################################
# Are there only unique super per species (sci_name), year, country?
test_uniq <- data.frame(species = stocks$sci_name, year = stocks$year, stockid =stocks$stockid)
stocks_uniq <- subset(stocks, (duplicated(test_uniq)) & !(is.na(super)))
#there are no duplicates in stocks_uniq

# what is the number of stocks in the original data? 
test_uniq$isospp <- paste(test_uniq$species, test_uniq$stockid, sep="_")
stocks_uniqnum <- test_uniq %>% 
  distinct(test_uniq$isospp, .keep_all = T)
#1740 stocks in stocks_uniqnum!

######################################  Format data  ##########################################
# trade data file
trade <- trade %>% 
  rename(year=t, hs92desc=Shortdescription.HS1992, hs92code=Commodity.HS2012code, value_usd=v, quantity_mt=q, group=group_name) %>% 
  select(imp_iso3, group, hs92code, hs92desc, exp_iso3, year, value_usd, quantity_mt) %>% 
  arrange(imp_iso3, group, hs92code, hs92desc, exp_iso3, year)

# match file
match = match %>% select(Shortdescription.HS1992, comm_name, sci_name)%>% 
  rename(hs92desc=Shortdescription.HS1992) 
match = trade %>% left_join(match) 

#################################################   REMOVE

##########################################   FIX WRONG MATCHES   ##################################################
###  remove sci_name with multiple matches to group
# remove sci_name multiple matches to group
t1 <- data.frame(sci_name = match$sci_name, group = match$group) # new df including sciname,group
# unique pairs
t2 <- unique(t1[c("sci_name","group")]) # new df incl only unique values of sci_name group pairs
# check 
t3 <- t2[1] # used in next line
t4 <- subset(t2, (duplicated(t3))) # new df incl only duplicate observations of sci_name
# now choose all duplicates and remove them from the match data after line 40 
t5 <- unique(t4[c("sci_name")]) # 
t6 = t2[(t2$sci_name %in% t5$sci_name),] # identify sci_name with double matches to group 
#remove data
rm(t1,t2,t3,t4,t5,t6)

# identified wrong matches 
wrong_match <- data.frame("sci_name" = c("Homarus americanus","Homarus gammarus","Jasus edwardsii","Jasus lalandii","Jasus novaehollandiae","Jasus paulensis","Jasus tristani","Metanephrops challengeri","Palinurus elephas","Palinurus gilchristi","Panulirus argus","Panulirus cygnus","Panulirus homarus","Paralichthys olivaceus","Reinhardtius hippoglossoides","Thenus orientalis", "Pecten maximus", "Arripis trutta", "Salmo salar", "Oncorhynchus tshawytscha", "Oncorhynchus keta", "Oncorhynchus kisutch", "Oncorhynchus nerka"),
                          "group" = c("homarus","homarus","lobster","lobster","lobster","lobster","lobster","lobster","lobster","lobster","lobster","lobster","lobster","flatfish","halibut","lobster", "sardines", "salmon", "salmon", "salmon", "salmon", "salmon", "salmon"))

# new columns to remove the wrong matches
match$grsci <- paste(match$group, match$sci_name, sep="_") 
wrong_match$grsci <- paste(wrong_match$group, wrong_match$sci_name, sep="_")

match = match[!(match$grsci %in% wrong_match$grsci),] # remove sci_name with multiple matches to group

#################################################   REMOVE


# stocks file 
stocks = stocks %>% select(stockid, sci_name, comm_name, super, year, iso3) %>% 
  rename(exp_iso3 = iso3)%>% left_join(match) %>% #join based on exporter
  filter(!is.na(super))%>%filter(!is.na(hs92desc)) %>%
  select(stockid, sci_name, exp_iso3, group, year, super)%>%
  distinct() #remove all doubles that are there because several stocks match with a group this wont work bc we keep sci_name and group above

rm(wrong_match)

######################################  Calc # stocks remaining  ##########################################
#### remove aquaculture and environmental variability driven species
# check which groups are there
check = match %>% select('group') 
check= unique(check)

# spp to remove
remove = c('tuna') #vector with groups to be removed
ndata = subset(match,!(group %in% remove))

# create a matching file btw group and sci_name 
nmatch <- ndata[,c("group","sci_name")]
nmuniq <- nmatch %>% 
  distinct(group, sci_name, .keep_all = F)

# rename, leftjoin to remove groups not included and spp without matches
nmuniq <- nmuniq %>% 
  rename(species=sci_name)
nspecies = stocks_uniqnum %>% left_join(nmuniq)  %>% 
  filter(!is.na(group))
nsppfinal = subset(nspecies,!(group %in% remove))
#876 stocks in stocks_uniqnum!

