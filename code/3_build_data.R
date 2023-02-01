# this file builds the data set for the analysis
# LGE, 10-Jul-22

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
library(WDI)
library(countrycode)

# Directories
datadir1 <- "~/Documents/SESYNC/Files/FISHMAR-data/rq1/previous/data" # for governance data
datadir2 = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data" # trade and fisheries data

# Read data
trade <- read.csv(file.path(datadir2, "fish_trade.csv"), as.is=T)
stocks  = read.csv(file.path(datadir2, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)
match = read.csv(file.path(datadir2,"matchingstocksHS92.csv"), as.is=T) # contains NA values for sci_name
trade_duration <- read.csv(file.path(datadir2, "actual_trade_duration.csv"), as.is=T)#newest trade duration file


###################################################################################################################
################################################    GOVERNANCE   ##################################################
###################################################################################################################

### read governance data
wgi1 = read.csv(file.path(datadir1,"wgi_government_effectiveness.csv"), as.is=T,check.names=FALSE) %>% select(-c(1)) %>% rename(exp_iso3=WBCode)
wgi2 = read.csv(file.path(datadir1,"wgi_regulatory_quality.csv"), as.is=T,check.names=FALSE)%>% select(-c(1)) %>% rename(exp_iso3=WBCode)
wgi3 = read.csv(file.path(datadir1,"wgi_rule_of_law.csv"), as.is=T,check.names=FALSE)%>% select(-c(1)) %>% rename(exp_iso3=WBCode)
wgi4 = read.csv(file.path(datadir1,"wgi_control_of_corruption.csv"), as.is=T,check.names=FALSE) %>%  rename(exp_iso3='')

# prep data
wgi1 <- gather(wgi1, year, government_effectiveness, -exp_iso3, factor_key=TRUE)
wgi2 <- gather(wgi2, year, regulatory_quality, -exp_iso3, factor_key=TRUE)
wgi3 <- gather(wgi3, year, rule_of_law, -exp_iso3, factor_key=TRUE)
wgi4 <- gather(wgi4, year, control_of_corruption, -exp_iso3, factor_key=TRUE)

# merge all indicators
wgi <- wgi1 %>% left_join(wgi2) %>% left_join(wgi3) %>% left_join(wgi4) %>% select(year,exp_iso3,government_effectiveness,regulatory_quality,rule_of_law,control_of_corruption)
wgi$year <- as.numeric(as.character(wgi$year))

### add missing years 1995,1997,1999,2001
# subset dataset 1996-2002 and rest
wgi_2003 <- wgi[rowSums(wgi[1] > 2002) > 0, ]
wgi_1996 <- wgi[rowSums(wgi[1] < 2003) > 0, ]

# create repetitions of data
wgi_1996 <- wgi_1996 %>% slice(rep(1:n(), each = 2)) %>% rename(year1=year)

# replace each one duplicate with new year
wgi_1996 <- wgi_1996 %>% mutate(year2=year1-1) 
wgi_1996 <- within(wgi_1996, year <- ifelse(duplicated(wgi_1996[,1:6]), year1, year2)) %>% select(-year1,-year2)

# merge data
wgi <- rbind(wgi_1996,wgi_2003)

### calculate composite indicator
wgi <- wgi %>% mutate(composite_governance = rowMeans(select(wgi,c(government_effectiveness,regulatory_quality,rule_of_law,control_of_corruption)), na.rm = TRUE)) %>%
  select(exp_iso3,year,composite_governance)

#remove unused df
rm(wgi1, wgi2, wgi3, wgi4, wgi_1996, wgi_2003)

###################################################################################################################
#################################################   CLEAN DATA   ##################################################
###################################################################################################################

##################################   FORMAT STOCK AND TRADE DATA   ##################################################
# trade data file
trade <- trade %>% 
  rename(year=t, hs92desc=Shortdescription.HS1992, hs92code=Commodity.HS2012code, value_usd=v, quantity_mt=q, group=group_name) %>% 
  select(imp_iso3, group, hs92code, hs92desc, exp_iso3, year, value_usd, quantity_mt) %>% 
  arrange(imp_iso3, group, hs92code, hs92desc, exp_iso3, year)

# match file
match = match %>% select(Shortdescription.HS1992, comm_name, sci_name)%>% 
  rename(hs92desc=Shortdescription.HS1992) 
match = trade %>% left_join(match) 

# unique spp and group matches saved
x <- unique(match[c("sci_name", "group")])
# write.csv(x, "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data/spp_groups.csv")
rm(x)

# trade duration file
# patch the issue that single trades from 1995 are NA by merging original trade data with the duration data and setting those missing values to 1
tr_data = trade %>% 
  select(year, exp_iso3, imp_iso3, group) %>% 
  distinct()%>% 
  rename(group_name = group)
trade_duration = tr_data %>%
  left_join(trade_duration)
rm(tr_data)
trade_duration$actual_duration[is.na(trade_duration$actual_duration)] = 1

# clean file
trade_duration <- trade_duration %>% rename(group = group_name) %>%
  select(-X) %>% select(exp_iso3, year, group, imp_iso3, actual_duration)
# average trade duration per exporter (multiple importers)
avg_duration = aggregate(trade_duration$actual_duration, list(trade_duration$exp_iso3, trade_duration$year, trade_duration$group), mean)
setnames(avg_duration, old = c('Group.1','Group.2','Group.3','x'), new = c('exp_iso3','year','group','avg_duration'))


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

# identified wrong matches (manually)
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

rm(match, trade_duration, wrong_match)

###################################################################################################################
##############################################   NETWORK VARIABLES   ##############################################
###################################################################################################################

# Groups
groups <- sort(unique(trade$group))
# Loop through groups
#results <- purrr:::map_df(groups, function(g) {
for(i in 1:length(groups)){
  
  # Subset group in trade data
  gdata <- trade %>% 
    # Reduce to target group
    filter(group==groups[i]) %>% 
    # Reduce to unique links by year
    select(imp_iso3, exp_iso3, year, quantity_mt) %>% 
    distinct() %>% 
    mutate(link_id=paste(imp_iso3, exp_iso3, sep="-"))
  
  # Loop through years
  years <- sort(unique(gdata$year))
  out <- purrr::map_df(years, function(t){
    
    # calculates an edgelist per year
    gdatat <- gdata[gdata$year==t, ]
    edgelist = gdatat %>%select(imp_iso3, exp_iso3, quantity_mt)
    edgelist = na.omit(edgelist)
    g = graph_from_data_frame(edgelist, directed = TRUE)
    
    # local clustering
    local_clustering = transitivity(g, type ="local")
    
    #modularity
    ceb <- cluster_edge_betweenness(g) 
    modu <- modularity(ceb)
    
    #degree and indegree
    nodeList = data.frame(iso3 = igraph::V(g)$name, 
                          ID = c(0:(igraph::vcount(g) - 1))) # make nodelist with ID number
    nodeList = cbind(nodeList, nodeDegree=igraph::degree(g, v = igraph::V(g), mode = "all"), i) # calculate total # of links of each node in network
    nodeList = cbind(nodeList, nodeDegree.in=igraph::degree(g, v = igraph::V(g), mode = "in")) # detect only importing countries to remove later 
    #local clustering to nodelist 
    nodeList = cbind(nodeList, local_clustering) # add clustering 
    
    #filter out countries that are not exporting so that we dont assign a stock status to them 
    nodeList = nodeList %>%
      filter(nodeDegree.in >0)
    #end change
    
    # create df
    df <- data.frame(group=groups[i], year=t, modularity = modu)
    #nodelist observations added
    df = merge(nodeList, df)
    # Merge into dataframe
    return(df)
    
  })
  
  # Return data
  if(i==1){results <- out}else{results <- rbind(results, out)}
  
}

# clean results data
results = results %>%
  select(-i, - ID) 

# remove unused df
rm(gdata, out)

# write and read data created here
# write.csv(results, "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data/interim_results.csv")
# results <- read.csv(file.path(datadir2, "interim_results.csv"), as.is=T) %>% select(-X)


###################################################################################################################
#################################################   MERGE DATA   ##################################################
###################################################################################################################

### add governance
results <- results %>% 
  rename(exp_iso3 = iso3)%>%
  left_join(wgi) #leftjoin wgi 

### add duration data 
results = results %>% left_join(avg_duration) 
results = results %>% select(exp_iso3, group, year, nodeDegree, local_clustering, modularity, avg_duration, composite_governance)

# set missing duration data to 1. (2a_trade_duration.R script selects exporters in year +1 so if an exporter does not occur in year + 1 the trade duration is not recorded.)
results$avg_duration[is.na(results$avg_duration)] = 1

# set missing clustering data to 0, clustering data is NaN when a country has only one trade partner
results$local_clustering[results$local_clustering=="NaN"] = 0
results$local_clustering[is.na(results$local_clustering)] = 0

### add stocks
# calculate mean of super
stocks_edited <- stocks %>%
  group_by(exp_iso3, group, year) %>% 
  summarize(mean(unique(super))) %>% # averaging is necessary. The different super values for the same set of network characteristics, inflates standard deviations.
  mutate(stock_id = paste0(group, exp_iso3, year))
#rename
names(stocks_edited)[4] <- "super"

## left join stocks
results <- results %>% left_join(stocks_edited) %>% distinct()

#check duplicates - no duplicates
results = results[!is.na(results$stock_id),]
results <- results[!duplicated(results[,c('exp_iso3','year','stock_id')]),]

# check which groups are there
check = results %>% select('group') 
check = unique(check) # should be n=28


###################################################################################################################
#################################################    SAVE DATA   ##################################################
###################################################################################################################

# spp to remove 10% is standard
# > 10%  in 2017 global aquaculture production excluding seaweed
remove1 = c('tuna') # vector with groups to be removed
result1 = subset(results,!(group %in% remove1))
# save data 
# write.csv(result1, "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data/RQ2_data.csv")

# > 5%  in 2017 global aquaculture production excluding seaweed
remove2 = c('oysters', 'shrimp','tuna') # vector with groups to be removed
result2 = subset(results,!(group %in% remove2))
# save data 
# write.csv(result2, "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data/RQ2_data_05aqua.csv")

# > 3%  in 2017 global aquaculture production excluding seaweed
remove3 = c('oysters','salmon','salmonidae','shrimp','tuna') # vector with groups to be removed
result3 = subset(results,!(group %in% remove3))
#save data
# write.csv(result3, "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data/RQ2_data_03aqua.csv")


