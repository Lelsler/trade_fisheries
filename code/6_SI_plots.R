# this file creates the timeseries plots of the selected species groups  
# LGE, MO, JG; 10-Jul-22
# Notes: Modification of networks_1023.R 

# clear workspace
rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)
library(igraph)
library(countrycode)
library(circlize)
library(RColorBrewer)
library(sf)
library(ggplot2)

# Directories
datadir = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data" #LGE

# Read data
trade <- read.csv(file.path(datadir, "fish_trade.csv"), as.is=T, stringsAsFactors = FALSE)
stocks  = read.csv(file.path(datadir, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T, stringsAsFactors = FALSE) %>% rename(exp_iso3=iso3)
match = read.csv(file.path(datadir,"matchingstocksHS92.csv"), as.is=T, stringsAsFactors = FALSE) 
duration <- read.csv(file.path(datadir, "actual_trade_duration.csv"), as.is=T, stringsAsFactors = FALSE)
result <- read.csv(file.path(datadir, "RQ2_data.csv"), as.is=T, stringsAsFactors = FALSE)

###################################################################################################################
#################################################   CLEAN DATA   ##################################################
###################################################################################################################

# trade data 
filematch <- match %>%
  select(Shortdescription.HS1992:sci_name) %>%
  filter(!is.na(comm_name))

trade <- trade %>% 
  left_join(match, by = "Shortdescription.HS1992") %>% 
  rename(year=t)

duration <- duration %>%
  select(c('year','group_name','exp_iso3','imp_iso3','actual_duration')) 

# vector with species groups to be removed
# > 10%  in 2017 global aquaculture production excluding seaweed
remove1 = c('tuna') # vector with groups to be removed
# > 5%  in 2017 global aquaculture production excluding seaweed
remove2 = c('oysters', 'shrimp','tuna') # vector with groups to be removed
# > 3%  in 2017 global aquaculture production excluding seaweed
remove3 = c('oysters','salmon','salmonidae','shrimp','tuna') # vector with groups to be removed
stocks  <- stocks %>% 
  left_join(trade, by = c("exp_iso3", "sci_name", "comm_name", "year")) %>% 
  left_join(duration, by = c("exp_iso3", "imp_iso3", "group_name", "year"))%>%
  filter(!is.na(group_name)) %>%
  filter(!(group_name %in% remove1)) # remove spp groups default 10%

###################################################################################################################
##############################################   ALL SPP GROUP   ##################################################
###################################################################################################################
# Create an "all" species group
all <- stocks %>%
  select(exp_iso3, imp_iso3, group_name, year, v, actual_duration, super) %>% 
  distinct() %>% 
  group_by(exp_iso3, imp_iso3, year) %>% 
  summarise(sum_trade = sum(v), max_duration = max(actual_duration), super = mean(super)) # Laura: Does it matter to take the mean of super here?
all$group_name <- "all"

# Calculate species group trade flow totals
stocks <- stocks  %>% 
  select(exp_iso3, imp_iso3, group_name, year, v, actual_duration, super) %>% 
  distinct() %>% 
  group_by(exp_iso3, imp_iso3, year, group_name) %>% 
  summarise(sum_trade = sum(v), max_duration = max(actual_duration), super = mean(super))

# Combine stocks and all dataframes
stocks <- rbind(stocks, all)

# Calculate unweighted average stock status per country per year
stock_status <- stocks %>% 
  group_by(year, exp_iso3, group_name) %>% 
  summarise(mean_super = mean(super)) 

# merge stocks trade and status
network <- stocks %>% 
  left_join(stock_status, by = c("exp_iso3", "group_name", "year")) 

###################################################################################################################
#################################################   PLOT DATA   ###################################################
###################################################################################################################
###  trade volume & duration
r1 <- ggplot(network, aes(x=(sum_trade/1000), y=max_duration)) + 
  geom_point() + theme_classic() +
  labs(y='Maximum trade duration', x='Traded volume') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
r1
# ggsave(filename="~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/figures/rq2_volume_duration.png", width = 210, height = 297, units = "mm", dpi=500)

###  mobility plots
mlow = c('crab','homarus','lobster','mussel','oyster','rocklobster','scallop') 
mmed = c('coalfish','cuttlefish','flatfish','haddock','hake','halibut','octopus','plaice','seabass','sole') 
mhigh = c('cod','eel','shark')  

network <- na.omit(network)   
temp <- aggregate(network[, 6:8], list(network$group_name), mean)

mobl = subset(temp,(Group.1 %in% mlow)) # include groups
mobm = subset(temp,(Group.1 %in% mmed)) # include groups
mobh = subset(temp,(Group.1 %in% mhigh)) # include groups

r2 <- ggplot(mobl, aes(x = max_duration, y = mean_super, color=Group.1, group=Group.1)) + geom_point()
r3 <- ggplot(mobm, aes(x = max_duration, y = mean_super, color=Group.1, group=Group.1)) + geom_point()
r4 <- ggplot(mobh, aes(x = max_duration, y = mean_super, color=Group.1, group=Group.1)) + geom_point()
r2
r3
r4
### panels per species group 
# degree - bbmsy
a <- ggplot(result, aes(x=nodeDegree, y=super, color=group)) + geom_point()
b <- a + facet_wrap(~group)

# clustering - bbmsy
c <- ggplot(data = result, aes(x = local_clustering, y = super, color=group)) + geom_point()
d <- c + facet_wrap(~group)

# trade duration - bbmsy
e <- ggplot(data = result, aes(x = avg_duration, y = super, color=group)) + geom_point()
f <- e + facet_wrap(~group)

# clustering - degree
g <- ggplot(data = result, aes(x = local_clustering, y = nodeDegree, color=group)) + geom_point()
h <- g + facet_wrap(~group)

# modularity - degree
g <- ggplot(data = result, aes(x = modularity, y = nodeDegree, color=group)) + geom_point()
h <- g + facet_wrap(~group)

###  scatter: degree & modularity
r5 <- ggplot(result, aes(x=modularity, y=nodeDegree)) + 
  geom_point() + theme_classic() +
  labs(y='Degree ', x='Modularity') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
r5
# ggsave(filename="~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/figures/rq2_modularity_degree.png", width = 210, height = 297, units = "mm", dpi=500)
# correlation between modularity and node degree
summary(lm(result$nodeDegree~result$modularity))
cor(result$nodeDegree,result$modularity) # pearson correlation coefficient

###################################################################################################################
##########################################   ADDITIONAL PLOTS   ###################################################
###################################################################################################################
###  trade volume & duration 
# plot modularity per spp group and year
r5 <- ggplot(result, aes(x = year, y = modularity, color=group, group=group)) + geom_line()
# plot modularity and clustering
r6 <- ggplot(result, aes(x = local_clustering, y = modularity, color=group, group=group)) + geom_point()
# timeseries degree scallop 
plot_scallop <- result %>% filter(group=='scallop') # remove only rows with entry 'row_name'
plot_scallop <- aggregate( nodeDegree ~ year, plot_scallop, mean ) # calculates speed per dive
r7 <- ggplot(plot_scallop, aes(x = year, y = nodeDegree)) + geom_point()
r7
# timeseries modularity scallop 
plot_scallop <- result %>% filter(group=='scallop') # remove only rows with entry 'row_name'
plot_scallop <- aggregate( modularity ~ year, plot_scallop, mean ) # calculates speed per dive
r8 <- ggplot(plot_scallop, aes(x = year, y = modularity)) + geom_point()
r8

