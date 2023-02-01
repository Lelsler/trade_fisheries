# this file creates the network plots of selected species groups
# Maartje Oostdijk, Laura Gabriele Elsler, WMU, SRC
# 13 July 2022

# clear workspace
rm(list = ls())
graphics.off()
#libraries
require(tidyverse)
library(tidygraph)
library(ggraph)
library(ggpubr)

# directory
datadir = "~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data" #LGE

# read data
trade <- read.csv(file.path(datadir, "fish_trade.csv"), as.is=T)
stocks  = read.csv(file.path(datadir1, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)

###################################################################################################################
################################################    CLEAN DATA   ##################################################
###################################################################################################################

# modify stocks for plotting
stocks_edited <- stocks %>%
  group_by(exp_iso3, group, year) %>% 
  summarize(mean(unique(super))) %>% # different super values for the same set of network characteristics, inflates standard deviations
  mutate(stock_id = paste0(group, exp_iso3, year))

##################################################    HAKE   #####################################################

trade=trade%>%
  left_join(stocks_edited)

low_mod1 = trade %>%
  filter(group=="hake" & year==2005)%>%
  filter(!is.na(super))%>%
  filter(quantity_mt>10) # filter out "lower" volume trades

low_mode1_edge= low_mod1%>%
  select(exp_iso3, imp_iso3, quantity_mt)#for edges

g = graph_from_data_frame(low_mode1_edge, directed = TRUE)#construct graph from edges
E(g)$weight <- low_mode1_edge$quantity_mt#edgeweight by traded quantity
hake = low_mod1 %>% #super for countries with stocks
  select(exp_iso3, super)%>% 
  distinct()%>%
  rename(iso3 = exp_iso3)

exp_volume = low_mod1%>%
  group_by(exp_iso3)%>%
  summarise(total_exp_quantity = sum(quantity_mt))%>%
  rename(iso3 = exp_iso3)


nodeList = data.frame(iso3 = igraph::V(g)$name, 
                      ID = c(0:(igraph::vcount(g) - 1))) # make nodelist with ID number

nodeList = nodeList%>%
  left_join(hake)%>%#merge nodeList with status of stocks
  left_join(exp_volume)%>%
  mutate(country_name = ifelse(is.na(super), "", iso3))%>%
  mutate(total_exp_quantity = ifelse(is.na(total_exp_quantity), 0, total_exp_quantity))#size by export volume

layout <- create_layout(g, layout = 'igraph', algorithm = 'nicely')

###################################################    EEL   ######################################################
high_mod1 = trade %>%
  filter(group=="eel" & year==2015)%>%
  filter(!is.na(super))%>%
  filter(quantity_mt>10)#filter out "lower" volume trades

high_mod1_edge= high_mod1%>%
  select(exp_iso3, imp_iso3, quantity_mt)#for edges


g2 = graph_from_data_frame(high_mod1_edge, directed = TRUE)#construct graph from edges
E(g2)$weight <- high_mod1_edge$quantity_mt#edgeweight by traded quantity
coalfish = high_mod1 %>% #super for countries with stocks
  select(exp_iso3, super)%>% 
  distinct()%>%
  rename(iso3 = exp_iso3)

exp_volume = high_mod1%>%
  group_by(exp_iso3)%>%
  summarise(total_exp_quantity = sum(quantity_mt))%>%
  rename(iso3 = exp_iso3)


nodeList2 = data.frame(iso3 = igraph::V(g2)$name, 
                       ID = c(0:(igraph::vcount(g2) - 1))) # make nodelist with ID number

nodeList2 = nodeList2%>%
  left_join(coalfish)%>%#merge nodeList with status of stocks
  left_join(exp_volume)%>%
  mutate(country_name = ifelse(is.na(super), "", iso3))%>%
  mutate(total_exp_quantity = ifelse(is.na(total_exp_quantity), 0, total_exp_quantity))#size by export volume



layout2 <- create_layout(g2, layout = 'igraph', algorithm = 'nicely')

################################################    COALFISH   ######################################################
high_mod2 = trade %>%
  filter(group=="coalfish" & year==1998)%>%
  filter(!is.na(super))%>%
  filter(quantity_mt>10)#filter out "lower" volume trades

high_mod2_edge= high_mod2%>%
  select(exp_iso3, imp_iso3, quantity_mt)#for edges


g3 = graph_from_data_frame(high_mod2_edge, directed = TRUE)#construct graph from edges
E(g3)$weight <- high_mod2_edge$quantity_mt#edgeweight by traded quantity
coalfish = high_mod2 %>% #super for countries with stocks
  select(exp_iso3, super)%>% 
  distinct()%>%
  rename(iso3 = exp_iso3)

exp_volume = high_mod2%>%
  group_by(exp_iso3)%>%
  summarise(total_exp_quantity = sum(quantity_mt))%>%
  rename(iso3 = exp_iso3)


nodeList3 = data.frame(iso3 = igraph::V(g3)$name, 
                       ID = c(0:(igraph::vcount(g3) - 1))) # make nodelist with ID number

nodeList3 = nodeList3%>%
  left_join(coalfish)%>%#merge nodeList with status of stocks
  left_join(exp_volume)%>%
  mutate(country_name = ifelse(is.na(super), "", iso3))%>%
  mutate(total_exp_quantity = ifelse(is.na(total_exp_quantity), 0, total_exp_quantity))#size by export volume



layout3 <- create_layout(g3, layout = 'igraph', algorithm = 'nicely')

#################################################    SCALLOP   ######################################################
low_mod2 = trade %>%
  filter(group=="scallop" & year==2003)%>%
  filter(!is.na(super))%>%
  filter(quantity_mt>10)#filter out "lower" volume trades

low_mod2_edge= low_mod2%>%
  select(exp_iso3, imp_iso3, quantity_mt)#for edges


g4 = graph_from_data_frame(low_mod2_edge, directed = TRUE)#construct graph from edges
E(g4)$weight <- low_mod2_edge$quantity_mt#edgeweight by traded quantity
scallop = low_mod2 %>% #super for countries with stocks
  select(exp_iso3, super)%>% 
  distinct()%>%
  rename(iso3 = exp_iso3)

exp_volume = low_mod2%>%
  group_by(exp_iso3)%>%
  summarise(total_exp_quantity = sum(quantity_mt))%>%
  rename(iso3 = exp_iso3)


nodeList4 = data.frame(iso3 = igraph::V(g4)$name, 
                       ID = c(0:(igraph::vcount(g4) - 1))) # make nodelist with ID number

nodeList4 = nodeList4%>%
  left_join(scallop)%>%#merge nodeList with status of stocks
  left_join(exp_volume)%>%
  mutate(country_name = ifelse(is.na(super), "", iso3))%>%
  mutate(total_exp_quantity = ifelse(is.na(total_exp_quantity), 0, total_exp_quantity))#size by export volume


#layout
layout4 <- create_layout(g4, layout = 'igraph', algorithm = 'nicely')#size by export volume

###################################################################################################################
################################################    PLOT DATA   ###################################################
###################################################################################################################

##################################################    HAKE   ######################################################
p1 = ggraph(layout, vertices = nodeList) +
  geom_edge_link(alpha = 0.5, colour ="grey")+
  geom_node_point(aes(fill = nodeList$super, size = (nodeList$total_exp_quantity+1)), shape = 21, stroke = 1) +
  geom_node_text(aes(label = nodeList$country_name), repel=TRUE) +                   # "name" is automatically generated from the node IDs in the edges
  theme_void()+
  scale_fill_gradient(high="dark blue",low="white")+
  labs(size="Total export volume",fill="Stock size estimate")+
  ggtitle("Hake 2005")

###################################################    EEL   ######################################################
p2 = ggraph(layout2, vertices = nodeList2) +
  geom_edge_link(alpha = 0.5, colour ="grey")+
  geom_node_point(aes(fill = nodeList2$super, size = (nodeList2$total_exp_quantity+1)), shape = 21, stroke = 1) +
  geom_node_text(aes(label = nodeList2$country_name), repel=TRUE) +                   # "name" is automatically generated from the node IDs in the edges
  theme_void()+
  scale_fill_gradient(high="dark blue",low="white")+
  labs(size="Total export volume",fill="Stock size estimate")+
  ggtitle("Eel 2015")

################################################    COALFISH   ######################################################
p3 = ggraph(layout3, vertices = nodeList) +
  geom_edge_link(alpha = 0.5, colour ="grey")+
  geom_node_point(aes(fill = nodeList3$super, size = (nodeList3$total_exp_quantity+1)), shape = 21, stroke = 1) +
  geom_node_text(aes(label = nodeList3$country_name), repel=TRUE) +                   # "name" is automatically generated from the node IDs in the edges
  theme_void()+
  scale_fill_gradient(high="dark blue",low="white")+
  labs(size="Total export volume",fill="Stock size estimate")+
  ggtitle("Coalfish 1998")

#################################################    SCALLOP   ######################################################
p4 = ggraph(layout4, vertices = nodeList4) +
  geom_edge_link(alpha = 0.5, colour ="grey")+
  geom_node_point(aes(fill = nodeList4$super, size = (nodeList4$total_exp_quantity+1)), shape = 21, stroke = 1) +
  geom_node_text(aes(label = nodeList4$country_name), repel=TRUE) +                   # "name" is automatically generated from the node IDs in the edges
  theme_void()+
  scale_fill_gradient(high="dark blue",low="white")+
  labs(size="Total export volume",fill="Stock size estimate")+
  ggtitle("Scallop 2003")

#################################################    JOIN PLOT   #####################################################
ggarrange(p1, p2,p4,p3,  common.legend = TRUE, legend="right")
#save plots
# ggsave("~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/figures/networks.png")


