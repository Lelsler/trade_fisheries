#size of nodes is exports
#color of nodes is stock status
#size of edges is stock status
library(rgeos)
library(tidyverse)
library(rworldmap)
par(mar = c(0.1, 0.1, 0.1, 0.1))#layout <- layout(matrix(c(1,1,2,3), 2, 2, byrow = F)) 

datadir = "~/Documents/RQ2/data" #MO
datadir2 = "~/Dropbox/Laura/" #MO
#stocks_c  = read.csv(file.path(datadir, "1950_2017_FAO_bbmsy_timeseries_merge.csv"), as.is=T)

# read data
trade <- read.csv(file.path(datadir2, "fish_trade.csv"), as.is=T)%>%
  rename(group = group_name, year=t)%>%
  select(-X)
stocks  = read.csv("~/Dropbox/Laura/RQ2_data.csv", as.is=T)%>%
  select(-X)
iso_codes = read.csv(file.path(datadir, "countries.csv"), as.is=T)%>%
  select(alpha3,en)%>%
  rename(iso3=alpha3, country=en)%>%
  mutate(iso3= toupper(iso3))

###################################################################################################################
################################################    CLEAN DATA   ##################################################
###################################################################################################################


##################################################    HAKE   #####################################################

trade=trade%>%
  group_by(exp_iso3, imp_iso3, group, year)%>%
  summarise(quantity_mt = sum(q))%>%
  left_join(stocks)%>%
  filter(!is.na(quantity_mt) & !is.na(super))%>%
  ungroup()

  wmap <- getMap(resolution="high")
  centroids <- gCentroid(wmap, byid=TRUE)
  
  # get centroids
  df <- as.data.frame(centroids)
  df$country = rownames(df)
  
  low_mod1 = trade %>%
    filter(group=="eel" & year==2015)%>%
    select(exp_iso3, imp_iso3, quantity_mt, super)
  
  #not sure yet how/if to use this
  exp_volume = low_mod1%>%
    group_by(exp_iso3)%>%
    summarise(total_exp_quantity = sum(quantity_mt))%>%
    rename(iso3 = exp_iso3)%>%
    rename(exports = total_exp_quantity)
  
  #df= df %>%
  #  left_join(exp_volume)%>%
  #mutate(exports = ifelse(is.na(exports), 0, exports))
  
  country_e = iso_codes%>%#this does not get us allll the countries & iso codes!
    select(iso3, country)%>%
    distinct()%>%
    rename(exp_iso3=iso3, exp_country=country)

  country_i = iso_codes%>%
    select(iso3, country)%>%
    distinct()%>%
    rename(imp_iso3=iso3)
  
  df_exp = df%>%
    rename(exp_country=country, exp_lat=y, exp_lon=x)
  
  
  low_mod1= low_mod1%>%
    left_join(country_e)%>%
    left_join(country_i)%>%
    left_join(df)%>%
    left_join(df_exp)
  
  low_mod2=low_mod1%>%
    select(exp_iso3, super, exp_lon,exp_lat)%>%
    distinct()
    
    
  library(ggplot2)
  library(ggrepel)
  worldmap <- borders("world", colour="light grey", fill="light grey") # create a layer of borders

  p1 <- ggplot() + worldmap + 
    theme_classic() +
    geom_curve(data = low_mod1, aes(x = exp_lon, y = exp_lat, xend = x, yend = y), col = "slateblue1", arrow = arrow(length = unit(2, "pt"),type = "closed"), curvature = 0.2, alpha = 0.3, 
               size = log(low_mod1$quantity_mt + 10) / 2
    ) + 
    geom_point(data = low_mod2, aes(x = exp_lon, y = exp_lat, size = super), col = "purple", alpha = 0.7) +
    scale_size_continuous(range = c(2, 6)) +  # Adjust the range to control point size
    ylab("latitude") + xlab("longitude") +
    ggtitle("eel 2015") +
    labs(size = "B/Bmsy") +
    theme(
      text = element_text(size = 14),  # Adjust the font size 
      plot.title = element_text(size = 18),  # Adjust the plot title font size
      axis.title = element_text(size = 14),  # Adjust the axis labels font size
      legend.title = element_text(size = 14),  # Adjust the legend title font size
      legend.text = element_text(size = 14)  # Adjust the legend text font size
    )
  
  timeseries = stocks%>%
    filter(group=="hake"|group=="eel"|group=="scallop"|group=="coalfish")%>%
    group_by(group, year)%>%
    summarise(B_Bmsy=median(super), modularity=median(modularity))
  
  p11= ggplot(timeseries, aes(x=year, y=B_Bmsy,group=1,linetype ="B/Bmsy"))+geom_line()+
    geom_line(aes(x=year,y=modularity, group=1,linetype="modularity"))+
    facet_wrap(~group, ncol = 4)+theme_classic()+ theme(legend.position = "bottom")+ylab("")+ theme(text = element_text(size = 20))+
    guides(linetype=guide_legend(title=""))+theme(axis.text.x = element_text(angle = 90))
  
  library(patchwork)
  
  p1/p11+
    plot_layout(heights = c(2, 1))
  
  ggsave("eel_trade.png", width = 12, height = 10, dpi=400)
  
  
