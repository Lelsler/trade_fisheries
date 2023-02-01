# Creates the timeseries plot 
# LGE, 10-Jul-22

# clear workspace
rm(list = ls())
graphics.off()

# libraries 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(patchwork)
#theme_set(theme_minimal())

# load data
#setwd("~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push")
paneldata <- read.csv("~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data/RQ2_data.csv", as.is=T)
paneldata$X <- NULL
names(paneldata)[10]<-"super" # change column name

# calculate average, sdv, lower and upper limit across all species groups 
x <- aggregate(cbind(nodeDegree, local_clustering, modularity, avg_duration, composite_governance, super) ~ year, data = paneldata, mean, na.rm = TRUE)
xs <- aggregate(cbind(nodeDegree, local_clustering, modularity, avg_duration, composite_governance, super) ~ year, data = paneldata, sd, na.rm = TRUE)
xl <- x-((1.96 * xs)/length(x$nodeDegree)) # lower limit
xu <- x+(1.96 * xs/length(x$nodeDegree)) # upper limit

# new columns for group 
x$group <- 'all'
xl$group <- 'all'
xu$group <- 'all'

###################################################################################################################
################################################    PREP DATA    ##################################################
###################################################################################################################

# calculate average, sdv, lower and upper limit PER species groups 
y <- aggregate(cbind(nodeDegree, local_clustering, modularity, avg_duration, composite_governance, super) ~ year+group, data = paneldata, mean, na.rm = TRUE) # mean

# merge 
z <- rbind(x,y)

# include only average all groups
a <- subset(z, group == 'all' | group == 'crab' | group == 'oysters' | group == 'shark' | group == 'shrimp' | group == 'haddock' | group == 'rocklobster' | group == 'seabass' | group == 'plaice' | group == 'lobster')

# change names for later leftjoin
colnames(a) <- paste("avg", colnames(a), sep = "_")
colnames(xl) <- paste("low", colnames(xl), sep = "_")
colnames(xu) <- paste("up", colnames(xu), sep = "_")

# in xl and xu replace by year column from x
xl$low_year <- xs$year 
xu$up_year <- xs$year

# rename year and group
xl <- xl %>%  dplyr::rename(year=low_year, group=low_group)
xu <- xu %>%  dplyr::rename(year=up_year, group=up_group)
a <- a %>%  dplyr::rename(year=avg_year, group=avg_group)

# leftjoin xl and xu to a by year and group
full_x <- a %>% 
  left_join(xl, by = c("year","group"))  %>% 
  left_join(xu, by = c("year","group")) 

# move position of columns
full_x <- full_x %>%
  select(year, group, everything())


#### line plots 
# color palette
col_order <- c("black", "#95A900", "#D89000", "#F763E0","#00B81F", "#F8766D","#C59900","#00C1AA","#00A5FF","#9590FF") # according to the selected groups


###################################################################################################################
################################################    PLOT DATA    ##################################################
###################################################################################################################
# plot only groups in gr
gr = full_x%>%
  filter(group =="all")

# plot bbmsy
k1 <- ggplot(gr, aes(x=year, y=avg_super, color=group, group=group)) + 
  geom_line(size=1)+theme_classic()+ theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) + labs(y=expression('B/B'['MSY']))+  theme(legend.position="none")+
  scale_color_manual(values = col_order) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=14), axis.title=element_text(size=20)) +
  geom_ribbon(full_x, 
              mapping = aes(x = year, ymin=low_super, ymax=up_super), fill="grey", alpha=0.3, show.legend = FALSE) 
k1

# plot modularity
k2 <- ggplot(gr, aes(x=year, y=avg_modularity, color=group, group=group)) + 
  geom_line(size=1)+theme_classic()+ theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) + labs(y=expression('Modularity')) +    theme(legend.position="none")+
  scale_color_manual(values = col_order) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=14), axis.title=element_text(size=20)) +
  geom_ribbon(full_x, 
              mapping = aes(x = year, ymin=low_modularity, ymax=up_modularity), fill="grey", alpha=0.3, show.legend = FALSE) 
k2

# plot degree
k3 <- ggplot(gr, aes(x=year, y=avg_nodeDegree, color=group, group=group)) + theme_classic()+  theme(legend.position="none")+ 
  geom_line(size=1)+theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) + labs(y=expression('Degree')) +  #ylim(0,NA) +
  scale_color_manual(values = col_order) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=14), axis.title=element_text(size=20)) +
  geom_ribbon(full_x, 
              mapping = aes(x = year, ymin=low_nodeDegree, ymax=up_nodeDegree), fill="grey", alpha=0.3, show.legend = FALSE) 
k3

# plot clustering
k4 <- ggplot(gr, aes(x=year, y=avg_local_clustering, color=group, group=group)) + 
  geom_line(size=1)+ theme_classic()+ theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) + labs(y=expression('Clustering')) +    theme(legend.position="none")+
  scale_color_manual(values = col_order) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=14), axis.title=element_text(size=20)) +
  geom_ribbon(full_x, 
              mapping = aes(x = year, ymin=low_local_clustering, ymax=up_local_clustering), fill="grey", alpha=0.3, show.legend = FALSE) 
k4

# plot duration
k5 <- ggplot(gr, aes(x=year, y=avg_avg_duration, color=group, group=group)) + 
  geom_line(size=1) + theme_classic() + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) + labs(y=expression("Trade duration")) +  theme(legend.position="none")+
  scale_color_manual(values = col_order) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=14), axis.title=element_text(size=20)) +
  geom_ribbon(full_x, 
              mapping = aes(x = year, ymin=low_avg_duration, ymax=up_avg_duration), fill="grey", alpha=0.3,  show.legend = FALSE)
k5

# plot governance
k6 <- ggplot(gr, aes(x=year, y=avg_composite_governance, color=group, group=group)) + theme_classic()+  theme(legend.position="none")+
  geom_line(size=1) + labs(y= "Governance", x='Year') + #ylim(0,NA) +
  scale_color_manual(values = col_order) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=14), axis.title=element_text(size=20)) +
  geom_ribbon(full_x, 
              mapping = aes(x = year, ymin=low_composite_governance, ymax=up_composite_governance), fill="grey", alpha=0.3,  show.legend = FALSE) +
  expand_limits(x = 1995)
k6


#### merge plots 
combined = (k1/
    k2/
    k3/
    k4/
    k5/      
      k6) & theme(legend.position = 'none')
combined + plot_layout(guides = "collect")

#### save plot
# ggsave(filename="~/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/figures/rq2_timeseries.png", width = 210, height = 297, units = "mm", dpi=500)

