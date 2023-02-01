# Elaine Bochniewicz
# first trade duration script, looks only "backwards" as to how long a trade is taking place.

# Clear workspace
rm(list = ls())
graphics.off()

# libraries
require(igraph)
require(dplyr)
require(reshape2)


setwd("/Users/lauraelsler/Documents/SESYNC/Files/FISHMAR-data/rq2/final_push/data") #LGE
data = read.csv("fish_trade.csv")

###################################################################################################################
###############################################   CREATE DATAFRAMES   #############################################
###################################################################################################################

commodities = unique(data$group_name)
data = data %>% group_by(t, exp_iso3, imp_iso3, group_name) %>% summarise(total.value = sum(v), total.quantity= sum(q)) %>% distinct()

#what data we need
newlinks.exports <- data.frame(t = integer(),
                               exp_iso3=character(), 
                               imp_iso3=character(),
                               total.value = numeric(),
                               total.quantity= numeric(),
                               group_name = character(),
                               new.export = character(),
                               duration = numeric(),
                               stringsAsFactors=FALSE) 

colnames(newlinks.exports) <- c("t", "exp_iso3","imp_iso3", "total.value", "total.quantity","group_name","new.export","duration") # Because the column names didn't work right.


#what data we need
newlinks.imports <- data.frame(t = integer(),
                               exp_iso3=character(), 
                               imp_iso3=character(),
                               total.value = numeric(),
                               total.quantity= numeric(),
                               group_name = character(),
                               new.import = character(),
                               duration = numeric(),
                               stringsAsFactors=FALSE) 


colnames(newlinks.imports) <- c("t", "iso3","imp_iso3", "total.value", "total.quantity","group_name", "new.import","duration") # Because the column names didn't work right.

###################################################################################################################
################################################   CALCULATE TRADE CONTINUITY   ###################################
###################################################################################################################

# LOOPS OVER EXP-IMP PAIRS THAT TRADE ONE SPECIES GROUP AT T AND IF THE SAME PAIR IS PRESENT IN T-1 CLASSIFIES AS OLD OR NEW CONNECTION. 

#fish species code
for (z in commodities){
  data2= subset(data, group_name == z)
  
  #I'm sure this could be done in a neater way but code below takes the iso codes 
  
  years <- unique(data2$t) # pull the unique set of years from the input file
  years <- years[-length(years)] # delete the last year
  
  # x <- length(years)
  # x <- x-1 # this said x- 2 but think it should be minus 1.. so changed it 
  # length(years) <- x # all years but the last year
  
  for (k in years) { 
    
    table1 <- data2[data2$t == k, ]
    #edgeList1 <- table1 %>% select(iso3, imp.iso3)
    
    nextYear <- k + 1
    # create appropriate second table, and first edgelist
    table2 <- data2[data2$t == nextYear, ]
    
    exp = unique(table2$exp_iso3)
    imp = as.factor(unique(table2$imp_iso3))
    imp = imp[!is.na(imp)]
    
    ###### New Links exports
    for (e in exp) {
      
      c2 <- table2[table2$exp_iso3 == e, ]
      c1 <- table1[table1$exp_iso3 == e, ]
      
      # Add a counter for the first year of each connection
      if (k == years[1] & nrow(c1) > 0){
        c1$duration = 1
        c1$new.export = "new_connection"
        newlinks.exports = bind_rows(newlinks.exports, c1)
      } else if(k > years[1]){
        c1 = newlinks.exports[newlinks.exports$exp_iso3 == e & newlinks.exports$t == k & newlinks.exports$group_name == z,]
      }
      
      comp <- c2$imp_iso3 %in% c1$imp_iso3 # New Exports
      
      
      c2$new.export = ifelse((c2$imp_iso3 %in% c1$imp_iso3), "old_connection", "new_connection")
      c2$duration = 1
      
      # Add 1 to the counter for old_connections in the most innefficient possible way
      id_old = unlist(sapply(c1$imp_iso3, function(x) which(x==c2$imp_iso3))) # Countries with old conn
      
      for (im in id_old){
        c2$duration[im] = c1$duration[which(c2$imp_iso3[im] == c1$imp_iso3)] + 1
      }
      
      newlinks.exports = bind_rows(newlinks.exports, c2)
    }
    
    ###### New links imports
    for (i in imp) {
      c2 <- table2[which(table2$imp_iso3 == i), ]
      c1 <- table1[which(table1$imp_iso3 == i), ]
      
      # Add a counter for the first year of each connection
      if (k == years[1] & nrow(c1) > 0){
        c1$duration = 1
        c1$new.import = "new_connection"
        newlinks.imports = bind_rows(newlinks.imports, c1)
      }  else if(k > years[1]){
        c1 = newlinks.imports[newlinks.imports$imp_iso3 == i & newlinks.imports$t == k & newlinks.imports$group_name == z,]
      }
      
      comp <- c2$exp_iso3 %in% c1$exp_iso3 # New Imports
      
      c2$new.import = ifelse((c2$exp_iso3 %in% c1$exp_iso3), "old_connection", "new_connection")
      c2$duration = 1
      
      # Add 1 to the counter for old_connections in the most innefficient possible way
      id_old = unlist(sapply(c1$exp_iso3, function(x) which(x==c2$exp_iso3))) # Countries with old conn
      
      for (im in id_old){
        c2$duration[im] = c1$duration[which(c2$exp_iso3[im] == c1$exp_iso3)] + 1
      }
      
      newlinks.imports = bind_rows(newlinks.imports, c2)
    }
  }
}

###################################################################################################################
#########################################################   SAVE DATA   ###########################################
###################################################################################################################

# write.csv(newlinks.exports,'~/Dropbox/Laura/exports_duration.csv')
