library(dplyr)
library(readr)

datafile <- "data/electionresults.csv"

dataframe.electionresults <- read_csv(datafile)

dataframe.electionresults

colnames(dataframe.electionresults) <- c("id",
                                         "numb_dem_votes",
                                         "numb_rep_votes",
                                         "numb_ind_Votes",
                                         "numb_tot_votes",
                                         "seat_make_up",
                                         "contested",
                                         "numb_tot_seates",
                                         "numb_dem_cand",
                                         "numb_rep_cand",
                                         "numb_ind_cand")

dataframe.electionresults$numb_dem_waste <- dataframe.electionresults$numb_dem_votes - (dataframe.electionresults$numb_tot_votes / 2)

dataframe.electionresults$numb_rep_waste <- dataframe.electionresults$numb_rep_votes - (dataframe.electionresults$numb_tot_votes / 2)

dataframe.electionresults$dem_eff_gap <- 100 * (dataframe.electionresults$numb_dem_waste / dataframe.electionresults$numb_tot_votes)

dataframe.electionresults$rep_eff_gap <- 100 * (dataframe.electionresults$numb_rep_waste / dataframe.electionresults$numb_tot_votes)

dataframe.electionresults$numb_dem_surplus <- (dataframe.electionresults$numb_dem_votes - dataframe.electionresults$numb_rep_votes)

dataframe.electionresults$numb_rep_surplus <- (dataframe.electionresults$numb_rep_votes - dataframe.electionresults$numb_dem_votes)


library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggmap)
library(readr)
library(dplyr)
library(scales)
library(RColorBrewer)

############################################
########## Preparing the map data ##########
############################################
# Reads in NH .shp file with rgdal's readOGR

nh <- suppressWarnings(
  readOGR("data/cb_2014_33_sldl_500k",
          "cb_2014_33_sldl_500k", stringsAsFactors=FALSE,
          verbose=FALSE))

#Orders nh spdf alphabetically by NAME
nh <- nh[order(nh@data$NAME),]
#Gets just Strafford and Rockingham counties
nh <- nh[107:153,]



#Verify it's mostly just Seacoast region
plot(nh)


#Prepares nh spdf for ggplot mapping
nh_map <- fortify(nh, region = "NAME")

#Fixes id column to match data
nh_map$id <- gsub("County ", "", nh_map$id)

############################################
######### Preparing attribute data #########
############################################
# Read in county republican/democrat counts

countydata <- dataframe.electionresults

#Create a new spdf that incorporates data from countydata
nh_map_with_data <- merge(nh_map, countydata, by = "id", all.x = TRUE)

#Orders new data because that matters I think
final_plot <- nh_map_with_data[order(nh_map_with_data$order),]

###########################################
######### Generating the maps #############
#########################################

gg <- ggplot() + 
  geom_polygon(data = final_plot,
               aes(x = long, y = lat, group = group, fill = numb_dem_surplus),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_distiller(name = "Gap", palette = "RdYlBu", breaks = pretty_breaks(n = 5)) +
  theme_nothing(legend = TRUE) +
  labs(title = "Efficiency Gap Seacoast") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(face = "bold", hjust= 0.5))
gg


##########################################
######## Some questions ##################
##########################################

# What were the total number of wasted votes
sum(countydata$numb_rep_waste[sign(countydata$numb_rep_waste) >= 1]) # Total Republican waste votes
sum(countydata$numb_dem_waste[sign(countydata$numb_dem_waste) >= 1]) # Total Democrat waste votes

# Number of districts won by each party
length(countydata$id[sign(countydata$numb_rep_waste) >= 1]) # Number of seats won by republicans
length(countydata$id[sign(countydata$numb_dem_waste) >= 1]) # Number of seats won by democrats


##########################################
######## Sample barplot #################
#########################################

set.seed(1492)

things <-  sample(-30:30, 70, replace=TRUE)

things <-  sort(things)

things_cols <- ifelse(things < 0, "blue", "red")

barplot(things, col=things_cols)


countydata$party <- ifelse(countydata$numb_rep_waste > 0, "Republican", "Democrat")



barplot(countydata$numb_rep_waste)

countydata_cols <- ifelse(countydata$numb_rep_waste < 0, "blue", "red")



barplot(countydata$numb_rep_waste, col=countydata_cols)


gg <- ggplot(data = countydata, mapping = aes(x=id, y=numb_rep_waste, fill = party))
gg <- gg + geom_bar(stat="identity")
gg <- gg + theme_minimal()
gg <- gg + scale_fill_brewer(palette="Dark2")
gg <- gg + coord_flip()
gg <- gg + scale_y_continuous(limits = c(-7500,7500))
gg


# This gets all positive numbers  in numb rep waste that were contested
countydata %>% 
  filter(sign(numb_rep_waste) >= 0, contested == TRUE) -> test11

sum(test11$numb_rep_waste)

countydata %>% 
  filter(sign(numb_dem_waste) >= 0, contested == TRUE) -> test12

sum(test12$numb_dem_waste)
