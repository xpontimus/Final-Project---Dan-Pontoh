library(dplyr)
library(readr)

###########################
###### Dataforming ########
###########################

#Data comes from nh.electionstats.com http://nh.electionstats.com/elections/view/48797/ - /48858
#Reads data from the csv file
datafile <- "data/electionresults.csv"

#Put csv file into a dataframe
dataframe.electionresults <- read_csv(datafile)

#Make better column names
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

#######
#To get the number of waste votes for each district
#Need to see how many votes over the number of votes needed to win the district 
# Party Waste Votes= (total number of votes for a Party) - (Total number of votes / 2)

#Make column for wasted votes for democratic party
dataframe.electionresults$numb_dem_waste <- dataframe.electionresults$numb_dem_votes - (dataframe.electionresults$numb_tot_votes / 2)

#Make column for wasted votes for republican party
dataframe.electionresults$numb_rep_waste <- dataframe.electionresults$numb_rep_votes - (dataframe.electionresults$numb_tot_votes / 2)


#######
#The Efficiency gap is percentage wasted votes make up out of the total number of votes

dataframe.electionresults <- mutate(dataframe.electionresults, numb_dem_waste1 = ifelse(numb_dem_waste < 0, 0, numb_dem_waste))
dataframe.electionresults <- mutate(dataframe.electionresults, numb_rep_waste1 = ifelse(numb_rep_waste < 0, 0, numb_rep_waste))

#Efficiency gap for democratic party
dataframe.electionresults$dem_eff_gap <- 100 * (dataframe.electionresults$numb_dem_waste / dataframe.electionresults$numb_tot_votes)

#Efficiency gap for republican party
dataframe.electionresults$rep_eff_gap <- 100 * (dataframe.electionresults$numb_rep_waste / dataframe.electionresults$numb_tot_votes)


#######
#Surplus votes = (number of party 1 votes) - (number of party 2 votes)

#Surplus votes for democratic party
dataframe.electionresults$numb_dem_surplus <- (dataframe.electionresults$numb_dem_votes - dataframe.electionresults$numb_rep_votes)

#Surplus votes for republican party
dataframe.electionresults$numb_rep_surplus <- (dataframe.electionresults$numb_rep_votes - dataframe.electionresults$numb_dem_votes)


write_csv(dataframe.electionresults, path = "data/nhelectiondata.csv")


##########################################
######## Some questions ##################
##########################################

# What were the total number of wasted votes for each party

#For Republicans
sum(dataframe.electionresults$numb_rep_waste[sign(dataframe.electionresults$numb_rep_waste) >= 1]) # Total Republican waste votes

#For Democrats
sum(dataframe.electionresults$numb_dem_waste[sign(dataframe.electionresults$numb_dem_waste) >= 1]) # Total Democrat waste votes


# Number of districts won by each party

#For Republicans
length(dataframe.electionresults$id[sign(dataframe.electionresults$numb_rep_waste) >= 1]) # Number of seats won by republicans

#For Democrats
length(dataframe.electionresults$id[sign(dataframe.electionresults$numb_dem_waste) >= 1]) # Number of seats won by democrats


#Takes only contested data in puts it in new dataframe
allcontestedracedata <- dataframe.electionresults[dataframe.electionresults$contested == TRUE,]







################
#####  1  ######
################

#Plot all contests wasted votes (blue is democratic wasted votes, red is republican waste votes)
contesteddata_cols <- ifelse(allcontestedracedata$numb_rep_waste < 0, "blue", "red")
barplot(allcontestedracedata$numb_rep_waste, col=contesteddata_cols)


###############
#####  2  #####
###############

#Plot only contested waste votes 
countydata_cols <- ifelse(dataframe.electionresults$numb_rep_waste < 0, "blue", "red")
barplot(dataframe.electionresults$numb_rep_waste[dataframe.electionresults$contested == TRUE], col=countydata_cols)

###############
#####  3  #####
###############

#Get the total wastes per party for only contested and all contests

#Waste votes for all contests for republican party
sum(dataframe.electionresults$numb_rep_waste[sign(dataframe.electionresults$numb_rep_waste) >=1])

#Waste votes for all contests for democrat party
sum(dataframe.electionresults$numb_dem_waste[sign(dataframe.electionresults$numb_dem_waste) >=1])




###############
#####  4  #####
###############

#Waste votes for only contested for republican party
sum(allcontestedracedata$numb_rep_waste[sign(allcontestedracedata$numb_rep_waste) >= 1])

#Waste votes for only contested for democratic party
sum(allcontestedracedata$numb_dem_waste[sign(allcontestedracedata$numb_dem_waste) >= 1])





###############
#####  5  #####
###############

# Difference in wasted votes from all contested to only contested

#Difference for republicans
(sum(dataframe.electionresults$numb_rep_waste[sign(dataframe.electionresults$numb_rep_waste) >=1])) - sum(allcontestedracedata$numb_rep_waste[sign(allcontestedracedata$numb_rep_waste) >= 1])

#Difference for democrats
(sum(dataframe.electionresults$numb_dem_waste[sign(dataframe.electionresults$numb_dem_waste) >=1])) - sum(allcontestedracedata$numb_dem_waste[sign(allcontestedracedata$numb_dem_waste) >= 1])


###############
#####  6  #####
###############

# Number of districts won by each party for all contests and only contested

#All contests for republicans
length(dataframe.electionresults$id[sign(dataframe.electionresults$numb_rep_waste) >= 1])

#All contestes for Democrats
length(dataframe.electionresults$id[sign(dataframe.electionresults$numb_dem_waste) >= 1])

#Only contested for Republicans
length(allcontestedracedata$id[sign(allcontestedracedata$numb_rep_waste) >= 1])

#Only contested for Democrats
length(allcontestedracedata$id[sign(allcontestedracedata$numb_dem_waste) >= 1])


###############
#####  7  #####
###############

# Number of districts won due to uncontested races

#For republicans
length(dataframe.electionresults$id[sign(dataframe.electionresults$numb_rep_waste) >= 1]) - length(allcontestedracedata$id[sign(allcontestedracedata$numb_rep_waste) >= 1])

#For democrats
length(dataframe.electionresults$id[sign(dataframe.electionresults$numb_dem_waste) >= 1]) - length(allcontestedracedata$id[sign(allcontestedracedata$numb_dem_waste) >= 1])























library(rgdal)
library(sp)
library(rgeos)
library(maptools)
library(ggmap)
library(readr)
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


#######################################
###### Graphs
# All contests bars

countydata_cols1 <- ifelse(dataframe.electionresults$numb_rep_waste < 0, "Democratic", "Republican")


gg1 <- ggplot(data = countydata, mapping = aes(x=id, y=numb_rep_waste, fill = countydata_cols1))
gg1 <- gg1 + geom_bar(stat="identity")
gg1 <- gg1 + theme_minimal()
gg1 <- gg1 + scale_fill_brewer(palette="Dark2")
gg1 <- gg1 + coord_flip()
gg1 <- gg1 + scale_y_continuous(limits = c(-7500,7500))
gg1



# Only contested results

countydata_cols2 <- ifelse(allcontestedracedata$numb_rep_waste < 0, "Democratic", "Republican")


gg2 <- ggplot(data = allcontestedracedata, mapping = aes(x=id, y=numb_rep_waste, fill = countydata_cols))
gg2 <- gg2 + geom_bar(stat="identity")
gg2 <- gg2 + theme_minimal()
gg2 <- gg2 + scale_fill_brewer(palette="Dark2")
gg2 <- gg2 + coord_flip()
gg2 <- gg2 + scale_y_continuous(limits = c(-7500,7500))
gg2



###################################
######## Visuals ##################
###################################





barplot(allcontestedracedata$numb_rep_waste, col=countydata_cols)
