##############################
##### ggplot2 classnotes #####
##############################

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(scales)

URL <- "data/nhelectiondata.csv"

elec <- read_csv(URL)

waste <- select(elec, id, numb_dem_waste, numb_rep_waste)

#First use tidyr

View(waste)

waste_long <- gather(waste, party, count, -id) #gather from the waste dataframe, story 

waste_long <- filter(waste_long, count > 0)

waste_long <- mutate(waste_long, count = ifelse(party == "numb_dem_waste", -count, count))

waste_long <- mutate(waste_long, party = ifelse(party == "numb_dem_waste", "D", "R"))

waste_long <- mutate(waste_long,
                     count = as.integer(count))

waste_long <- arrange(waste_long, count)

waste_long <- mutate(waste_long,
                     id=factor(id,
                     levels=waste_long$id))



gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity")
gg


waste_long <- mutate(waste_long,
                     id=factor(id,
                               levels=rev(waste_long$id)))

waste_long <- mutate(waste_long,
                     just = ifelse(party == "D", 0, 1))

#create label column,
waste_long <- mutate(waste_long,
                     lab=ifelse(sign(count)<0,
                                sprintf("(%s) %s", comma(abs(count)), id),
                                sprintf("%s (%s)", id, comma(abs(count)))))

View(waste_long)













gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity")
gg

gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg


# go to colorbrewer2.org and pick colors
##b2182b
##2166ac

fill_cols <- c(R="#b2182b", D="#2166ac")
gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg <- gg + scale_fill_manual(values= fill_cols)
gg

#Time to get rid of the ugly
#add a theme
#?theme list for all themes
#?theme_bw black and white themes?

gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + theme(panel.background = element_blank())
gg

#now background is completely gone
#but gridline has disappeared
#comment it out
#add other theme names

gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + theme_bw() #change this theme
#gg <- gg + theme(panel.background = element_blank())
gg

# Want to get rid of horrible gridlines
# and get rid of border
gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + theme_bw() #change this theme
gg <- gg + theme(panel.grid.major.x = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg 


# get rid of the legend
gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + theme_bw()
gg <- gg + theme(panel.grid.major.x = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(legend.position = "none")
gg 


# want to get rid of the labels

gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + labs(x = NULL, y = NULL) #
gg <- gg + theme_bw() 
gg <- gg + theme(panel.grid.major.x = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(legend.position = "none")
gg 



#flip the coordinates get the gridlines back
gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + labs(x = NULL, y = NULL) #
gg <- gg + theme_bw() 
gg <- gg + theme(panel.grid.major.y = element_blank())
gg <- gg + theme(panel.grid.minor.x = element_blank())
gg <- gg + theme(panel.grid.minor.y = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(legend.position = "none")
gg <- gg + coord_flip()
gg 

########## Put the labels next to the bars

# get rid of axis'
gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + labs(x = NULL, y = NULL) #
gg <- gg + theme_bw() 
gg <- gg + theme(panel.grid.major.y = element_blank())
gg <- gg + theme(panel.grid.minor.x = element_blank())
gg <- gg + theme(panel.grid.minor.y = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(legend.position = "none")
gg <- gg + coord_flip()
gg 

#**********refer to ?element_text
#**********refer to ?geom_text
#do just geom_text stuff
gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party))
gg <- gg + geom_text(aes(y=0, label = id, hjust = just))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + labs(x = NULL, y = NULL) #
gg <- gg + theme_bw() 
gg <- gg + theme(panel.grid.major.y = element_blank())
gg <- gg + theme(panel.grid.minor.x = element_blank())
gg <- gg + theme(panel.grid.minor.y = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(legend.position = "none")
gg <- gg + coord_flip()
gg 

# make bars look better
gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party), color = "white")
gg <- gg + geom_text(aes(y=0, label = id, hjust = just))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + labs(x = NULL, y = NULL) #
gg <- gg + theme_bw() 
gg <- gg + theme(panel.grid.major.y = element_blank())
gg <- gg + theme(panel.grid.minor.x = element_blank())
gg <- gg + theme(panel.grid.minor.y = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(legend.position = "none")
gg <- gg + coord_flip()
gg

# nudging
gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party), color = "white")
gg <- gg + geom_text(aes(y=-(sign(count)*100), label = id, hjust = just))
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + labs(x = NULL, y = NULL) #
gg <- gg + theme_bw() 
gg <- gg + theme(panel.grid.major.y = element_blank())
gg <- gg + theme(panel.grid.minor.x = element_blank())
gg <- gg + theme(panel.grid.minor.y = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(legend.position = "none")
gg <- gg + coord_flip()
gg

#add the counts
gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party), color = "white")
gg <- gg + geom_text(aes(y=-(sign(count)*100), label = lab, hjust = just), size = 2.5) # change id to lab
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + labs(x = NULL, y = NULL,
                title = "All Contests Wasted Votes (NH General State House 2014") #
gg <- gg + theme_bw() 
gg <- gg + theme(panel.grid.major.y = element_blank())
gg <- gg + theme(panel.grid.minor.x = element_blank())
gg <- gg + theme(panel.grid.minor.y = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(legend.position = "none")
gg <- gg + coord_flip()
gg



