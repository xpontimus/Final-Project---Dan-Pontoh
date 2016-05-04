library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(scales)

URL <- "data/nhelectiondata.csv"

elec <- read_csv(URL)

waste <- select(elec, id, numb_dem_waste, numb_rep_waste)

waste_long <- gather(waste, party, count, -id)

waste_long <- filter(waste_long, count > 0)

waste_long <- mutate(waste_long, count = ifelse(party == "numb_dem_waste", -count, count))

waste_long <- mutate(waste_long, party = ifelse(party == "numb_dem_waste", "D", "R"))

waste_long <- mutate(waste_long,
                     count = as.integer(count))

waste_long <- arrange(waste_long, count)

waste_long <- mutate(waste_long,
                     id=factor(id,
                               levels=waste_long$id))

waste_long <- mutate(waste_long,
                     id=factor(id,
                               levels=rev(waste_long$id)))

waste_long <- mutate(waste_long,
                     just = ifelse(party == "D", 0, 1))

waste_long <- mutate(waste_long,
                     lab=ifelse(sign(count)<0,
                                sprintf("(%s) %s", comma(abs(count)), id),
                                sprintf("%s (%s)", id, comma(abs(count)))))

fill_cols <- c(R="#b2182b", D="#2166ac")


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
