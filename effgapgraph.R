##############################
##### ggplot2 classnotes #####
##############################

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(scales)

elec <- read_csv("data/nhelectiondata.csv")

waste <- select(elec, id, dem_eff_gap, rep_eff_gap, contested)

waste_long <- gather(waste, party, count, -id, -contested)

waste_long <- filter(waste_long, count > 0)

waste_long <- mutate(waste_long, count = ifelse(party == "dem_eff_gap", -count, count))

waste_long <- mutate(waste_long, party = ifelse(party == "dem_eff_gap", "D", "R"))

waste_long <- mutate(waste_long,
                     count = round(count, digits = 1))

waste_long <- arrange(waste_long, count)

waste_long <- mutate(waste_long,
                     id=factor(id,
                               levels=waste_long$id))

waste_long <- mutate(waste_long,
                     id=factor(id,
                               levels=rev(waste_long$id)))

waste_long <- mutate(waste_long,
                     just = ifelse(party == "D", 2, -1))

#create label column,
waste_long <- mutate(waste_long,
                     lab=ifelse(sign(count)<0,
                                sprintf("(%s) %s", paste(comma(abs(count)), "%"), id),
                                sprintf("%s (%s)", id, paste(comma(abs(count)), "%", sep = ""))))


fill_cols <- c(R="#b2182b", D="#2166ac")


gg <- ggplot(waste_long, aes(id, count))
gg <- gg + geom_bar(stat = "identity", aes(fill = party), color = "white")
gg <- gg + geom_text(aes(y=-(sign(count)*100), label = lab, hjust = just), size = 2.25) # change id to lab
gg <- gg + scale_fill_manual(values= fill_cols)
gg <- gg + labs(x = NULL, y = NULL,
                title = "Efficiency Gap (NH General State House 2014)") #
gg <- gg + theme_bw() 
gg <- gg + theme(panel.grid.major.y = element_blank())
gg <- gg + theme(panel.grid.minor.x = element_blank())
gg <- gg + theme(panel.grid.minor.y = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(legend.position = "none")
gg <- gg + coord_flip()
gg



