library(readr)
library(tabplot)
library(readxl)

URL <- "data/Table_1.2_Primary_Energy_Production_by_Source.xlsx"


file <- read_excel(URL, sheet = 2)

colnames(file) <- c(file[10,])

data <- file[12:78,]

data$`Annual Total` <- as.character(data$`Annual Total`)

