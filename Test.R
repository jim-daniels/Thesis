# load packages
library(shiny)
library(ggplot2)

# load files (change file location based on dataset)
MWCN <- read.delim(file = "Data/Dover/MWCN", header = TRUE, sep = "|")
MWOA <- read.delim(file = "Data/Dover/MWOA", header = TRUE, sep = "|")

# rename work order number column (to merge datasets)
names(MWCN)[names(MWCN) == "MWCN.WONR"] <- "WO_Number"
names(MWOA)[names(MWOA) == "MWOA.WONR"] <- "WO_Number"

# merge datasets
baseData <- merge(MWCN, MWOA, by = "WO_Number")

# remove useless columns
baseData <- subset(baseData, select = c(WO_Number,
                                        MWOA.WOTITLE,
                                        MWOA.FACIDNR,
                                        MWCN.SHOPCODE,
                                        MWOA.SICODE,
                                        MWCN.EST.HRS,
                                        MWCN.MIL.HRS,
                                        MWCN.CIV.HRS,
                                        MWCN.DATECLOS))

# change date columns from number to date
baseData$MWCN.DATECLOS <- as.Date(as.character(baseData$MWCN.DATECLOS), "%y%m%d")

names(baseData)[names(baseData) == "MWOA.WOTITLE"] <- "Title"
names(baseData)[names(baseData) == "MWOA.FACIDNR"] <- "Fac_Number"
names(baseData)[names(baseData) == "MWCN.SHOPCODE"] <- "Shop"
names(baseData)[names(baseData) == "MWOA.SICODE"] <- "Priority"
names(baseData)[names(baseData) == "MWCN.EST.HRS"] <- "Est_Hrs"
names(baseData)[names(baseData) == "MWCN.MIL.HRS"] <- "Mil_Hrs"
names(baseData)[names(baseData) == "MWCN.CIV.HRS"] <- "Civ_Hrs"
names(baseData)[names(baseData) == "MWCN.DATECLOS"] <- "Date_Closed"

View(baseData)
