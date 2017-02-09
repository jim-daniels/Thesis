# ----

# load packages
library(shiny)
library(ggplot2)
library(tidyr)

# load data for each base
# load files (change file location based on dataset)
MWCN <- read.delim(file = "Data/Dover/MWCN", header = TRUE, sep = "|", quote = "", skipNul = TRUE)
MWOA <- read.delim(file = "Data/Dover/MWOA", header = TRUE, sep = "|", quote = "", skipNul = TRUE)
# rename work order number column (to merge datasets)
names(MWCN)[names(MWCN) == "MWCN.WONR"] <- "WO_Number"
names(MWOA)[names(MWOA) == "MWOA.WONR"] <- "WO_Number"
# merge datasets
doverData <- merge(MWCN, MWOA, by = "WO_Number")

# remove useless columns
doverData <- subset(doverData, select = c(WO_Number,
                                          MWOA.WOTITLE,
                                          MWOA.FACIDNR,
                                          MWCN.SHOPCODE,
                                          MWOA.SICODE,
                                          MWCN.EST.HRS,
                                          MWCN.MIL.HRS,
                                          MWCN.CIV.HRS,
                                          MWOA.DATEOPEN,
                                          MWCN.DATECLOS))
# change date columns from number to date
doverData$MWCN.DATECLOS <- as.Date(as.character(doverData$MWCN.DATECLOS), "%y%m%d")
doverData$MWOA.DATEOPEN <- as.Date(as.character(doverData$MWOA.DATEOPEN), "%y%m%d")

# repeat for Dyess
MWCN <- read.delim(file = "Data/Dyess/MWCN", header = TRUE, sep = "|", quote = "", skipNul = TRUE)
MWOA <- read.delim(file = "Data/Dyess/MWOA", header = TRUE, sep = "|", quote = "", skipNul = TRUE)
names(MWCN)[names(MWCN) == "MWCN.WONR"] <- "WO_Number"
names(MWOA)[names(MWOA) == "MWOA.WONR"] <- "WO_Number"
dyessData <- merge(MWCN, MWOA, by = "WO_Number")
dyessData <- subset(dyessData, select = c(WO_Number,
                                          MWOA.WOTITLE,
                                          MWOA.FACIDNR,
                                          MWCN.SHOPCODE,
                                          MWOA.SICODE,
                                          MWCN.EST.HRS,
                                          MWCN.MIL.HRS,
                                          MWCN.CIV.HRS,
                                          MWOA.DATEOPEN,
                                          MWCN.DATECLOS))
dyessData$MWCN.DATECLOS <- as.Date(as.character(dyessData$MWCN.DATECLOS), "%y%m%d")
dyessData$MWOA.DATEOPEN <- as.Date(as.character(dyessData$MWOA.DATEOPEN), "%y%m%d")

# repeat for Minot
MWCN <- read.delim(file = "Data/Minot/MWCN", header = TRUE, sep = "|", quote = "", skipNul = TRUE)
MWOA <- read.delim(file = "Data/Minot/MWOA", header = TRUE, sep = "|", quote = "", skipNul = TRUE)
names(MWCN)[names(MWCN) == "MWCN.WONR"] <- "WO_Number"
names(MWOA)[names(MWOA) == "MWOA.WONR"] <- "WO_Number"
minotData <- merge(MWCN, MWOA, by = "WO_Number")
minotData <- subset(minotData, select = c(WO_Number,
                                          MWOA.WOTITLE,
                                          MWOA.FACIDNR,
                                          MWCN.SHOPCODE,
                                          MWOA.SICODE,
                                          MWCN.EST.HRS,
                                          MWCN.MIL.HRS,
                                          MWCN.CIV.HRS,
                                          MWOA.DATEOPEN,
                                          MWCN.DATECLOS))
minotData$MWCN.DATECLOS <- as.Date(as.character(minotData$MWCN.DATECLOS), "%y%m%d")
minotData$MWOA.DATEOPEN <- as.Date(as.character(minotData$MWOA.DATEOPEN), "%y%m%d")

# repeat for Scott
MWCN <- read.delim(file = "Data/Scott/MWCN", header = TRUE, sep = "|", quote = "", skipNul = TRUE)
MWOA <- read.delim(file = "Data/Scott/MWOA", header = TRUE, sep = "|", quote = "", skipNul = TRUE)
names(MWCN)[names(MWCN) == "MWCN.WONR"] <- "WO_Number"
names(MWOA)[names(MWOA) == "MWOA.WONR"] <- "WO_Number"
scottData <- merge(MWCN, MWOA, by = "WO_Number")
scottData <- subset(scottData, select = c(WO_Number,
                                          MWOA.WOTITLE,
                                          MWOA.FACIDNR,
                                          MWCN.SHOPCODE,
                                          MWOA.SICODE,
                                          MWCN.EST.HRS,
                                          MWCN.MIL.HRS,
                                          MWCN.CIV.HRS,
                                          MWOA.DATEOPEN,
                                          MWCN.DATECLOS))
scottData$MWCN.DATECLOS <- as.Date(as.character(scottData$MWCN.DATECLOS), "%y%m%d")
scottData$MWOA.DATEOPEN <- as.Date(as.character(scottData$MWOA.DATEOPEN), "%y%m%d")

# ----

# remove close dates outside a year
doverData <- subset(doverData, MWCN.DATECLOS <= max("2016-04-30"))
doverData <- subset(doverData, MWCN.DATECLOS >= min("2015-08-01"))
dyessData <- subset(dyessData, MWCN.DATECLOS <= max("2016-04-30"))
dyessData <- subset(dyessData, MWCN.DATECLOS >= min("2015-08-01"))
minotData <- subset(minotData, MWCN.DATECLOS <= max("2016-04-30"))
minotData <- subset(minotData, MWCN.DATECLOS >= min("2015-08-01"))
scottData <- subset(scottData, MWCN.DATECLOS <= max("2016-04-30"))
scottData <- subset(scottData, MWCN.DATECLOS >= min("2015-08-01"))

# add column for month
doverData$Month <- format(doverData$MWCN.DATECLOS, format = "%y-%m")

# incase you want to do it by week
# doverData$Week <- format(doverData$MWCN.DATECLOS, format = "%y-%U")

# count WO close each month
doverData <- table(doverData$Month)
doverData <- as.data.frame(doverData)

# rename column
names(doverData)[names(doverData) == "Var1"] <- "Date"
names(doverData)[names(doverData) == "Freq"] <- "WO_Closed"

# add column for base discrimination
doverData$Base <- "Dover"

# repeat for Dyess
dyessData$Month <- format(dyessData$MWCN.DATECLOS, format = "%y-%m")
dyessData <- table(dyessData$Month)
dyessData <- as.data.frame(dyessData)
names(dyessData)[names(dyessData) == "Var1"] <- "Date"
names(dyessData)[names(dyessData) == "Freq"] <- "WO_Closed"
dyessData$Base <- "Dyess"

# repeat for Minot
minotData$Month <- format(minotData$MWCN.DATECLOS, format = "%y-%m")
minotData <- table(minotData$Month)
minotData <- as.data.frame(minotData)
names(minotData)[names(minotData) == "Var1"] <- "Date"
names(minotData)[names(minotData) == "Freq"] <- "WO_Closed"
minotData$Base <- "Minot"

# repeat for Scott
scottData$Month <- format(scottData$MWCN.DATECLOS, format = "%y-%m")
scottData <- table(scottData$Month)
scottData <- as.data.frame(scottData)
names(scottData)[names(scottData) == "Var1"] <- "Date"
names(scottData)[names(scottData) == "Freq"] <- "WO_Closed"
scottData$Base <- "Scott"

# combine to one dataset
baseData <- rbind(doverData, dyessData, minotData, scottData)

# # plot data
# ggplot(data = baseData, aes(x = Month, y = WO_Closed, fill = Base)) +
#   geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
#   ggtitle("Base Comparison")

ggplot(data = baseData, aes(x = Date, y = WO_Closed, group = Base, colour = Base)) +
  geom_line() +
  geom_point() +
  ggtitle("Base Comparison")
