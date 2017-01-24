# load packages
library(ggplot2)

# load files (change file location based on dataset)
baseData <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWOA", 
                       header = TRUE, 
                       sep = "|")

# remove useless columns
baseData <- subset(baseData, select = c(MWOA.MILHRS,
                                        MWOA.CIVHRS,
                                        MWOA.DATECLOS))

# change date columns from number to date
baseData$MWOA.DATECLOS <- as.Date(as.character(baseData$MWOA.DATECLOS), "%y%m%d")

# count WO close each day
count <- table(baseData$MWOA.DATECLOS)
count <- as.data.frame(count)
count$Var1 <- as.Date(as.character(count$Var1), "%Y-%m-%d")

# sum mil hours charged for each day
hrMil <- aggregate(baseData$MWOA.MILHRS, by = list(Var1 = baseData$MWOA.DATECLOS), FUN = sum)

# sum civ hours charged for each day
hrCiv <- aggregate(baseData$MWOA.CIVHRS, by = list(Var1 = baseData$MWOA.DATECLOS), FUN = sum)

# merge datasets
manpowerOutput <- merge(count, hrMil, by = "Var1")
manpowerOutput <- merge(manpowerOutput, hrCiv, by = "Var1")

# rename columns
names(manpowerOutput)[names(manpowerOutput) == "Var1"] <- "Date"
names(manpowerOutput)[names(manpowerOutput) == "Freq"] <- "WO_Closed"
names(manpowerOutput)[names(manpowerOutput) == "x.x"] <- "Mil_Hours"
names(manpowerOutput)[names(manpowerOutput) == "x.y"] <- "Civ_Hours"

# convert to long format for plotting
library(tidyr)
manpowerOutput <- gather(manpowerOutput, Legend, Count, WO_Closed:Civ_Hours, factor_key=TRUE)

# plot data
ggplot(data = manpowerOutput, aes(x = Date, y = Count, fill = Legend)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  ggtitle("Manpower Output")

# Questions???
# add WO opened?
# include open? manpower output 2?
# split for week/month/year? or date slider?
# new dataset for indirect work?
# remove weekends? not necessary
# use MWOA.DATEPERF?
