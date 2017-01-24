# load packages
library(ggplot2)

# load files (change file location based on dataset)
baseData <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWOA", 
                       header = TRUE, 
                       sep = "|")

# remove useless columns
baseData <- subset(baseData, select = c(MWOA.ESTHRS,
                                        MWOA.MILHRS,
                                        MWOA.CIVHRS,
                                        MWOA.DATECLOS))

# change date columns from number to date
baseData$MWOA.DATECLOS <- as.Date(as.character(baseData$MWOA.DATECLOS), "%y%m%d")

# add column for just month
baseData$month <- baseData$MWOA.DATECLOS
baseData$month <- format(baseData$month, "%y-%m")

# count WO close each day
count <- table(baseData$month)
count <- as.data.frame(count)

# sum est hours charged for each day
hrEst <- aggregate(baseData$MWOA.ESTHRS, by = list(Var1 = baseData$month), FUN = sum)

# sum mil hours charged for each day
hrMil <- aggregate(baseData$MWOA.MILHRS, by = list(Var1 = baseData$month), FUN = sum)

# sum civ hours charged for each day
hrCiv <- aggregate(baseData$MWOA.CIVHRS, by = list(Var1 = baseData$month), FUN = sum)

# merge datasets
schedulingCompliance <- merge(count, hrEst, by = "Var1")
schedulingCompliance <- merge(schedulingCompliance, hrMil, by = "Var1")
schedulingCompliance <- merge(schedulingCompliance, hrCiv, by = "Var1")

# rename columns
names(schedulingCompliance)[names(schedulingCompliance) == "Var1"] <- "Date"
names(schedulingCompliance)[names(schedulingCompliance) == "Freq"] <- "WO_Closed"
names(schedulingCompliance)[names(schedulingCompliance) == "x.x"] <- "Est_Hours"
names(schedulingCompliance)[names(schedulingCompliance) == "x.y"] <- "Mil_Hours"
names(schedulingCompliance)[names(schedulingCompliance) == "x"] <- "Civ_Hours"

# convert to long format for plotting
library(tidyr)
schedulingCompliance <- gather(schedulingCompliance, Legend, Count, WO_Closed:Civ_Hours, factor_key=TRUE)

# plot data
ggplot(data = schedulingCompliance, aes(x = Date, y = Count, fill = Legend)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  ggtitle("Scheduling Compliance")

# Questions???
# include open?
