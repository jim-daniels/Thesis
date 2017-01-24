# load packages
library(ggplot2)

# load files (change file location based on dataset)
baseData <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWOA", 
                    header = TRUE, 
                    sep = "|")

# remove useless columns
baseData <- subset(baseData, select = c(MWOA.SICODE,
                                        MWOA.MILHRS,
                                        MWOA.CIVHRS))

# remove inaccurate rows (couldn't figure out another way to only show correct inputs)
baseData[baseData == "1A"] = NA
baseData[baseData == "2 "] = NA
baseData[baseData == "3Z"] = NA
baseData[baseData == "4 "] = NA
baseData[baseData == "EC"] = NA
baseData[baseData == "MD"] = NA
baseData[baseData == "X "] = NA
na.omit(baseData)

# count WO in each priority
count <- table(baseData$MWOA.SICODE)

# sum mil hours charged for each priority
hrMil <- aggregate(baseData$MWOA.MILHRS, by = list(Var1 = baseData$MWOA.SICODE), FUN = sum)

# sum civ hours charged for each priority
hrCiv <- aggregate(baseData$MWOA.CIVHRS, by = list(Var1 = baseData$MWOA.SICODE), FUN = sum)

# merge datasets
priorityImpact <- merge(count, hrMil, by = "Var1")
priorityImpact <- merge(priorityImpact, hrCiv, by = "Var1")

# rename columns
names(priorityImpact)[names(priorityImpact) == "Var1"] <- "Priority"
names(priorityImpact)[names(priorityImpact) == "Freq"] <- "WO_Count"
names(priorityImpact)[names(priorityImpact) == "x.x"] <- "Mil_Hours"
names(priorityImpact)[names(priorityImpact) == "x.y"] <- "Civ_Hours"

# convert to long format for plotting
library(tidyr)
priorityImpact <- gather(priorityImpact, Legend, Count, WO_Count:Civ_Hours, factor_key=TRUE)

# plot data
ggplot(data = priorityImpact, aes(x = Priority, y = Count, fill = Legend)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  ggtitle("Priority Impact")

# Questions???
# add hours per WO averages?
# why is the read.delim dataset smaller than the excel text to columns dataset
# replace blank with No Priority?

# Why doesn't na.omit work here?
# # remove inaccurate rows
# priorityImpact[priorityImpact == "1A"] = NA
# priorityImpact[priorityImpact == "2 "] = NA
# priorityImpact[priorityImpact == "3Z"] = NA
# priorityImpact[priorityImpact == "4 "] = NA
# priorityImpact[priorityImpact == "EC"] = NA
# priorityImpact[priorityImpact == "MD"] = NA
# priorityImpact[priorityImpact == "X "] = NA
# na.omit(priorityImpact)
# or
# priorityImpact[!is.na(priorityImpact$Priority),]