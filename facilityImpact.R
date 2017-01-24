# load packages
library(ggplot2)

# load files (change file location based on dataset)
baseData <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWOA", 
                    header = TRUE, 
                    sep = "|")

# remove useless columns
baseData <- subset(baseData, select = c(MWOA.FACIDNR,
                                        MWOA.MILHRS,
                                        MWOA.CIVHRS))

# count WO each facility
count <- table(baseData$MWOA.FACIDNR)
count <- as.data.frame(count)

# sum mil hours charged for each facility
hrMil <- aggregate(baseData$MWOA.MILHRS, by = list(Var1 = baseData$MWOA.FACIDNR), FUN = sum)

# sum civ hours charged for each facility
hrCiv <- aggregate(baseData$MWOA.CIVHRS, by = list(Var1 = baseData$MWOA.FACIDNR), FUN = sum)

# merge datasets
facilityImpact <- merge(count, hrMil, by = "Var1")
facilityImpact <- merge(facilityImpact, hrCiv, by = "Var1")

# rename columns
names(facilityImpact)[names(facilityImpact) == "Var1"] <- "Facility"
names(facilityImpact)[names(facilityImpact) == "Freq"] <- "WO_Count"
names(facilityImpact)[names(facilityImpact) == "x.x"] <- "Mil_Hours"
names(facilityImpact)[names(facilityImpact) == "x.y"] <- "Civ_Hours"

# reorder size
facilityImpact <- facilityImpact[with(facilityImpact, order(-WO_Count)), ]

# remove all but highest count facilities
facilityImpact <- facilityImpact[1:10, ]

# convert to long format for plotting
library(tidyr)
facilityImpact <- gather(facilityImpact, Legend, Count, WO_Count:Civ_Hours, factor_key=TRUE)

# plot data
ggplot(data = facilityImpact, aes(x = Facility, y = Count, fill = Legend)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  ggtitle("Facility Impact")

# Questions???
# sort by WO count?
# both stack and dodge to split between open/closed WO and civ/mil hrs?
# frequency slider? or only show top 10?
# add IFAC to split between 
# use MWOA.CUSTCDE instead of facility number?