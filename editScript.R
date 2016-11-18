library(ggplot2)

# load files (change file location based on dataset)
MWCN <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWCN", header = TRUE, sep = "|")
MWOA <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWOA", header = TRUE, sep = "|")

# rename work order number column (to merge datasets)
names(MWCN)[names(MWCN) == "MWCN.WONR"] <- "woNumber"
names(MWOA)[names(MWOA) == "MWOA.WONR"] <- "woNumber"

# merge datasets
Dover <- merge(MWCN, MWOA, by = "woNumber")

# change date columns from number to date
Dover$MWOA.SHOPCOMP <- as.Date(as.character(Dover$MWOA.SHOPCOMP), "%y%m%d")
Dover$MWOA.TRKSUSP <- as.Date(as.character(Dover$MWOA.TRKSUSP), "%y%m%d")
Dover$MWOA.DATEOPEN <- as.Date(as.character(Dover$MWOA.DATEOPEN), "%y%m%d")
Dover$MWOA.DATEMATL <- as.Date(as.character(Dover$MWOA.DATEMATL), "%y%m%d")
Dover$MWOA.DATECLOS <- as.Date(as.character(Dover$MWOA.DATECLOS), "%y%m%d")
Dover$MWOA.DATEPERF <- as.Date(as.character(Dover$MWOA.DATEPERF), "%y%m%d")
Dover$MWOA.CREDATE <- as.Date(as.character(Dover$MWOA.CREDATE), "%y%m%d")
Dover$MWOA.MODDATE <- as.Date(as.character(Dover$MWOA.MODDATE), "%y%m%d")

Dover <- Dover

Dover$daysOpen <- Dover$MWOA.DATECLOS - Dover$MWOA.DATEOPEN

# get rid of wo open less than 0 days
Dover <- subset(Dover, Dover$daysOpen >= 0) 

ggplot(Dover, aes(Dover$daysOpen,Dover$MWOA.TOTHRS)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle("WO Closed per Day") + 
  labs(x = "Date", y = "WO Closed")


