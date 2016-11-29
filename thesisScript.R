library(shiny)
library(ggplot2)

# load files (change file location based on dataset)
MWCN <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWCN", 
                   header = TRUE, 
                   sep = "|")
MWOA <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWOA", 
                   header = TRUE, 
                   sep = "|")

# rename work order number column (to merge datasets)
names(MWCN)[names(MWCN) == "MWCN.WONR"] <- "woNumber"
names(MWOA)[names(MWOA) == "MWOA.WONR"] <- "woNumber"

# merge datasets
Dover <- merge(MWCN, MWOA, by = "woNumber")

# # change blanks and 0s to na
# Dover[Dover == 0] = NA 
# Dover[Dover == " "] = NA
# Dover[Dover == "  "] = NA
# Dover[Dover == "   "] = NA
# Dover[Dover == "    "] = NA
# Dover[Dover == "     "] = NA
# Dover[Dover == "            "] = NA
# Dover[Dover == "               "] = NA
# Dover[Dover == "                    "] = NA
# Dover[Dover == "                              "] = NA

Dover[Dover %in% c("0"," ","  ","   ","    ","     ","            ","               ","                    ","                              ")] = NA

View(Dover)
# Dover <- if(input$woType == '5-digit') {subset(Dover, substr(Dover$woNumber, 0, 1) %in% c('1','2','3','4','5','6','7','8','9'))
# } else if (input$woType == 'DSW') {subset(Dover, substr(Dover$woNumber, 0, 1) %in% c('0','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'))
# } else Dover


# remove useless columns
# Dover <- subset(Dover, select = c(woNumber, 
#                                   MWCN.SHOPCODE,
#                                   MWCN.DATE.MAT,
#                                   MWCN.CUR.FY,
#                                   MWOA.SICODE,
#                                   MWOA.FACIDNR,
#                                   MWOA.WOTITLE,
#                                   MWOA.LUC,
#                                   MWOA.MILHRS,
#                                   MWOA.CIVHRS,
#                                   MWOA.TOTHRS,
#                                   MWOA.TOTCOST,
#                                   MWOA.DATEOPEN,
#                                   MWOA.DATECLOS,
#                                   MWOA.DATEPERF,
#                                   MWOA.MODDATE))

# # days open vs hours charged scatter plot
# DoverCombined <- DoverCombined[-c(3), ] # removed 3rd row because date wrong (wo EC105)
# # do I need to remove 0 hours charged from this analysis? what does 0 hours charged mean?
# daysOpen = DoverCombined$MWOA.DATECLOS - DoverCombined$MWOA.DATEOPEN
# plot(daysOpen, 
#      DoverCombined$MWOA.TOTHRS, 
#      xlab = "Days Open", 
#      ylab = "Total Hours Charged",
#      xlim = c(0, 100),
#      ylim = c(0, 100),
#      main = "WO Days Open vs Hours Charged")
# abline(lm(daysOpen ~ DoverCombined$MWOA.TOTHRS)) # best fit linear
# 
# # wo closed per day
# count_wo_closed_per_day <- function(X) {
#   dates <- as.Date(strftime(DoverCombined$MWOA.DATECLOS[!is.na(DoverCombined$MWOA.DATECLOS)], "%Y-%m-%d"))
#   allDates <- seq(from = min(dates), to = max(dates), by = "day")
#   woCount <- sapply(allDates, FUN = function(X) sum(dates == X))
#   data.frame(day = allDates, woCount = woCount)}
# woClosed <- count_wo_closed_per_day(DoverCombined$MWOA.DATECLOS)
# 
# woClosed <- woClosed[(woClosed$day >= as.Date("2015-07-01") 
#                       & woClosed$day <= as.Date("2016-06-30")), ] # restricts dates to same size as woOpened
# plot(woClosed$day, 
#      woClosed$woCount,
#      xlab = "Date", 
#      ylab = "WO Closed",
#      xlim = as.Date(c("2015-07-01", "2016-07-01")),
#      ylim = c(0, 100),
#      main = "WO Closed per Day")
# ggplot(woClosed, aes(woClosed$day,woClosed$woCount)) + 
#   geom_point() + 
#   geom_smooth() + 
#   ggtitle("WO Closed per Day") + 
#   labs(x = "Date", y = "WO Closed")
# 
# # wo opened per day
# count_wo_opened_per_day <- function(X) {
#   dates <- as.Date(strftime(DoverCombined$MWOA.DATEOPEN, "%Y-%m-%d"))
#   allDates <- seq(from = min(dates), to = max(dates), by = "day")
#   woCount <- sapply(allDates, FUN = function(X) sum(dates == X))
#   data.frame(day = allDates, woCount = woCount)}
# woOpened <- count_wo_opened_per_day(DoverCombined$MWOA.DATEOPEN)
# woOpened <- woOpened[(woOpened$day >= as.Date("2015-07-01") 
# ), ] # restricts dates to same size as woClosed if needed add: & woClosed$day <= as.Date("2016-06-30")
# plot(woOpened$day, 
#      woOpened$woCount,
#      xlab = "Date", 
#      ylab = "WO Opened",
#      xlim = as.Date(c("2015-07-01", "2016-07-01")),
#      ylim = c(0, 100),
#      main = "WO Opened per Day")
# 
# # wo opened - closed
# woDifference <- woOpened$woCount - woClosed$woCount
# plot(woOpened$day, 
#      woDifference,
#      xlab = "Date", 
#      ylab = "WO Difference",
#      xlim = as.Date(c("2015-07-01", "2016-07-01")),
#      ylim = NULL,
#      main = "WO Opened - Closed")
# ggplot(data = NULL, aes(woOpened$day, woDifference)) + 
#   geom_point() + 
#   geom_smooth() + 
#   ggtitle("WO Opened - Closed") + 
#   labs(x = "Date", y = "WO Difference")
