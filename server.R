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

# change blank and 0 to na
Dover[Dover == 0] = NA 
Dover[Dover == " "] = NA
Dover[Dover == "  "] = NA
Dover[Dover == "   "] = NA
Dover[Dover == "    "] = NA
Dover[Dover == "     "] = NA
Dover[Dover == "            "] = NA
Dover[Dover == "               "] = NA
Dover[Dover == "                    "] = NA
Dover[Dover == "                              "] = NA

# remove useless columns
Dover <- subset(Dover, select = c(woNumber, 
                                  MWCN.SHOPCODE,
                                  MWCN.DATE.MAT,
                                  MWCN.CUR.FY,
                                  MWOA.SICODE,
                                  MWOA.FACIDNR,
                                  MWOA.WOTITLE,
                                  MWOA.LUC,
                                  MWOA.MILHRS,
                                  MWOA.CIVHRS,
                                  MWOA.TOTHRS,
                                  MWOA.TOTCOST,
                                  MWOA.DATEOPEN,
                                  MWOA.DATECLOS,
                                  MWOA.DATEPERF,
                                  MWOA.MODDATE))

# change date columns from number to date
Dover$MWCN.DATE.MAT <- as.Date(as.character(Dover$MWCN.DATE.MAT), "%y%m%d") 
Dover$MWOA.DATEOPEN <- as.Date(as.character(Dover$MWOA.DATEOPEN), "%y%m%d")
Dover$MWOA.DATECLOS <- as.Date(as.character(Dover$MWOA.DATECLOS), "%y%m%d")
Dover$MWOA.DATEPERF <- as.Date(as.character(Dover$MWOA.DATEPERF), "%y%m%d")
Dover$MWOA.MODDATE <- as.Date(as.character(Dover$MWOA.MODDATE), "%y%m%d") 

shinyServer(function(input, output) {
  
  output$daysOpenVsHoursCharged <- renderPlot({

    # turn each subset into a new column/new dataset
    # subset based on shop dropdown 
    Dover <- if(input$shop == 'Structures') {subset(Dover, Dover$MWCN.SHOPCODE == 'CA')
    } else if (input$shop == 'Entomology') {subset(Dover, Dover$MWCN.SHOPCODE == 'LF')
    } else if (input$shop == 'Water Plant') {subset(Dover, Dover$MWCN.SHOPCODE == 'HZ')
    } else if (input$shop == 'Water') {subset(Dover, Dover$MWCN.SHOPCODE == 'EC')
    } else if (input$shop == 'Electrical') {subset(Dover, Dover$MWCN.SHOPCODE == 'IT')
    } else if (input$shop == 'Alarms') {subset(Dover, Dover$MWCN.SHOPCODE == 'ET')
    } else if (input$shop == 'PowerPro') {subset(Dover, Dover$MWCN.SHOPCODE == 'OX')
    } else if (input$shop == 'HVAC') {subset(Dover, Dover$MWCN.SHOPCODE == 'WW')
    } else if (input$shop == 'Sweeper') {subset(Dover, Dover$MWCN.SHOPCODE == 'EO')
    } else if (input$shop == 'Dorms') {subset(Dover, Dover$MWCN.SHOPCODE == 'PC')
    } else if (input$shop == 'EMCS') {subset(Dover, Dover$MWCN.SHOPCODE == 'CX')
    } else if (input$shop == 'ENG') {subset(Dover, Dover$MWCN.SHOPCODE == 'PG')
    } else Dover
    
    # subset based on priority dropdown
    Dover <- if(input$priority == '1: Emergency Corrective Work') {subset(Dover, Dover$MWOA.SICODE == '1 ')
    } else if (input$priority == '2A: Preventive Maintenance') {subset(Dover, Dover$MWOA.SICODE == '2A')
    } else if (input$priority == '2B: Contingency Construction Training') {subset(Dover, Dover$MWOA.SICODE == '2B')
    } else if (input$priority == '3A: Sustainment (High)') {subset(Dover, Dover$MWOA.SICODE == '3A')
    } else if (input$priority == '3B: Sustainment (Medium)') {subset(Dover, Dover$MWOA.SICODE == '3B')
    } else if (input$priority == '3C: Sustainment (Low)') {subset(Dover, Dover$MWOA.SICODE == '3C')
    } else if (input$priority == '4A: Enhancement (High)') {subset(Dover, Dover$MWOA.SICODE == '4A')
    } else if (input$priority == '4B: Enhancement (Low)') {subset(Dover, Dover$MWOA.SICODE == '4B')
    } else Dover
    
    # create new column for days open
    Dover$daysOpen <- as.numeric(Dover$MWOA.DATECLOS - Dover$MWOA.DATEOPEN)
    
    # get rid of wo open less than 0 days
    Dover <- subset(Dover, Dover$daysOpen >= 0) 
    
    # get rid of wo open more than 3 years
    Dover <- subset(Dover, Dover$MWOA.DATECLOS - Dover$MWOA.DATEOPEN <= input$daysOpenSlider)
    
    # prevent errors with no data
    if(nrow(Dover) <= 2) {ggplot(NULL, aes(0, 0)) + ggtitle('No Data')
    } else ggplot(Dover, aes(daysOpen, MWOA.TOTHRS)) +
      geom_point() +
      geom_smooth(method = lm) +
      ggtitle("WO Days Open vs Hours Charged") +
      labs(x = "Days Open", y = "Hours Charged")
  })
  # need to remove finite or missing values
  output$daysOpenAfterMaterialVsHoursCharged <- renderPlot({
    # subset based on shop dropdown 
    Dover <- if(input$shop == 'Structures') {subset(Dover, Dover$MWCN.SHOPCODE == 'CA')
    } else if (input$shop == 'Entomology') {subset(Dover, Dover$MWCN.SHOPCODE == 'LF')
    } else if (input$shop == 'Water Plant') {subset(Dover, Dover$MWCN.SHOPCODE == 'HZ')
    } else if (input$shop == 'Water') {subset(Dover, Dover$MWCN.SHOPCODE == 'EC')
    } else if (input$shop == 'Electrical') {subset(Dover, Dover$MWCN.SHOPCODE == 'IT')
    } else if (input$shop == 'Alarms') {subset(Dover, Dover$MWCN.SHOPCODE == 'ET')
    } else if (input$shop == 'PowerPro') {subset(Dover, Dover$MWCN.SHOPCODE == 'OX')
    } else if (input$shop == 'HVAC') {subset(Dover, Dover$MWCN.SHOPCODE == 'WW')
    } else if (input$shop == 'Sweeper') {subset(Dover, Dover$MWCN.SHOPCODE == 'EO')
    } else if (input$shop == 'Dorms') {subset(Dover, Dover$MWCN.SHOPCODE == 'PC')
    } else if (input$shop == 'EMCS') {subset(Dover, Dover$MWCN.SHOPCODE == 'CX')
    } else if (input$shop == 'ENG') {subset(Dover, Dover$MWCN.SHOPCODE == 'PG')
    } else Dover
    # subset based on priority dropdown
    Dover <- if(input$priority == '1: Emergency Corrective Work') {subset(Dover, Dover$MWOA.SICODE == '1 ')
    } else if (input$priority == '2A: Preventive Maintenance') {subset(Dover, Dover$MWOA.SICODE == '2A')
    } else if (input$priority == '2B: Contingency Construction Training') {subset(Dover, Dover$MWOA.SICODE == '2B')
    } else if (input$priority == '3A: Sustainment (High)') {subset(Dover, Dover$MWOA.SICODE == '3A')
    } else if (input$priority == '3B: Sustainment (Medium)') {subset(Dover, Dover$MWOA.SICODE == '3B')
    } else if (input$priority == '3C: Sustainment (Low)') {subset(Dover, Dover$MWOA.SICODE == '3C')
    } else if (input$priority == '4A: Enhancement (High)') {subset(Dover, Dover$MWOA.SICODE == '4A')
    } else if (input$priority == '4B: Enhancement (Low)') {subset(Dover, Dover$MWOA.SICODE == '4B')
    } else Dover
    
    # create new column for days open
    Dover$daysOpenAfterMaterial <- as.numeric(Dover$MWOA.DATECLOS - Dover$MWCN.DATE.MAT)
    
    # get rid of wo open less than 0 days
    Dover <- subset(Dover, Dover$daysOpenAfterMaterial >= 0) 
    
    # get rid of wo open more than 3 years
    Dover <- subset(Dover, Dover$MWOA.DATECLOS - Dover$MWCN.DATE.MAT <= input$daysOpenSlider)
    
    # # remove rows with na (not working?)
    # Dover <- subset(Dover, !is.na(daysOpenAfterMaterial))
    # Dover <- subset(Dover, !is.na(MWOA.TOTHRS))
    
    # prevent errors with no data
    if(nrow(Dover) <= 2) {ggplot(NULL, aes(0, 0)) + ggtitle('No Data')
    } else ggplot(Dover, aes(daysOpenAfterMaterial, MWOA.TOTHRS)) +
      geom_point() +
      geom_smooth(method = lm) +
      ggtitle("WO Days Open After Material vs Hours Charged") +
      labs(x = "Days Open After Material", y = "Hours Charged") #+
    # annotate("text", 
    # x = max(Dover$daysOpenAfterMaterial), 
    # y = max(Dover$MWOA.TOTHRS), 
    # label = "Insert correlation coefficient here", 
    # hjust = 1)
  })

  output$woClosedPerDay <- renderPlot({
    
    # subset based on shop dropdown 
    Dover <- if(input$shop == 'Structures') {subset(Dover, Dover$MWCN.SHOPCODE == 'CA')
    } else if (input$shop == 'Entomology') {subset(Dover, Dover$MWCN.SHOPCODE == 'LF')
    } else if (input$shop == 'Water Plant') {subset(Dover, Dover$MWCN.SHOPCODE == 'HZ')
    } else if (input$shop == 'Water') {subset(Dover, Dover$MWCN.SHOPCODE == 'EC')
    } else if (input$shop == 'Electrical') {subset(Dover, Dover$MWCN.SHOPCODE == 'IT')
    } else if (input$shop == 'Alarms') {subset(Dover, Dover$MWCN.SHOPCODE == 'ET')
    } else if (input$shop == 'PowerPro') {subset(Dover, Dover$MWCN.SHOPCODE == 'OX')
    } else if (input$shop == 'HVAC') {subset(Dover, Dover$MWCN.SHOPCODE == 'WW')
    } else if (input$shop == 'Sweeper') {subset(Dover, Dover$MWCN.SHOPCODE == 'EO')
    } else if (input$shop == 'Dorms') {subset(Dover, Dover$MWCN.SHOPCODE == 'PC')
    } else if (input$shop == 'EMCS') {subset(Dover, Dover$MWCN.SHOPCODE == 'CX')
    } else if (input$shop == 'ENG') {subset(Dover, Dover$MWCN.SHOPCODE == 'PG')
    } else Dover
    
    # subset based on priority dropdown
    Dover <- if(input$priority == '1: Emergency Corrective Work') {subset(Dover, Dover$MWOA.SICODE == '1 ')
    } else if (input$priority == '2A: Preventive Maintenance') {subset(Dover, Dover$MWOA.SICODE == '2A')
    } else if (input$priority == '2B: Contingency Construction Training') {subset(Dover, Dover$MWOA.SICODE == '2B')
    } else if (input$priority == '3A: Sustainment (High)') {subset(Dover, Dover$MWOA.SICODE == '3A')
    } else if (input$priority == '3B: Sustainment (Medium)') {subset(Dover, Dover$MWOA.SICODE == '3B')
    } else if (input$priority == '3C: Sustainment (Low)') {subset(Dover, Dover$MWOA.SICODE == '3C')
    } else if (input$priority == '4A: Enhancement (High)') {subset(Dover, Dover$MWOA.SICODE == '4A')
    } else if (input$priority == '4B: Enhancement (Low)') {subset(Dover, Dover$MWOA.SICODE == '4B')
    } else Dover
    
    # create new column for days open
    Dover$daysOpenAfterMaterial <- as.numeric(Dover$MWOA.DATECLOS - Dover$MWCN.DATE.MAT)
    
    # get rid of wo open less than 0 days
    Dover <- subset(Dover, Dover$daysOpenAfterMaterial >= 0) 
    
    # get rid of wo open more than 3 years
    Dover <- subset(Dover, Dover$MWOA.DATECLOS - Dover$MWCN.DATE.MAT <= input$daysOpenSlider)
    
    # count wo closed per day
    countWoClosedPerDay <- function(X) {
      dates <- as.Date(strftime(Dover$MWOA.DATECLOS[!is.na(Dover$MWOA.DATECLOS)], "%Y-%m-%d"))
      allDates <- seq(from = min(dates), to = max(dates), by = "day")
      woCount <- sapply(allDates, FUN = function(X) sum(dates == X))
      data.frame(day = allDates, woCount = woCount)}
    woClosed <- countWoClosedPerDay(Dover$MWOA.DATECLOS)
    
    woClosed <- woClosed[(woClosed$day >= as.Date("2015-07-01") 
                          & woClosed$day <= as.Date("2016-06-30")), ] # restricts dates to same size as woOpened
    # plot(woClosed$day, 
    #      woClosed$woCount,
    #      xlab = "Date", 
    #      ylab = "WO Closed",
    #      xlim = as.Date(c("2015-07-01", "2016-07-01")),
    #      ylim = c(0, 100),
    #      main = "WO Closed per Day")
    # prevent errors with no data
    if(nrow(Dover) <= 2) {ggplot(NULL, aes(0, 0)) + ggtitle('No Data')
    } else ggplot(woClosed, aes(woClosed$day,woClosed$woCount)) + 
      geom_point() + 
      geom_smooth() + 
      ggtitle("WO Closed per Day") + 
      labs(x = "Date", y = "WO Closed")
  })

  output$meanClosed <- renderText({
    mean(Dover$MWOA.TOTHRS)
  })
})
