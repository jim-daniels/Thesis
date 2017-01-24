# Questions???
# warnings?

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
                                        MWCN.DATECLOS))
# change date columns from number to date
doverData$MWCN.DATECLOS <- as.Date(as.character(doverData$MWCN.DATECLOS), "%y%m%d")

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
                                        MWCN.DATECLOS))
dyessData$MWCN.DATECLOS <- as.Date(as.character(dyessData$MWCN.DATECLOS), "%y%m%d")

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
                                         MWCN.DATECLOS))
minotData$MWCN.DATECLOS <- as.Date(as.character(minotData$MWCN.DATECLOS), "%y%m%d")

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
                                         MWCN.DATECLOS))
scottData$MWCN.DATECLOS <- as.Date(as.character(scottData$MWCN.DATECLOS), "%y%m%d")

function(input, output) {
  
  # create reactive
  baseReactive <- reactive({

    baseData <- if(input$base == 'Dover') {doverData
    } else if(input$base == 'Dyess') {dyessData
    } else if(input$base == 'Minot') {minotData
    } else scottData
   
    # subset shop dropdown
    baseData <- if(input$shop == 'Structures') {subset(baseData, MWCN.SHOPCODE == 'CA')
    } else if (input$shop == 'Entomology') {subset(baseData, MWCN.SHOPCODE == 'LF')
    } else if (input$shop == 'Water Plant') {subset(baseData, MWCN.SHOPCODE == 'HZ')
    } else if (input$shop == 'Water') {subset(baseData, MWCN.SHOPCODE == 'EC')
    } else if (input$shop == 'Electrical') {subset(baseData, MWCN.SHOPCODE == 'IT')
    } else if (input$shop == 'Alarms') {subset(baseData, MWCN.SHOPCODE == 'ET')
    } else if (input$shop == 'PowerPro') {subset(baseData, MWCN.SHOPCODE == 'OX')
    } else if (input$shop == 'HVAC') {subset(baseData, MWCN.SHOPCODE == 'WW')
    } else if (input$shop == 'Sweeper') {subset(baseData, MWCN.SHOPCODE == 'EO')
    } else if (input$shop == 'Dorms') {subset(baseData, MWCN.SHOPCODE == 'PC')
    } else if (input$shop == 'EMCS') {subset(baseData, MWCN.SHOPCODE == 'CX')
    } else if (input$shop == 'ENG') {subset(baseData, MWCN.SHOPCODE == 'PG')
    } else baseData
    
    # subset priority dropdown
    baseData <- if(input$priority == '1: Emergency Corrective Work') {subset(baseData, MWOA.SICODE == '1 ')
    } else if (input$priority == '2A: Preventive Maintenance') {subset(baseData, MWOA.SICODE == '2A')
    } else if (input$priority == '2B: Contingency Construction Training') {subset(baseData, MWOA.SICODE == '2B')
    } else if (input$priority == '3A: Sustainment (High)') {subset(baseData, MWOA.SICODE == '3A')
    } else if (input$priority == '3B: Sustainment (Medium)') {subset(baseData, MWOA.SICODE == '3B')
    } else if (input$priority == '3C: Sustainment (Low)') {subset(baseData, MWOA.SICODE == '3C')
    } else if (input$priority == '4A: Enhancement (High)') {subset(baseData, MWOA.SICODE == '4A')
    } else if (input$priority == '4B: Enhancement (Low)') {subset(baseData, MWOA.SICODE == '4B')
    } else baseData
  })
  
  output$testPlot <- renderPlot({
    
    # turn reactive into dataframe
    baseData <- as.data.frame(baseReactive())
    
    # remove useless columns
    baseData <- subset(baseData, select = c(MWCN.MIL.HRS,
                                            MWCN.CIV.HRS,
                                            MWCN.DATECLOS))
    
    # count WO close each day
    count <- table(baseData$MWCN.DATECLOS)
    count <- as.data.frame(count)
    count$Var1 <- as.Date(as.character(count$Var1), "%Y-%m-%d")
    
    # sum mil hours charged for each day
    hrMil <- aggregate(baseData$MWCN.MIL.HRS, by = list(Var1 = baseData$MWCN.DATECLOS), FUN = sum)
    
    # sum civ hours charged for each day
    hrCiv <- aggregate(baseData$MWCN.CIV.HRS, by = list(Var1 = baseData$MWCN.DATECLOS), FUN = sum)
    
    # merge datasets
    manpowerOutput <- merge(count, hrMil, by = "Var1")
    manpowerOutput <- merge(manpowerOutput, hrCiv, by = "Var1")
    
    # rename columns
    names(manpowerOutput)[names(manpowerOutput) == "Var1"] <- "Date"
    names(manpowerOutput)[names(manpowerOutput) == "Freq"] <- "WO_Closed"
    names(manpowerOutput)[names(manpowerOutput) == "x.x"] <- "Mil_Hours"
    names(manpowerOutput)[names(manpowerOutput) == "x.y"] <- "Civ_Hours"
    
    # convert to long format for plotting
    manpowerOutput <- gather(manpowerOutput, Legend, Count, WO_Closed:Civ_Hours, factor_key=TRUE)
    
    # plot data
    ggplot(data = manpowerOutput, aes(x = Date, y = Count, fill = Legend)) +
      geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
      ggtitle("Manpower Output")  })
  
  output$testTable <- DT::renderDataTable({
    DT::dataTable(baseData, options = list(pageLength = 25))
  })
}