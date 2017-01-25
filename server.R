# Questions???

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
    
    # remove dates outside slider input
    baseData <- subset(baseData, MWCN.DATECLOS <= max(input$dateRange))
    baseData <- subset(baseData, MWCN.DATECLOS >= min(input$dateRange))
  })
  
  output$manpowerOutput <- renderPlot({
    
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
  
  output$priorityImpact <- renderPlot({

    # turn reactive into dataframe
    baseData <- as.data.frame(baseReactive())
    
    # remove useless columns
    baseData <- subset(baseData, select = c(MWOA.SICODE,
                                            MWCN.MIL.HRS,
                                            MWCN.CIV.HRS))
    
    # remove unnecessary rows (many are entered incorrectly) (Scott entered emergency as 1A not 1...)
    baseData <- baseData[which(baseData$MWOA.SICODE %in% c("1 ", "1A", "2A", "2B", "3A", "3B", "3C", "4A", "4B", "  ")),]
    
    # count WO in each priority
    count <- table(baseData$MWOA.SICODE)
    
    # sum mil hours charged for each priority
    hrMil <- aggregate(baseData$MWCN.MIL.HRS, by = list(Var1 = baseData$MWOA.SICODE), FUN = sum)
    
    # sum civ hours charged for each priority
    hrCiv <- aggregate(baseData$MWCN.CIV.HRS, by = list(Var1 = baseData$MWOA.SICODE), FUN = sum)
    
    # merge datasets
    priorityImpact <- merge(count, hrMil, by = "Var1")
    priorityImpact <- merge(priorityImpact, hrCiv, by = "Var1")
    
    # rename columns
    names(priorityImpact)[names(priorityImpact) == "Var1"] <- "Priority"
    names(priorityImpact)[names(priorityImpact) == "Freq"] <- "WO_Count"
    names(priorityImpact)[names(priorityImpact) == "x.x"] <- "Mil_Hours"
    names(priorityImpact)[names(priorityImpact) == "x.y"] <- "Civ_Hours"
    
    # convert to long format for plotting
    priorityImpact <- gather(priorityImpact, Legend, Count, WO_Count:Civ_Hours, factor_key=TRUE)
    
    # plot data
    ggplot(data = priorityImpact, aes(x = Priority, y = Count, fill = Legend)) +
      geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
      ggtitle("Priority Impact")
    
  })
  
  output$facilityImpact <- renderPlot({
    
    # turn reactive into dataframe
    baseData <- as.data.frame(baseReactive())
    
    # count WO each facility
    count <- table(baseData$MWOA.FACIDNR)
    count <- as.data.frame(count)
    
    # sum mil hours charged for each facility
    hrMil <- aggregate(baseData$MWCN.MIL.HRS, by = list(Var1 = baseData$MWOA.FACIDNR), FUN = sum)
    
    # sum civ hours charged for each facility
    hrCiv <- aggregate(baseData$MWCN.CIV.HRS, by = list(Var1 = baseData$MWOA.FACIDNR), FUN = sum)
    
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
    facilityImpact <- gather(facilityImpact, Legend, Count, WO_Count:Civ_Hours, factor_key=TRUE)
    
    # plot data
    ggplot(data = facilityImpact, aes(x = Facility, y = Count, fill = Legend)) +
      geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
      ggtitle("Facility Impact")
  })
  
  output$schedulingCompliance <- renderPlot({
    
    # turn reactive into dataframe
    baseData <- as.data.frame(baseReactive())
    
    # remove useless columns
    baseData <- subset(baseData, select = c(MWCN.EST.HRS,
                                            MWCN.MIL.HRS,
                                            MWCN.CIV.HRS,
                                            MWCN.DATECLOS))
    
    # add column for just month
    baseData$month <- baseData$MWCN.DATECLOS
    baseData$month <- format(baseData$month, "%y-%m")
    
    # count WO close each day
    count <- table(baseData$month)
    count <- as.data.frame(count)
    
    # sum est hours charged for each day
    hrEst <- aggregate(baseData$MWCN.EST.HRS, by = list(Var1 = baseData$month), FUN = sum)
    
    # sum mil hours charged for each day
    hrMil <- aggregate(baseData$MWCN.MIL.HRS, by = list(Var1 = baseData$month), FUN = sum)
    
    # sum civ hours charged for each day
    hrCiv <- aggregate(baseData$MWCN.CIV.HRS, by = list(Var1 = baseData$month), FUN = sum)
    
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
    schedulingCompliance <- gather(schedulingCompliance, Legend, Count, WO_Closed:Civ_Hours, factor_key=TRUE)
    
    # plot data
    ggplot(data = schedulingCompliance, aes(x = Date, y = Count, fill = Legend)) +
      geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
      ggtitle("Scheduling Compliance")
    
  })
  
  output$dataTable <- renderDataTable({
    
    # turn reactive into dataframe
    baseData <- as.data.frame(baseReactive())
    
    # rename columns
    names(baseData)[names(baseData) == "MWOA.WOTITLE"] <- "Title"
    names(baseData)[names(baseData) == "MWOA.FACIDNR"] <- "Fac_Number"
    names(baseData)[names(baseData) == "MWCN.SHOPCODE"] <- "Shop"
    names(baseData)[names(baseData) == "MWOA.SICODE"] <- "Priority"
    names(baseData)[names(baseData) == "MWCN.EST.HRS"] <- "Est_Hrs"
    names(baseData)[names(baseData) == "MWCN.MIL.HRS"] <- "Mil_Hrs"
    names(baseData)[names(baseData) == "MWCN.CIV.HRS"] <- "Civ_Hrs"
    names(baseData)[names(baseData) == "MWCN.DATECLOS"] <- "Date_Closed"
    
    baseData
    
  })
}