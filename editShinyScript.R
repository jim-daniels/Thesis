library(shiny)
library(ggplot2)

# load files (change file location based on dataset)
MWCN <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWCN", 
                   header = TRUE, 
                   sep = "|",
                   na.strings = c(" ", # changes spaces etc. to NA
                                  "  ",
                                  "   ",
                                  "    ",
                                  "            ",
                                  "               ",
                                  "                    ",
                                  "0"
                   )
)
MWOA <- read.delim(file = "C:/Users/jimda/OneDrive/Documents/R/Thesis/Data/Dover FJXT/JMWOA", 
                   header = TRUE, 
                   sep = "|",
                   na.strings = c(" ", # changes spaces etc. to NA
                                  "  ",
                                  "   ",
                                  "    ",
                                  "            ",
                                  "               ",
                                  "                    ",
                                  "0"
                   )
)

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

ui <- 
  
  pageWithSidebar(
    headerPanel('Test Data Analysis'),
    sidebarPanel(
      fileInput(inputId = 'basedata', label = 'Load File: (Under Construction)', multiple = TRUE),
      helpText('File input has the potential for each base to upload their own data for analysis'),
      selectInput(inputId = 'shop', label = 'Shop:', choices = c('All', 'Structures', 'Entomology', 'Water Plant', 'Water', 'Electrical', 'Alarms', 'PowerPro', 'HVAC', 'Sweeper', 'Heavy', 'Dorms', 'EMCS', 'ENG')),
      selectInput(inputId = 'priority', label = 'Priority:', choices = c('All', 
                                                                         '1: Emergency Corrective Work',
                                                                         '2A: Preventive Maintenance', 
                                                                         '2B: Contingency Construction Training', 
                                                                         '3A: Sustainment (High)', 
                                                                         '3B: Sustainment (Medium)', 
                                                                         '3C: Sustainment (Low)', 
                                                                         '4A: Enhancement (High)', 
                                                                         '4B: Enhancement (Low)')),
      sliderInput(inputId = 'daysOpenSlider', label = 'WO Days Open Cutoff', min = 0, max = 1000, value = 365), 
      textOutput(outputId = 'meanClosed')
    ),
    mainPanel(
      plotOutput(outputId = 'daysOpenVsHoursCharged'),
      plotOutput(outputId = 'daysOpenAfterMaterialVsHoursCharged')
    )
  )

server <- function(input, output) {
  
  output$daysOpenVsHoursCharged <- renderPlot({
    # subset based on shop dropdown 
    daysOpenVsHoursChargedData <- if(input$shop == 'Structures') {subset(Dover, Dover$MWCN.SHOPCODE == 'CA')
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
    daysOpenVsHoursChargedData <- if(input$priority == '1: Emergency Corrective Work') {subset(daysOpenVsHoursChargedData, daysOpenVsHoursChargedData$MWOA.SICODE == '1 ')
    } else if (input$priority == '2A: Preventive Maintenance') {subset(daysOpenVsHoursChargedData, daysOpenVsHoursChargedData$MWOA.SICODE == '2A')
    } else if (input$priority == '2B: Contingency Construction Training') {subset(daysOpenVsHoursChargedData, daysOpenVsHoursChargedData$MWOA.SICODE == '2B')
    } else if (input$priority == '3A: Sustainment (High)') {subset(daysOpenVsHoursChargedData, daysOpenVsHoursChargedData$MWOA.SICODE == '3A')
    } else if (input$priority == '3B: Sustainment (Medium)') {subset(daysOpenVsHoursChargedData, daysOpenVsHoursChargedData$MWOA.SICODE == '3B')
    } else if (input$priority == '3C: Sustainment (Low)') {subset(daysOpenVsHoursChargedData, daysOpenVsHoursChargedData$MWOA.SICODE == '3C')
    } else if (input$priority == '4A: Enhancement (High)') {subset(daysOpenVsHoursChargedData, daysOpenVsHoursChargedData$MWOA.SICODE == '4A')
    } else if (input$priority == '4B: Enhancement (Low)') {subset(daysOpenVsHoursChargedData, daysOpenVsHoursChargedData$MWOA.SICODE == '4B')
    } else daysOpenVsHoursChargedData
    # create new column for days open
    daysOpenVsHoursChargedData$daysOpen <- as.numeric(daysOpenVsHoursChargedData$MWOA.DATECLOS - daysOpenVsHoursChargedData$MWOA.DATEOPEN)
    # get rid of wo open less than 0 days
    daysOpenVsHoursChargedData <- subset(daysOpenVsHoursChargedData, daysOpenVsHoursChargedData$daysOpen >= 0) 
    # get rid of wo open more than 3 years
    daysOpenVsHoursChargedData <- 
      subset(daysOpenVsHoursChargedData, 
             daysOpenVsHoursChargedData$MWOA.DATECLOS - daysOpenVsHoursChargedData$MWOA.DATEOPEN <= input$daysOpenSlider)
    # prevent errors with no data
    if(nrow(daysOpenVsHoursChargedData) <= 2) {ggplot(NULL, aes(0, 0)) + ggtitle('No Data')
    } else ggplot(daysOpenVsHoursChargedData, aes(daysOpen, MWOA.TOTHRS)) +
      geom_point() +
      geom_smooth(method = lm) +
      ggtitle("WO Days Open vs Hours Charged") +
      labs(x = "Days Open", y = "Hours Charged")
  })
  output$daysOpenAfterMaterialVsHoursCharged <- renderPlot({
    # subset based on shop dropdown 
    daysOpenAfterMaterialVsHoursCharged <- if(input$shop == 'Structures') {subset(Dover, Dover$MWCN.SHOPCODE == 'CA')
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
    daysOpenAfterMaterialVsHoursCharged <- if(input$priority == '1: Emergency Corrective Work') {subset(daysOpenAfterMaterialVsHoursCharged, daysOpenAfterMaterialVsHoursCharged$MWOA.SICODE == '1 ')
    } else if (input$priority == '2A: Preventive Maintenance') {subset(daysOpenAfterMaterialVsHoursCharged, daysOpenAfterMaterialVsHoursCharged$MWOA.SICODE == '2A')
    } else if (input$priority == '2B: Contingency Construction Training') {subset(daysOpenAfterMaterialVsHoursCharged, daysOpenAfterMaterialVsHoursCharged$MWOA.SICODE == '2B')
    } else if (input$priority == '3A: Sustainment (High)') {subset(daysOpenAfterMaterialVsHoursCharged, daysOpenAfterMaterialVsHoursCharged$MWOA.SICODE == '3A')
    } else if (input$priority == '3B: Sustainment (Medium)') {subset(daysOpenAfterMaterialVsHoursCharged, daysOpenAfterMaterialVsHoursCharged$MWOA.SICODE == '3B')
    } else if (input$priority == '3C: Sustainment (Low)') {subset(daysOpenAfterMaterialVsHoursCharged, daysOpenAfterMaterialVsHoursCharged$MWOA.SICODE == '3C')
    } else if (input$priority == '4A: Enhancement (High)') {subset(daysOpenAfterMaterialVsHoursCharged, daysOpenAfterMaterialVsHoursCharged$MWOA.SICODE == '4A')
    } else if (input$priority == '4B: Enhancement (Low)') {subset(daysOpenAfterMaterialVsHoursCharged, daysOpenAfterMaterialVsHoursCharged$MWOA.SICODE == '4B')
    } else daysOpenAfterMaterialVsHoursCharged
    # create new column for days open
    daysOpenAfterMaterialVsHoursCharged$daysOpenAfterMaterial <- as.numeric(daysOpenAfterMaterialVsHoursCharged$MWOA.DATECLOS - daysOpenAfterMaterialVsHoursCharged$MWOA.DATEMATL)
    # get rid of wo open less than 0 days
    daysOpenAfterMaterialVsHoursCharged <- subset(daysOpenAfterMaterialVsHoursCharged, daysOpenAfterMaterialVsHoursCharged$daysOpenAfterMaterial >= 0) 
    # get rid of wo open more than 3 years
    daysOpenAfterMaterialVsHoursCharged <- 
      subset(daysOpenAfterMaterialVsHoursCharged, 
             daysOpenAfterMaterialVsHoursCharged$MWOA.DATECLOS - daysOpenAfterMaterialVsHoursCharged$MWOA.DATEMATL <= input$daysOpenSlider)
    # prevent errors with no data
    if(nrow(daysOpenAfterMaterialVsHoursCharged) <= 2) {ggplot(NULL, aes(0, 0)) + ggtitle('No Data')
    } else ggplot(daysOpenAfterMaterialVsHoursCharged, aes(daysOpenAfterMaterial, MWOA.TOTHRS)) +
      geom_point() +
      geom_smooth(method = lm) +
      ggtitle("WO Days Open After Material vs Hours Charged") +
      labs(x = "Days Open After Material", y = "Hours Charged") #+
    # annotate("text", 
    # x = max(daysOpenAfterMaterialVsHoursCharged$daysOpen), 
    # y = max(daysOpenAfterMaterialVsHoursCharged$MWOA.TOTHRS), 
    # label = "Insert correlation coefficient here", 
    # hjust = 1)
  })
  output$meanClosed <- renderText({
     mean(Dover$MWOA.TOTHRS)
  })
}

shinyApp(ui = ui, server = server)