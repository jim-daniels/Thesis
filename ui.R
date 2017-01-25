# load packages
library(shiny)
library(ggplot2)

pageWithSidebar(
    headerPanel('Air Force Maintenance and Repair Data Analysis'),
    sidebarPanel(
        helpText('Please select the data you would like to analyze.'),
        selectInput(inputId = 'base', label = 'Base:', choices = c('Dover', 
                                                                   'Dyess', 
                                                                   'Minot', 
                                                                   'Scott')),
        selectInput(inputId = 'shop', label = 'Shop:', choices = c('All', 
                                                                   'Structures', 
                                                                   'Entomology', 
                                                                   'Water Plant', 
                                                                   'Water', 
                                                                   'Electrical', 
                                                                   'Alarms', 
                                                                   'PowerPro', 
                                                                   'HVAC', 
                                                                   'Sweeper', 
                                                                   'Heavy', 
                                                                   'Dorms', 
                                                                   'EMCS', 
                                                                   'ENG')),
        selectInput(inputId = 'priority', label = 'Priority:', choices = c('All',
                                                                           '1: Emergency Corrective Work',
                                                                           '2A: Preventive Maintenance',
                                                                           '2B: Contingency Construction Training',
                                                                           '3A: Sustainment (High)',
                                                                           '3B: Sustainment (Medium)',
                                                                           '3C: Sustainment (Low)',
                                                                           '4A: Enhancement (High)',
                                                                           '4B: Enhancement (Low)')),
        sliderInput("dateRange", "Date WO Closed Range:", min = as.Date("2015-07-01"), 
                    max = as.Date("2016-07-01"), 
                    value = c(as.Date("2015-07-01"), as.Date("2016-07-01")))
    ),
    mainPanel(
        tabsetPanel(id = NULL, 
            tabPanel('Manpower Output', plotOutput(outputId = 'manpowerOutput')),
            tabPanel('Priority Impact', plotOutput(outputId = 'priorityImpact')),
            tabPanel('Facility Impact', plotOutput(outputId = 'facilityImpact')),
            tabPanel('Scheduling Compliance', plotOutput(outputId = 'schedulingCompliance')),
            tabPanel('Data Table', dataTableOutput(outputId = 'dataTable'))
        )
    )
)

#may need to add shinyserver and shinyui back in