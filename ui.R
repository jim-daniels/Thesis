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
                                                                           '4B: Enhancement (Low)'))
    ),
    mainPanel(
        tabsetPanel(id = 'testTable', 
            tabPanel('Test Plot', plotOutput(outputId = 'testPlot')),
            tabPanel('Data Table', DT::dataTableOutput('dataTable'))
        )
    )
)

#may need to add shinyserver and shinyui back in