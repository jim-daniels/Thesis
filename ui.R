library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel('Test Data Analysis'),

  sidebarPanel(
    
    fileInput(inputId = 'basedata', label = 'Load File: (Under Construction)', multiple = TRUE),
    
    helpText('File input has the potential for each base to upload their own data for analysis'),
    
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
    
    sliderInput(inputId = 'daysOpenSlider', label = 'WO Days Open Cutoff', min = 0, max = 1000, value = 365), 
    
    textOutput(outputId = 'meanClosed')
  ),
  
  mainPanel(
    plotOutput(outputId = 'daysOpenVsHoursCharged'),
    plotOutput(outputId = 'daysOpenAfterMaterialVsHoursCharged'),
    plotOutput(outputId = 'woClosedPerDay')
  )
))

