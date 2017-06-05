

library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)

LibData <- read_excel("SCLASurvey.xlsx")

shinyServer(function(input, output) {

  LibInfoPlotFunc <- function(col){
    
    
    LibInfoxAxisName <- switch(input$LibInfo,
                    "SquareFeet" = "Library Square Footage",
                    "MeetingRoomCapacity" = "Library Meeting Room Capacity",
                    "WeeklyHours" = "Library Weekly Hours")

    ggplot(aes(x = LibData[[col]], y=LibraryName), data = LibData) +
    geom_point() + 
    labs(x = LibInfoxAxisName, 
         y = "Library Name")  +
    theme_hc()
  }

  LibInfoHistFunc <- function(col){
    
    LibInfoxAxisName <- switch(input$LibInfo,
                               "SquareFeet" = "Library Square Footage",
                               "MeetingRoomCapacity" = "Library Meeting Room Capacity",
                               "WeeklyHours" = "Library Weekly Hours")
    ggplot(aes(x = LibData[[col]]), data = LibData) +
      geom_histogram(fill = "dark green", alpha = .5) + 
      labs(x = LibInfoxAxisName, 
           y = "# of Libraries") +
      theme_hc() 
  }
  
  LibSalPlotFunc <- function(col){

    LibSalxAxisName <- switch(input$SalInfo, 
                        "LibIIISal" = "Librarian III Salary", 
                        "LibISal" = "Librarian I Salary",
                        "LibTraineeSal" = "Librarian Trainee Salary")

    LibSalLow <- paste0(input$SalInfo,"Low")
    LibSalHigh <- paste0(input$SalInfo, "High")
    
    ggplot(aes(x = LibData[[LibSalLow]], y=LibraryName, xend = LibData[[LibSalHigh]], yend = LibraryName),
           data = LibData) + 
      geom_segment() +
      geom_point(aes(x = LibData[[LibSalLow]])) + 
      geom_point(aes(x = LibData[[LibSalHigh]])) +
      labs(x = LibSalxAxisName, 
           y = "Library Name") + 
      scale_x_continuous(limits = c(40000,160000))+
      theme_hc()
  }
  
  LibSalHistFunc <- function(col){
    
    LibSalxAxisName <- switch(input$SalInfo, 
                              "LibIIISal" = "Librarian III Salary", 
                              "LibISal" = "Librarian I Salary",
                              "LibTraineeSal" = "Librarian Trainee Salary")

    LibSalLow <- paste0(input$SalInfo,"Low")
    LibSalHigh <- paste0(input$SalInfo, "High")
    
    ggplot(data = LibData) +
      geom_histogram(aes(x = LibData[[LibSalLow]]) , fill = "dark red", alpha = .5) +
      geom_histogram(aes(x = LibData[[LibSalHigh]]), fill = "dark green", alpha = .5) + 
      labs(x = LibSalxAxisName, 
           y = "# of Libraries")  + 
      scale_x_continuous(limits = c(40000,160000)) +
      theme_hc()
  }
  
    
    output$LibInfoPlot <- renderPlot({
    
    LibInfoPlotFunc(input$LibInfo)
    
    }, height = 725)
  
    output$LibInfoHist <- renderPlot({

      LibInfoHistFunc(input$LibInfo)
            
    })
    
    output$SalInfoPlot <- renderPlot({
    
      LibSalPlotFunc(input$SalInfo)
      
    }, height = 725)
    
    output$SalInfoHist <- renderPlot({
      
      LibSalHistFunc(input$SalInfo)
      
    })
    
  
  })
