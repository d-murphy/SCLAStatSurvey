
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)

LibData <- read_excel("SCLASurvey.xlsx")

shinyServer(function(input, output) {

  LibInfoPlotFunc <- function(col){
    
    
    xaxisName <- switch(input$LibInfo,
                    "SquareFeet" = "Library Square Footage",
                    "MeetingRoomCapacity" = "Library Meeting Room Capacity",
                    "WeeklyHours" = "Library Weekly Hours")
    
    
    ggplot(aes(x = LibData[[col]], y=LibraryName), 
           data = LibData %>% filter(is.na(col)==FALSE)) +
      
    geom_point() + 
    labs(x = xaxisName, 
         y = "Library Name")  
    
  }
  
  output$LibInfoPlot <- renderPlot({
    
    LibInfoPlotFunc(input$LibInfo)
    
  }, height = 725)

  output$LibInfoDotPlot <- renderPlot({
      
      if(input$LibInfo == "SquareFootage"){
        
        ggplot(aes(x = SquareFeet), data = (LibData %>% filter(is.na(SquareFeet)==FALSE)
        )
        ) + geom_histogram() +
          labs(x = "Library Square Footage", 
               y = "Count")
        
      } else if (input$LibInfo == "MeetingRoomCapacity"){
        
        ggplot(aes(x = MeetingRoomCapacity), data = (LibData %>% filter(is.na(MeetingRoomCapacity)==FALSE)
        )
        ) + geom_histogram() +
          labs(x = "Meeting Room Capacity", 
               y = "Count")
        
      } else if (input$LibInfo == "WeeklyHours"){
        
        ggplot(aes(x = WeeklyHours), data = (LibData %>% filter(is.na(WeeklyHours)==FALSE)
        )
        ) + geom_histogram() +
          labs(x = "Weekly Hours", 
               y = "Count")
        
      }
    })
    
  
    output$SalInfoPlot <- renderPlot({
    
    if(input$SalInfo == "Librarian III Salary"){
      
      ggplot(aes(x = LibrarianIIISalLow, y=LibraryName, xend = LibrarianIIISalHigh, yend = LibraryName),
             data = (LibData %>% filter(is.na(LibrarianIIISalLow)==FALSE)
      )
      ) + geom_segment() +
        labs(x = "Librarian III Salary", 
             y = "Library Name") + 
        scale_x_continuous(limits = c(40000,120000))
      
    } 
  }, height = 725)
  
  
  })
