
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)

LibData <- read_excel("SCLASurvey.xlsx")

shinyServer(function(input, output) {

  output$LibInfoPlot <- renderPlot({

    if(input$LibInfo == "Square Footage"){
    
    ggplot(aes(x = SquareFeet, y=LibraryName), data = (LibData %>% filter(is.na(SquareFeet)==FALSE)
                                                                )
           ) + geom_point() +
      labs(x = "Library Square Footage", 
           y = "Library Name")
      
    } else {
      
      ggplot(aes(x = MeetingRoomCapacity, y=LibraryName), data = (LibData %>% filter(is.na(MeetingRoomCapacity)==FALSE)
      )
      ) + geom_point() +
        labs(x = "Meeting Room Capacity", 
             y = "Library Name")
    
    }
  }, height = 725)

  output$SalInfoPlot <- renderPlot({
    
    if(input$SalInfo == "Librarian III Salary"){
      
      ggplot(aes(x = LibrarianIIISalLow, y=LibraryName, xend = LibrarianIIISalHigh, yend = LibraryName),
             data = (LibData %>% filter(is.na(LibrarianIIISalLow)==FALSE)
      )
      ) + geom_segment() +
        labs(x = "Librarian III Salary", 
             y = "Library Name") + 
        scale_x_continuous(limits = c(40000,120000))
      
    } else {
      
      ggplot(aes(x = LibrarianIIIWageLow, y=LibraryName), data = (LibData %>% filter(is.na(LibrarianIIIWageLow)==FALSE)
      )
      ) + geom_point() +
        labs(x = "Librarian III Wage", 
             y = "Library Name")
      
    }
  }, height = 725)
  
  
  })
