

library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)

LibData <- read_excel("SCLASurvey.xlsx")
LibData <- LibData[1:29,]

shinyServer(function(input, output) {

  LibInfoPlotFunc <- function(col){
    
    
    LibInfoxAxisName <- switch(input$LibInfo,
                               "SquareFeet" = "Square Footage", 
                               "MeetingRoomCapacity" = "Meeting Room Capacity", 
                               "YearlyOpenHours" = "Yearly Open Hours",
                               "WeeklyOpenHours" = "Weekly Hours", 
                               "YearEstablished" = "YearEstablished")
                    
    ggplot(aes(x = LibData[[col]], y=LibraryName), data = LibData) +
    geom_point() + 
    labs(x =  LibInfoxAxisName, 
         y = "Library Name")  +
    theme_hc() + 
    scale_x_continuous(breaks = pretty_breaks())
  }

  LibInfoHistFunc <- function(col){
    
    LibInfoxAxisName <- switch(input$LibInfo,
                               "SquareFeet" = "Square Footage", 
                               "MeetingRoomCapacity" = "Meeting Room Capacity", 
                               "YearlyOpenHours" = "Yearly Open Hours",
                               "WeeklyOpenHours" = "Weekly Hours", 
                               "YearEstablished" = "YearEstablished")
    ggplot(aes(x = LibData[[col]]), data = LibData) +
      geom_histogram(fill = "dark green", alpha = .5, bins = 15) + 
      labs(x = LibInfoxAxisName, 
           y = "# of Libraries") +
      theme_hc() + 
      scale_x_continuous(breaks = pretty_breaks())
  }

  StaffCtPlotFunc <- function(col){
    
    
    StaffCtAxisName <- switch(input$StaffCt,
                              "Librarians_FT" = "Librarian FT",
                              "Librarians_PT" = "Librarian PT",
                              "Administration_FT" = "Administration FT",
                              "Administration _PT" = "Administration PT",
                              "AdministrativeAssistants_FT" = "Admin Assistant FT",
                              "AdministrativeAssistants_PT" = "Admin Assistant PT",
                              "BuildingGrounds_FT" = "Building and Grounds FT",
                              "BuildingGrounds_PT" = "Building and Grounds PT",
                              "Circulation_FT" = "Circulation FT",
                              "Circulation_PT" = "Circulation PT",
                              "Clerks_FT" = "Clerks FT",
                              "Clerks_PT" = "Clerks PT",
                              "CommunityServices_FT" = "Community Services FT",
                              "CommunityServices_PT" = "Community Services PT",
                              "InformationTechnology_FT" = "IT FT",
                              "InformationTechnology_PT" = "IT PT",
                              "Pages_FT" = "Pages FT",
                              "Pages_PT" = "Pages PT",
                              "Security_FT" = "Security FT",
                              "Security_PT" = "Security PT" 
                              )
    
    ggplot(aes(x = LibData[[col]], y=LibraryName), data = LibData) +
      geom_point() + 
      labs(x =  StaffCtAxisName, 
           y = "Library Name")  +
      theme_hc() + 
      scale_x_continuous(breaks = pretty_breaks())
  }
  
  StaffCtHistFunc <- function(col){
    
    StaffCtAxisName <- switch(input$LibInfo,
                              "Librarians_FT" = "Librarian FT",
                              "Librarians_PT" = "Librarian PT",
                              "Administration_FT" = "Administration FT",
                              "Administration _PT" = "Administration PT",
                              "AdministrativeAssistants_FT" = "Admin Assistant FT",
                              "AdministrativeAssistants_PT" = "Admin Assistant PT",
                              "BuildingGrounds_FT" = "Building and Grounds FT",
                              "BuildingGrounds_PT" = "Building and Grounds PT",
                              "Circulation_FT" = "Circulation FT",
                              "Circulation_PT" = "Circulation PT",
                              "Clerks_FT" = "Clerks FT",
                              "Clerks_PT" = "Clerks PT",
                              "CommunityServices_FT" = "Community Services FT",
                              "CommunityServices_PT" = "Community Services PT",
                              "InformationTechnology_FT" = "IT FT",
                              "InformationTechnology_PT" = "IT PT",
                              "Pages_FT" = "Pages FT",
                              "Pages_PT" = "Pages PT",
                              "Security_FT" = "Security FT",
                              "Security_PT" = "Security PT" 
    )
    ggplot(aes(x = LibData[[col]]), data = LibData) +
      geom_histogram(fill = "dark green", alpha = .5, bins = 15) + 
      labs(x = StaffCtAxisName, 
           y = "# of Libraries") +
      theme_hc() + 
      scale_x_continuous(breaks = pretty_breaks())
  }
  
  LoanPerPlotFunc <- function(col){
    
    
    LoanPerAxisName <- switch(input$LoanPer,
                              "New_Books_Fiction" = "New Books - Fiction",
                              "New_Books_Non-Fiction" = "New Books - Non-fiction",
                              "Regular_Books_Fiction" = "Regular Books - Fiction",
                              "Regular_Books_Non-Fiction" = "Regular Books - Non-fiction", 
                              "New_Audiobooks_Fiction" = "New Audiobooks - Fiction",
                              "New_Audiobooks_Non-Fiction" = "New Audiobooks - Non-fiction",
                              "Regular_Audiobook_Fiction" = "Regular Audiobooks - Fiction",
                              "Regular_Audiobooks_Non-Fiction" = "Regular Audiobooks - Non-fiction",
                              "New_Digital_Audio_Books_Fiction" = "New Digital Audio Books",
                              "Regular_Digital_Audio_Books_Fiction" = "Regular Digital Audio Books", 
                              "New_DVDs-Fiction" = "New DVDs - Fiction",
                              "New_DVDs_Non-Fiction" = "New DVDs - Non-fiction",
                              "Regular_DVDs_Fiction" = "Regular DVDs - Fiction", 
                              "Regular_DVDs_Non-Fiction" = "Regular DVDs - Non-fiction", 
                              "New_Periodicals_Fiction" = "New Periodicals - Fiction",
                              "New_Periodicals_Non-Fiction" = "New Periodicals - Non-fiction", 
                              "Regular_Periodicals_Fiction" = "Regular Periodicals - Fiction", 
                              "Regular_Periodicals_Non-Fiction" = "Regular Periodicals - Non-fiction",
                              "New_Music_Fiction" = "New Music", 
                              "Regular_Music_Fiction" = "Regular Music",
                              "New_Software_Fiction" = "New Software", 
                              "Regular_Software_Fiction" = "Regular Software", 
                              "New_Videogames_Fiction" = "New Videogames", 
                              "Regular_Videogames_Fiction" = "Regular Videogames"
                              
    )
    
    ggplot(aes(x = LibData[[col]], y=LibraryName), data = LibData) +
      geom_point() + 
      labs(x =  LoanPerAxisName, 
           y = "Library Name")  +
      theme_hc() + 
      scale_x_continuous(breaks = pretty_breaks())
  }
  
  LoanPerHistFunc <- function(col){
    
    LoanPerAxisName <- switch(input$LoanPer,
                              "New_Books_Fiction" = "New Books - Fiction",
                              "New_Books_Non-Fiction" = "New Books - Non-fiction",
                              "Regular_Books_Fiction" = "Regular Books - Fiction",
                              "Regular_Books_Non-Fiction" = "Regular Books - Non-fiction", 
                              "New_Audiobooks_Fiction" = "New Audiobooks - Fiction",
                              "New_Audiobooks_Non-Fiction" = "New Audiobooks - Non-fiction",
                              "Regular_Audiobook_Fiction" = "Regular Audiobooks - Fiction",
                              "Regular_Audiobooks_Non-Fiction" = "Regular Audiobooks - Non-fiction",
                              "New_Digital_Audio_Books_Fiction" = "New Digital Audio Books",
                              "Regular_Digital_Audio_Books_Fiction" = "Regular Digital Audio Books", 
                              "New_DVDs-Fiction" = "New DVDs - Fiction",
                              "New_DVDs_Non-Fiction" = "New DVDs - Non-fiction",
                              "Regular_DVDs_Fiction" = "Regular DVDs - Fiction", 
                              "Regular_DVDs_Non-Fiction" = "Regular DVDs - Non-fiction", 
                              "New_Periodicals_Fiction" = "New Periodicals - Fiction",
                              "New_Periodicals_Non-Fiction" = "New Periodicals - Non-fiction", 
                              "Regular_Periodicals_Fiction" = "Regular Periodicals - Fiction", 
                              "Regular_Periodicals_Non-Fiction" = "Regular Periodicals - Non-fiction",
                              "New_Music_Fiction" = "New Music", 
                              "Regular_Music_Fiction" = "Regular Music",
                              "New_Software_Fiction" = "New Software", 
                              "Regular_Software_Fiction" = "Regular Software", 
                              "New_Videogames_Fiction" = "New Videogames", 
                              "Regular_Videogames_Fiction" = "Regular Videogames"
                              
    )
    
    ggplot(aes(x = LibData[[col]]), data = LibData) +
      geom_histogram(fill = "dark green", alpha = .5, bins = 15) + 
      labs(x = LoanPerAxisName, 
           y = "# of Libraries") +
      theme_hc() + 
      scale_x_continuous(breaks = pretty_breaks())
  }
  
  
  LibSalPlotFunc <- function(col){

    LibSalxAxisName <- switch(input$SalInfo, 
                        "LibIIISal" = "Librarian III Salary", 
                        "LibISal" = "Librarian I Salary",
                        "LibTraineeSal" = "Librarian Trainee Salary",
                        "LibIISal" = "Librarian II Salary",
                        "LibIVSal" = "Librarian IV Salary",
                        "ClerkSal" = "Cleark Salary",
                        "SenClerkSal" = "Senior Clerk Salary",
                        "LibDirSal" = "Library Director Salary")
   
    
   
    LibSalLow <- paste0(input$SalInfo,"Low")
    LibSalHigh <- paste0(input$SalInfo, "High")
    
    ggplot(aes(x = LibData[[LibSalLow]], y=LibraryName, xend = LibData[[LibSalHigh]], yend = LibraryName),
           data = LibData) + 
      geom_segment() +
      geom_point(aes(x = LibData[[LibSalLow]])) + 
      geom_point(aes(x = LibData[[LibSalHigh]])) +
      labs(x = LibSalxAxisName, 
           y = "Library Name") + 
      scale_x_continuous(limits = c(20000,160000))+
      theme_hc()  + 
      ggtitle("") + 
      scale_x_continuous(breaks = pretty_breaks())
  }
  
  LibSalHistFunc <- function(col){
    
    LibSalxAxisName <- switch(input$SalInfo, 
                              "LibIIISal" = "Librarian III Salary", 
                              "LibISal" = "Librarian I Salary",
                              "LibTraineeSal" = "Librarian Trainee Salary",
                              "LibIISal" = "Librarian II Salary",
                              "LibIVSal" = "Librarian IV Salary",
                              "ClerkSal" = "Cleark Salary",
                              "SenClerkSal" = "Senior Clerk Salary",
                              "LibDirSal" = "Library Director Salary")
    

    LibSalLow <- paste0(input$SalInfo,"Low")
    LibSalHigh <- paste0(input$SalInfo, "High")
    
    ggplot(data = LibData) +
      geom_histogram(aes(x = LibData[[LibSalLow]]) , fill = "dark red", alpha = .5, bins = 15) +
      geom_histogram(aes(x = LibData[[LibSalHigh]]), fill = "dark green", alpha = .5, bins = 15) + 
      labs(x = LibSalxAxisName, 
           y = "# of Libraries")  + 
      scale_x_continuous(limits = c(20000,160000)) +
      theme_hc() 
  }
  
    output$LibInfoPlot <- renderPlot({
    
    LibInfoPlotFunc(input$LibInfo)
    
    }, height = 650)
  
    output$LibInfoHist <- renderPlot({

      LibInfoHistFunc(input$LibInfo)
            
    })

    output$StaffCtPlot <- renderPlot({
      
      StaffCtPlotFunc(input$StaffCt)
      
    }, height = 650)
    
  
    output$StaffCtHist <- renderPlot({
      
      StaffCtHistFunc(input$StaffCt)
      
    })
    
    output$LoanPerHist <- renderPlot({
      
      LoanPerHistFunc(input$LoanPer)
      
    })
    
    output$LoanPerPlot <- renderPlot({
      
      LoanPerPlotFunc(input$LoanPer)
      
    }, height = 650)
    
    
                  
    output$SalInfoPlot <- renderPlot({
    
      LibSalPlotFunc(input$SalInfo)
      
    }, height = 650)
    
    output$SalInfoHist <- renderPlot({
      
      LibSalHistFunc(input$SalInfo)
      
    })
    
})  
