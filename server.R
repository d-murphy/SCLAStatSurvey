
# redo - put all drop down list is the form: "New Books" = "New_Books", then use string replace to do axis names
# where putting in options, reactive functions will be necessary.  example below. 
# find where functions can overlap - maybe only salary functions need new function.  
# call function with column name and place in different outputs





library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)

LibData <- read_excel("SCLASurvey.xlsx")
LibData <- LibData[1:29,]

shinyServer(function(input, output) {

  LibInfoAxisName <- reactive({
  
    paste0(input$LibInfo)
  
  })
  
  StaffCtAxisName <- reactive({
  
    paste0("StaffCt_",input$StaffCt,
            if(input$StaffCtFT){"_FT"} else {"_PT"}
           )
  
  })

  LoanPerAxisName <- reactive({
    
    paste0("LoanPer_", 
           if(input$LoanPerNew){"New_"} else {"Regular_"},
           input$LoanPer
           )
    
  })  
  
  FineColAxisName <- reactive({
    
    paste0(
      "Fine_", input$FineType, 
      if(input$FinePerson){"_Adult"} else {"_Juvenile"}, 
      if(input$FineMax){"_Max"} else {"_Daily"} 
          )
  })
  
  SalColAxisNameLow <- reactive({
    
    paste0(
      "MinSal_", input$SalInfo, 
      if(input$SalInfo %in% c("Page", "Security Guard"))
        {"_PT"}
      else if(input$SalInfo %in% 
                  c("Library Director", "Assistant Director",
                    "Administrative Assistant","Custodial Worker II",
                    "Custodial Worker III", "Head Custodian",
                    "Circulation Supervisor", "Principal Clerk",
                    "Librarian IV"))
        {"_FT"}
      else if(input$SalInfoFT)
        {"_FT"} 
      else 
        {"_PT"} 
    )
  })
  
  SalColAxisNameHigh <- reactive({
    
    paste0(
      "MaxSal_", input$SalInfo, 
      if(input$SalInfo %in% c("Page", "Security Guard"))
      {"_PT"}
      else if(input$SalInfo %in% 
              c("Library Director", "Assistant Director",
                "Administrative Assistant","Custodial Worker II",
                "Custodial Worker III", "Head Custodian",
                "Circulation Supervisor", "Principal Clerk",
                "Librarian IV"))
      {"_FT"}
      else if(input$SalInfoFT)
      {"_FT"} 
      else 
      {"_PT"} 
    )
  })
  
  
  
  geom_pointPlotFunc <- function(col){
    
    axisTitle <- substring(col, regexpr("_", col)[1]+1) 
    axisTitle <- gsub("_"," ", axisTitle)

    ggplot(aes(x = LibData[[col]], y=LibraryName), data = LibData) +
      geom_point() + 
      labs(x =  axisTitle, 
           y = "Library Name")  +
      theme_hc() + 
      scale_x_continuous(breaks = pretty_breaks())
  }
  
  geom_histPlotFunc <- function(col){
    
    axisTitle <- substring(col, regexpr("_", col)[1]+1) 
    axisTitle <- gsub("_"," ", axisTitle)
    
      ggplot(aes(x = LibData[[col]]), data = LibData) +
        geom_histogram(fill = "dark green", alpha = .5, bins = 15) + 
        labs(x = axisTitle, 
             y = "# of Libraries") +
        theme_hc() + 
        scale_x_continuous(breaks = pretty_breaks())
    
  }

  geom_segPlotFunc <- function(LowSal, HighSal){

    
        
    axisTitle <- substring(LowSal, regexpr("_", LowSal)[1]+1) 
    axisTitle <- gsub("_"," ", axisTitle)
    
      ggplot(aes(x = LibData[[LowSal]], y=LibraryName, xend = LibData[[HighSal]], yend = LibraryName),
           data = LibData) + 
      geom_segment() +
      geom_point(aes(x = LibData[[LowSal]])) + 
      geom_point(aes(x = LibData[[HighSal]])) +
      labs(x = axisTitle, 
           y = "Library Name") + 
      theme_hc()  + 
      ggtitle("") + 
      scale_x_continuous(breaks = pretty_breaks())
    

  }


    output$LibInfoPlot <- renderPlot({
    
      geom_pointPlotFunc(LibInfoAxisName())
      
    }, height = 650)
  
    output$LibInfoHist <- renderPlot({

      geom_histPlotFunc(LibInfoAxisName())

    })

    output$StaffCtPlot <- renderPlot({
      
      geom_pointPlotFunc(StaffCtAxisName())
      
    }, height = 650)
    
  
    output$StaffCtHist <- renderPlot({
      
      geom_histPlotFunc(StaffCtAxisName())
      
    })
    
    output$LoanPerPlot <- renderPlot({
      
      geom_pointPlotFunc(LoanPerAxisName())
      
    }, height = 650)

        
    output$LoanPerHist <- renderPlot({
      
      geom_histPlotFunc(LoanPerAxisName())
      
    })
    
        
    output$FinePlot <- renderPlot({
      
      geom_pointPlotFunc(FineColAxisName())
      
    }, height = 650)
    

    output$FineHist <- renderPlot({
      
      geom_histPlotFunc(FineColAxisName())
      
    })
    
                  
    output$SalInfoPlot <- renderPlot({
    
      geom_segPlotFunc(SalColAxisNameLow(), SalColAxisNameHigh())
      
    }, height = 650)
    
    output$SalInfoHist <- renderPlot({
      
      LibSalHistFunc(input$SalInfo)
      
    })
    
})  
