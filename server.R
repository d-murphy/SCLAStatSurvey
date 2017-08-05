

library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)

LibData <- read_excel("SCLASurvey.xlsx")
LibData <- LibData[1:29,]
#LibData <- LibData %>% arrange(desc(LibraryName))

shinyServer(function(input, output) {


  # First sent of functions takes UI input and converts to proper column name  
  
  
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
  
  # 4 functions needed.  2 geom_points (1 for single points and 1 for range) and 2 geom_hists(same justification)
  
  geom_pointPlotFunc <- function(col){
    
    axisTitle <- substring(col, regexpr("_", col)[1]+1) 
    axisTitle <- gsub("_"," ", axisTitle)

    ggplot(aes(x = reorder(LibraryName,-LibData[[col]]), y=LibData[[col]]), data = LibData) +
      geom_bar(stat = "identity", fill = "#6bb1ea") + 
      labs(x =  axisTitle, 
           y = "Library Name")  +
      theme_pander() + 
      coord_flip() + 
      geom_text(label = LibData[[col]], hjust=-0.1) +
      scale_y_continuous(limits = c(0,NA),breaks = pretty_breaks(), labels = scales::comma)
  }
  
  geom_histPlotFunc <- function(col){
    
    axisTitle <- substring(col, regexpr("_", col)[1]+1) 
    axisTitle <- gsub("_"," ", axisTitle)
    
      ggplot(aes(x = LibData[[col]]), data = LibData) +
        geom_density(fill = "dark green", alpha = .5) + 
        labs(x = axisTitle, 
             y = "# of Libraries") +
        theme_pander() + 
        scale_x_continuous(limits = c(0,NA), breaks = pretty_breaks(), labels = scales::comma) + 
        theme(axis.title.y=element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
    
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
      theme_pander()  + 
      ggtitle("") + 
      scale_x_continuous(limits = c(0,NA),breaks = pretty_breaks(), labels = scales::comma)
    

  }

  geom_histRangePlotFunc <- function(LowSal, HighSal){
    
    axisTitle <- substring(LowSal, regexpr("_", LowSal)[1]+1) 
    axisTitle <- gsub("_"," ", axisTitle)
    
    ggplot(data = LibData) +
      geom_density(aes(x = LibData[[LowSal]]), fill = "dark red", alpha = .5) + 
      geom_density(aes(x = LibData[[HighSal]]), fill = "dark green", alpha = .5) +
      labs(x = axisTitle, 
           y = "# of Libraries") +
      theme_pander() + 
      scale_x_continuous(limits = c(0,NA), breaks = pretty_breaks(), labels = scales::comma)+ 
      theme(axis.title.y=element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
  }
  
    # Lib Info tab needs reactive graph titles.  Other tabs use Axis Name.  
  
    output$LibInfoTitle <- renderText({
      
      chartTitle <- LibInfoAxisName()
      chartTitle <- substring(chartTitle, regexpr("_", chartTitle)[1]+1) 
      chartTitle <- gsub("_"," ", chartTitle)
      chartTitle
      })

    # call column name with correct geom function to create outputs
    
        
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
      
      geom_histRangePlotFunc(SalColAxisNameLow(), SalColAxisNameHigh())
      
    })
    
})  
