

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
  
    paste0("StaffCt_",input$StaffCt,"_",input$StaffCtFT)
  
  })

  LoanPerAxisName <- reactive({
    
    paste0("LoanPer_", input$LoanPerNew, "_", input$LoanPer )
    
  })  
  
  FineColAxisName <- reactive({
    
    paste0(
      "Fine_", input$FineType,
      "_",input$FinePerson,
      "_",input$FineMax 
          )
  })
  
  SalColAxisName <- reactive({
    
    paste0(input$SalInfoLowHigh, input$SalInfo, 
          if(input$SalInfo %in% c("Page", "Security Guard"))
            {"_PT"}
          else if(input$SalInfo %in% 
                      c("Library Director", "Assistant Director",
                        "Administrative Assistant","Custodial Worker II",
                        "Custodial Worker III", "Head Custodian",
                        "Circulation Supervisor", "Principal Clerk",
                        "Librarian IV"))
            {"_FT"}
          else if(input$SalInfoFT == "FT")
            {"_FT"} 
          else 
          {"_PT"} 
        )
  })
  

  geom_barPlotFunc <- function(col){
    
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
  
  geom_densPlotFunc <- function(col){
    
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

  # Salaries have their own function in order to fix the x axis scale
  # PT is fixed at 50, FT at 175k
  
  geom_barSalPlotFunc <- function(col){
    
    axisTitle <- substring(col, regexpr("_", col)[1]+1) 
    axisTitle <- gsub("_"," ", axisTitle)
    
    ggplot(aes(x = reorder(LibraryName,-LibData[[col]]), y=LibData[[col]]), data = LibData) +
      geom_bar(stat = "identity", fill = "#6bb1ea") + 
      labs(x =  axisTitle, 
           y = "Library Name")  +
      theme_pander() + 
      coord_flip() + 
      geom_text(label = LibData[[col]], hjust=-0.1) +
      scale_y_continuous(limits = c(0,if(substring(col,nchar(col)-1)=="PT"){50}else{175000}),
                         breaks = pretty_breaks(), labels = scales::comma) 
      
  }
  
  geom_densSalPlotFunc <- function(col){
    
    axisTitle <- substring(col, regexpr("_", col)[1]+1) 
    axisTitle <- gsub("_"," ", axisTitle)
    
    ggplot(aes(x = LibData[[col]]), data = LibData) +
      geom_density(fill = "dark green", alpha = .5) + 
      labs(x = axisTitle, 
           y = "# of Libraries") +
      theme_pander() + 
      scale_x_continuous(limits = c(0,if(substring(col,nchar(col)-1)=="PT"){50}else{175000}),
                         breaks = pretty_breaks(), labels = scales::comma) + 
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
    
      geom_barPlotFunc(LibInfoAxisName())
      
    }, height = 650)
  
    output$LibInfoHist <- renderPlot({

      geom_densPlotFunc(LibInfoAxisName())

    })

    output$StaffCtPlot <- renderPlot({
      
      geom_barPlotFunc(StaffCtAxisName())
      
    }, height = 650)
    
  
    output$StaffCtHist <- renderPlot({
      
      geom_densPlotFunc(StaffCtAxisName())
      
    })
    
    output$LoanPerPlot <- renderPlot({
      
      geom_barPlotFunc(LoanPerAxisName())
      
    }, height = 650)

        
    output$LoanPerHist <- renderPlot({
      
      geom_densPlotFunc(LoanPerAxisName())
      
    })
    
        
    output$FinePlot <- renderPlot({
      
      geom_barPlotFunc(FineColAxisName())
      
    }, height = 650)
    

    output$FineHist <- renderPlot({
      
      geom_densPlotFunc(FineColAxisName())
      
    })
    
                  
    output$SalInfoPlot <- renderPlot({
      
      geom_barSalPlotFunc(SalColAxisName())
    

    }, height = 650)
    
    output$SalInfoHist <- renderPlot({
      
      geom_densSalPlotFunc(SalColAxisName())
      

    })
    
})  
