
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  navbarPage("2012 SCLA Statistical Survey",


    tabPanel("Library Info",
             sidebarLayout(
                sidebarPanel(

                  #### left off here - need to get abstract function
                  
                  selectInput("LibInfo", "Select a statistic:", 
                      choices = c("Square Footage" = "SquareFeet", 
                                  "Meeting Room Capacity" = "MeetingRoomCapacity", 
                                  "Weekly Hours" = "WeeklyHours"
                                  )), 
                  
                  br(), br(), br(), br(), br(),br(),br(),br(),br(),
                  plotOutput("LibInfoHist")
                          ),

                mainPanel(
                  plotOutput("LibInfoPlot")
                  )
              )
        ),
    tabPanel("Salary Info",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("SalInfo", "Select a statistic:", 
                             choices = c("Librarian Trainee Salary" = "LibTraineeSal",
                                         "Librarian I Salary" = "LibISal",
                                         "Librarian III Salary" = "LibIIISal")),
                 br(), br(), br(), br(), br(), br(), br(), br(), br(),
                 plotOutput("SalInfoHist")
               ),
               
               mainPanel(
                 plotOutput("SalInfoPlot")
               )
             )

        )





))
