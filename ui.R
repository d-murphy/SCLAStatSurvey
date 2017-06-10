
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyUI(
  navbarPage("2012 SCLA Statistical Survey", theme = shinytheme("sandstone"),


    tabPanel("Library Info",
             sidebarLayout(
                sidebarPanel(

                  #### left off here - need to get abstract function
                  
                  selectInput("LibInfo", "Select a statistic:", 
                      choices = c("Square Footage" = "SquareFeet", 
                                  "Meeting Room Capacity" = "MeetingRoomCapacity", 
                                  "Weekly Hours" = "WeeklyHours",
                                  "New Fiction Loan Period" = "NewFictionLoan",
                                  "New Non-Fiction Loan Period" = "NewNonFicLoan",
                                  "Other Books Loan Period" = "OtherBooksLoan"
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
                             choices = c("Clerk Salary" = "ClerkSal",
                                         "Senior Clerk Salary" = "SenClerkSal",
                                         "Librarian Trainee Salary" = "LibTraineeSal",
                                         "Librarian I Salary" = "LibISal",
                                         "Librarian II Salary" = "LibIISal",
                                         "Librarian III Salary" = "LibIIISal",
                                         "Librarian IV Salary" = "LibIVSal",
                                         "Library Director Salary" = "LibDirSal"
                                         )),
                 br(), br(), br(), br(), br(), br(), br(), br(),  
                 p("Reds used to collect the lower ends of each range.  Greens collect the highs."),
                 plotOutput("SalInfoHist")
               ),
               
               mainPanel(
                 plotOutput("SalInfoPlot")
               )
             )

        )





))
