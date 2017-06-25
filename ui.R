
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyUI(
  navbarPage("2015 SCLA Statistical Survey", theme = shinytheme("sandstone"),


    tabPanel("Library Info",
             sidebarLayout(
                sidebarPanel(

                  selectInput("LibInfo", "Select a statistic:", 
                      choices = c(
                                  "Square Footage" = "SquareFeet", 
                                  "Meeting Room Capacity" = "MeetingRoomCapacity", 
                                  "Yearly Open Hours" = "YearlyOpenHours",
                                  "Weekly Hours" = "WeeklyOpenHours"
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

        ),
    tabPanel("About the Project", 
             
             p("The Suffolk County Library Association Statistical Survey Committee is chaired by Peter Ward of the Brentwood Library.  
               This web app was created by Dan Murphy of the West Islip Library.  Some of the data captured by the committee is not
               accessble through this site.  Contact Peter for any additional information.")
             
             )





))
