
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

                  selectInput("LibInfo", "Select a statistic:", 
                      choices = c("Square Footage", "Meeting Room Capacity"))
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
                             choices = c("Librarian III Salary", "Librarian III Wage"))
               ),
               
               mainPanel(
                 plotOutput("SalInfoPlot")
               )
             )

        )





))
