
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
                                  "Weekly Hours" = "WeeklyOpenHours",
                                  "Year Established" = "YearEstablished"
                                  )), 
                  
                  br(), br(), br(), br(), br(),br(),br(),br(),br(),
                  plotOutput("LibInfoHist")
                          ),

                mainPanel(
                  plotOutput("LibInfoPlot")
                  )
              )
        ),

    tabPanel("Staff Counts",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("StaffCt", "Select a role:", 
                             choices = c("Librarian FT" = "Librarians_FT",
                                         "Librarian PT" = "Librarians_PT",
                                         "Administration FT" = "Administration_FT",
                                         "Administration PT" = "Administration _PT",
                                         "Admin Assistant FT" = "AdministrativeAssistants_FT",
                                         "Admin Assistant PT" = "AdministrativeAssistants_PT",
                                         "Building and Grounds FT" = "BuildingGrounds_FT",
                                         "Building and Grounds PT" = "BuildingGrounds_PT",
                                         "Circulation FT" = "Circulation_FT",
                                         "Circulation PT" = "Circulation_PT",
                                         "Clerks FT" = "Clerks_FT",
                                         "Clerks PT" = "Clerks_PT",
                                         "Community Services FT" = "CommunityServices_FT",
                                         "Community Services PT" = "CommunityServices_PT",
                                         "IT FT" = "InformationTechnology_FT",
                                         "IT PT" = "InformationTechnology_PT",
                                         "Pages FT" = "Pages_FT",
                                         "Pages PT" = "Pages_PT",
                                         "Security FT" = "Security_FT",
                                         "Security PT" = "Security_PT"
                                         
                             )),
                 br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                 plotOutput("StaffCtHist")
               ),
               
               mainPanel(
                 plotOutput("StaffCtPlot")
               )
             )
             
    ),
    
    tabPanel("Loan Periods", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("LoanPer", "Select a material type:",
                             choices = c("New Books - Fiction" = "New_Books_Fiction",
                                         "New Books - Non-fiction" = "New_Books_Non-Fiction",
                                         "Regular Books - Fiction" = "Regular_Books_Fiction",
                                         "Regular Books - Non-fiction" = "Regular_Books_Non-Fiction", 
                                         "New Audiobooks - Fiction" = "New_Audiobooks_Fiction",
                                         "New Audiobooks - Non-fiction" = "New_Audiobooks_Non-Fiction",
                                         "Regular Audiobooks - Fiction" = "Regular_Audiobook_Fiction",
                                         "Regular Audiobooks - Non-fiction" = "Regular_Audiobooks_Non-Fiction",
                                         "New Digital Audio Books" = "New_Digital_Audio_Books_Fiction",
                                         "Regular Digital Audio Books" = "Regular_Digital_Audio_Books_Fiction", 
                                         "New DVDs - Fiction" = "New_DVDs-Fiction",
                                         "New DVDs - Non-fiction" = "New_DVDs_Non-Fiction",
                                         "Regular DVDs - Fiction" = "Regular_DVDs_Fiction", 
                                         "Regular DVDs - Non-fiction" = "Regular_DVDs_Non-Fiction", 
                                         "New Periodicals - Fiction" = "New_Periodicals_Fiction",
                                         "New Periodicals - Non-fiction" = "New_Periodicals_Non-Fiction", 
                                         "Regular Periodicals - Fiction" = "Regular_Periodicals_Fiction", 
                                         "Regular Periodicals - Non-fiction" = "Regular_Periodicals_Non-Fiction",
                                         "New Music" = "New_Music_Fiction", 
                                         "Regular Music" = "Regular_Music_Fiction",
                                         "New Software" = "New_Software_Fiction", 
                                         "Regular Software" = "Regular_Software_Fiction", 
                                         "New Videogames" = "New_Videogames_Fiction", 
                                         "Regular Videogames" = "Regular_Videogames_Fiction")),
                 br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                 plotOutput("LoanPerHist")
               ),
               
               mainPanel(
                 plotOutput("LoanPerPlot")
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
