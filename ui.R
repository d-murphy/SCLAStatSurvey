
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
                                  "Square Footage" = "Info_Square_Feet", 
                                  "Meeting Room Capacity" = "Info_Meeting_Room_Capacity", 
                                  "Yearly Open Hours" = "Info_Yearly_Open_Hours",
                                  "Weekly Hours" = "Info_Weekly_Open_Hours",
                                  "Year Established" = "Info_Year_Established
"
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
                             choices = c("Librarians" = "Librarians",
                                         "Administration" = "Administration",
                                         "Administrative Assistants" = "Administrative_Assistants",
                                         "Building and Grounds" = "Building_and_Grounds",
                                         "Circulation" = "Circulation",
                                         "Clerks" = "Clerks",
                                         "Community Services" = "Community_Services",
                                         "Information Technology" = "Information_Technology",
                                         "Pages" = "Pages", 
                                         "Security" = "Security"
                                         
                             )),
                 checkboxInput("StaffCtFT", "FT or PT", value = TRUE),
                 br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                 plotOutput("StaffCtHist")
               ),
               
               mainPanel(
                 p("Staff Counts"),
                 plotOutput("StaffCtPlot")
               )
             )
             
    ),
    
    tabPanel("Loan Periods", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("LoanPer", "Select a material type:",
                             choices = c("Books - Fiction" = "Books_Fiction",
                                         "Books - Non-fiction" = "Books_Non-Fiction",
                                         "Audiobooks" = "Audiobooks",
                                         "Digital Audiobooks" = "Digital_Audiobooks", 
                                         "DVDs - Fiction" = "DVDs-Fiction",
                                         "DVDs - Non-fiction" = "DVDs_Non-Fiction", 
                                         "Periodicals - Fiction" = "Periodicals_Fiction",
                                         "Periodicals - Non-fiction" = "Periodicals_Non-Fiction", 
                                         "Music" = "Music", 
                                         "Software" = "New_Software", 
                                         "Videogames" = "Videogames" )),
                 checkboxInput("LoanPerNew", "New to collection?"),
                 br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                 plotOutput("LoanPerHist")
               ),
               
               mainPanel(
                 plotOutput("LoanPerPlot")
               )
             )
             
             ),
    tabPanel("Fines", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("FineType", "Select a material type:",
                             choices = c("Books",# = "Books_",
                                         "New Books",# = "NewBooks_", 
                                         "Audiobooks",# = "Audiobooks_",
                                         "DVDs",# = "DVDs_",
                                         "Periodicals",# = "Periodicals_",
                                         "Music",# = "Music_",
                                         "Software",# = "Software_",
                                         "Videogames")),# = "Videogames_")),
                 checkboxInput("FinePerson", "Adult or Juvenile", value = TRUE),
                 checkboxInput("FineMax", "Daily or Max", value = TRUE),
                 br(), br(), br(), br(), br(), br(), br(), br(), br(),  
                 plotOutput("FineHist")
               ),
               
               mainPanel(
                 plotOutput("FinePlot")
               )
             )
             
    ),    
    
            tabPanel("Salary Info",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("SalInfo", "Select a statistic:", 
                             choices = c("Page" = "Page",
                                         "Clerk" = "Clerk",
                                         "Senior Clerk" = "Senior_Clerk",
                                         "Principal Clerk" = "Principal_Clerk",
                                         "Circulation Supervisor" = "Circulation Supervisor",
                                         "Librarian Trainee" = "Library_Trainee",
                                         "Librarian I" = "Librarian_I",
                                         "Librarian II" = "Librarian_II",
                                         "Librarian III" = "Librarian_III",
                                         "Librarian IV" = "Librarian_IV",
                                         "Assistant Director" = "Assistant_Director",
                                         "Library Director" = "Library_Director",
                                         "Administrative Assistant" = "Administrative_Assistant",
                                         "Security Guard" = "Security_Gaurd",
                                         "Custodial Worker I" = "Custodial_Worker_I",
                                         "Custodial Worker II" = "Custodial_Worker_II",
                                         "Custodial Worker III" = "Custodial_Worker_III",
                                         "Head Custodian" = "Head_Custodian"
                                         )),
                 
                 checkboxInput("StaffCtFT", "FT or PT", value = TRUE),
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
