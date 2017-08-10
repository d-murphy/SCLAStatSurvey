

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
                                  "Year Established" = "Info_Year_Established"
                                  )), 
                  
                  br(), br(), br(), br(), br(),br(),br(),br(),
                  h5("Trend Analysis", align = "center"),
                  plotOutput("LibInfoHist")
                          ),

                mainPanel(
                  h5(textOutput("LibInfoTitle"), align = "center"),
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
                 radioButtons("StaffCtFT", "FT or PT:",
                              choices = list("FT", "PT"),
                              selected = "FT"),
                 br(), br(), br(), br(), 
                 h5("Trend Analysis", align = "center"),  
                 plotOutput("StaffCtHist")
               ),
               
               mainPanel(
                 h5("Staff Counts", align = "center"),
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
                                         "Software" = "Software", 
                                         "Videogames" = "Videogames" )),
                 radioButtons("LoanPerNew", "Item Age:",
                              choices = list("Regular" = "Regular", "New to Collection" = "New"),
                              selected = "Regular"),
                 
                 br(), br(), br(), br(), 
                 h5("Trend Analysis", align = "center"),
                 plotOutput("LoanPerHist")
               ),
               
               mainPanel(
                 h5("Loan Period (Days)", align = "center"),
                 plotOutput("LoanPerPlot")
               )
             )
             
             ),
    tabPanel("Fines", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("FineType", "Select a material type:",
                             choices = c("Books",# = "Books",
                                         "New Books",# = "NewBooks", 
                                         "Audiobooks",# = "Audiobooks",
                                         "DVDs",# = "DVDs",
                                         "Periodicals",# = "Periodicals",
                                         "Music",# = "Music",
                                         "Software",# = "Software",
                                         "Videogames")),# = "Videogames")),
                 radioButtons("FinePerson", "Fine type:",
                              choices = list("Adult", "Juvenile"),
                              selected = "Adult"),
                 radioButtons("FineMax", "Fine period:", 
                               choices = list("Daily", "Max"),
                               selected = "Daily"),
                  
                 h5("Trend Analysis", align = "center"),
                 plotOutput("FineHist")
               ),
               
               mainPanel(
                 h5("Fine Amount (Dollars)", align = "center"),
                 plotOutput("FinePlot")
               )
             )
             
    ),    
    
            tabPanel("Salary Info",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("SalInfo", "Select a statistic:", 
                             choices = c("Page",
                                         "Clerk",
                                         "Senior Clerk",
                                         "Principal Clerk",
                                         "Circulation Supervisor",
                                         "Librarian Trainee",
                                         "Librarian I",
                                         "Librarian II",
                                         "Librarian III",
                                         "Librarian IV",
                                         "Assistant Director",
                                         "Library Director",
                                         "Administrative Assistant",
                                         "Security Guard",
                                         "Custodial Worker I",
                                         "Custodial Worker II",
                                         "Custodial Worker III",
                                         "Head Custodian"
                                         )),
                conditionalPanel( 
                  condition = "input.SalInfo == 'Librarian Trainee' ||
                               input.SalInfo == 'Librarian I' ||
                               input.SalInfo == 'Librarian II' ||
                               input.SalInfo == 'Librarian III' ||
                               input.SalInfo == 'Custodial Worker I' ||
                               input.SalInfo == 'Clerk' ||
                               input.SalInfo == 'Senior Clerk'", 
                  checkboxInput("SalInfoFT", "FT or PT", value = TRUE)
                  ),
          
                 
                 br(), br(), br(), br(), br(), br(), br(), 
                h5("Trend Analysis", align = "center"),
                 p("Red used to collect the lower ends of each salary range.  Green collects the highs.  Dark Green shows overlap"),
                 plotOutput("SalInfoHist")
               ),
               
               mainPanel(
                 h5("Compensation Range (Dollars)", align = "center"),
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
