
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(htmltools)
library(shiny)
library(DT)
library(googleVis)

shinyUI(
        fluidPage(
                tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = file.path("bootstrap.css")),
                        tags$link(rel = "stylesheet", type = "text/css", href = file.path("project.css"))
                ),
                titlePanel("Developing Data Products - Course Project"),
                h4("Trieu Tran"),
                h5("March 7, 2016"),
                #titlePanel("United Nation Statistics Division - Energy Statistics Database (1990 - 2013)"),
                #h4("Electricity, net installed capacity of electric power plants"),
                hr(),
                fluidRow(
                        column(12, 
                                tabsetPanel(
                                      type = "tabs", 
                                      tabPanel("Main", 
                                                p("This is a demonstration of building a Shiny IO Application."),
                                                p(strong(em("Energy Statistics Database")), 
                                                  " is a collective of data of energy production, trade, conversion and consumption of more than 220 countries/territories in the world.  The United Nation Statistics Division has developed this database and has made it publicly available."),
                                                p("This application provides a quick look at",  
                                                        strong(em("Electricity, net installed capacity of electric power plants")),
                                                        "data (as a subset of the", strong(em("Energy Statistics Database)"))),
                                                p("Data Source: ", em(a("http://data.un.org/Data.aspx?d=EDATA&f=cmID%3aEC")))
                                      ),
                                      tabPanel("Data Table",
                                               p("Lorem Ipsum"),
                                               DT::dataTableOutput("table")
                                      ),
                                      tabPanel("Plots",
                                               column(6,  h4("Top-10 Net Installed Capacity"),
                                                      sliderInput("plotYear", "", 
                                                                  min= min(d$Year), max=max(d$Year), value=2013,  step=1, sep=""
                                                      ),
                                                      plotOutput("plot1")
                                                ),
                                                column(6,
                                                        h4("Top-10 Net Generating Capacity"),
                                                        sliderInput("plotYear2", "", 
                                                           min= min(d$Year), max=max(d$Year), value=2013,  step=1, sep=""
                                                        ),
                                                        plotOutput("plot2")
                                                ),
                                               column(12, htmlOutput("blank")),
                                                column(10, 
                                                        h4("G20 Countries Installed Capacity"),
                                                        
                                                        plotOutput("plot3")
                                                ),
                                               column(2, 
                                                      checkboxGroupInput("countriesCheck",
                                                                            #inline = TRUE,
                                                                            label = "", 
                                                                            choices = getG20CountriesList(),
                                                                            selected = getG20Countries()
                                                        )
                                               )
                                               
                                               
                                      ), 
                                      #tabPanel("Summary", leafletOutput("map", width="100%", height="100%")), 
                                      tabPanel("Interactive Map",
                                                includeCSS(file.path("www", "dttable.css")),
                                               
                                                column(9, 
                                                       radioButtons("radioCode", label = "",
                                                                       choices = list("Net Installed Capacity" = "I", "Net Generating Capacity" = "G"), 
                                                                       selected = "I", inline = TRUE)
                                                ),
                                                column(9,
                                                      sliderInput("Year", "Year", 
                                                                  min= min(d$Year), max=max(d$Year), value=2013,  step=1, sep=""
                                                      )
                                                ),
                                                column(12, htmlOutput('map'))
                                                
                                      )
                                )
                        )
                ),
                fluidRow(column(12, includeHTML(file.path("www", "footer.html"))))
        )
)

