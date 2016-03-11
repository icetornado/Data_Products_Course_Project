
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(googleVis)
library(htmltools)
library(shiny)
library(DT)
#library(leaflet)

library(dplyr)


shinyServer(function(input, output) {
#         output$table <- DT::renderDataTable(
#                 DT::datatable(
#                         subset(d, select = c(Country, Commodity, Year, Quantity)),
#                         colnames = c("Country", "Source", "Year", "Quantity")
#                 )
#         )
        myYear <- reactive({
                input$Year
        })
        
        myCode <- reactive({
                input$radioCode
        })
        
#         myPlotYear <- reactive({
#                 input$plotYear
#         })
        
        plotData1 <- reactive({
                getTop10byYear(d, input$plotYear, "I")
        })
        
        plotData2 <- reactive({
                getTop10byYear(d, input$plotYear2, "G")
        })
        
        plotData3 <- reactive({
                getG20Data(d, "I", input$countriesCheck)
        })
        
        dTable = datatable(
                subset(d, select = c(Country, Commodity, Year, Quantity)),
                colnames = c("Country", "Source", "Year", "Quantity"), 
                rownames = FALSE,
                filter = "top",
                options = list(
                        pageLength = 25
                )
        )
        output$table <- DT::renderDataTable(
                dTable
        )
       
        output$map <- renderGvis({
                mapData = d %>%
                        filter(Year == myYear(), code == myCode()) %>%
                        group_by(Country) %>%
                        summarise(Total = sum(Quantity), logSum = log10(sum(Quantity))) 
                        #%>%
                        #mutate(hoverText = paste(Country, "<br />", " Total: ", netSum))
                
                if(myCode() == "I") {
                      colorAxis <- "{colors:['#B3D0E8', '#4F6C86', '#0584F3'], minValue: 0, maxValue: 6000000}"
                }
                else if(myCode() == "G") {
                        colorAxis <- "{colors:['#FFF2BD', '#B55400', '#FF9E04'], minValue: 0, maxValue: 50000}"
                }
                
                gvisGeoChart(mapData, locationvar="Country", 
                             colorvar="Total",
                             #hovervar = "hoverText",
                             options=list(
                                     #displayMode = "text",
                                     title = "Hello",
                                     projection="kavrayskiy-vii",
                                     displayMode="regions",
                                     width="100%", 
                                     colorAxis=colorAxis,
                                     backgroundColor= "#2b3e50",
                                     forceIFrame='TRUE',
                                     #legend = "{textStyle: {color: 'blue', fontSize: 16}}",
                                     tooltip = "{textStyle: {color: 'red'}, showColorCode: true, fontSize: 20}"
                                     #titleTextStyle="{color:'red', fontName:'Courier', fontSize:16}"
                                     
                             )
                )
        }) 
        
        output$plot1 <- renderPlot({
                plotTop10byYear(plotData1(), input$plotYear, "I")
        })
        
        output$plot2 <- renderPlot({
                plotTop10byYear(plotData2(), input$plotYear2, "G")
        })
        
        output$plot3 <- renderPlot({
                plotG20(plotData3(),"I")
        })
})
