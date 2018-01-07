library(shiny)
library(leaflet)
library(dplyr)
library(sqldf)
library(plotly)

dfFiltered <- df

dfSummarised <- sqldf("select 
                        t.month, 
                        t.'Crime.type', 
                        count(t.'Crime.type') 
                        from df t
                        group by t.month, t.'Crime.type'")

colnames(dfSummarised)<- c("month","crimeType","crimeTypeCount")

shinyServer(function(input, output) {
  output$mymap <- renderLeaflet({
          
          set.seed(2018-01-06)
          
          if(input$month =='all')
                {if(input$crime.type == 'all') {} 
                 else  {dfFiltered <- filter(df, Crime.type == input$crime.type)}
                }
          else  
                {if(input$crime.type == 'all')
                        {dfFiltered <- filter(df, Month == input$month)}
                 else   {dfFiltered <- filter(df, Month == input$month & Crime.type == input$crime.type)}
                }
          
          myMap <- leaflet() %>%
                  addTiles()
          
          if (input$legend == FALSE) 
                {if (input$cluster == FALSE) {myMap <- myMap %>% addMarkers(dfFiltered$Longitude, dfFiltered$Latitude)}
                 
                  else {myMap <- myMap %>% addMarkers(dfFiltered$Longitude, dfFiltered$Latitude, 
                                                      clusterOptions = markerClusterOptions())}
                }
          else 
          {if (input$cluster == FALSE) {myMap <- myMap %>% addMarkers(dfFiltered$Longitude, dfFiltered$Latitude, 
                                                                      popup = dfFiltered$Last.outcome.category)}
                  
                 else {myMap <- myMap %>% addMarkers(dfFiltered$Longitude, dfFiltered$Latitude, 
                                                     popup = dfFiltered$Last.outcome.category,
                                                     clusterOptions = markerClusterOptions())}
          }        
  })
  output$myplot <- renderPlotly({
          
          if(input$crime.type == 'all') {} 
                else  {dfSummarised <- filter(dfSummarised, crimeType == input$crime.type)}
          
          p <- plot_ly(source = "source") %>% 
                       add_lines(data = dfSummarised, x = ~month, y = ~crimeTypeCount, 
                       color = ~crimeType, 
                       mode = "lines", line = list(width = 1))

          p <- p %>%
                layout(title = paste("Crime trend for '", input$crime.type, "' during 2016"),
                       yaxis = list(title = "crime type count"),
                       xaxis = list(title = "month"))
          p
          
  })
})
