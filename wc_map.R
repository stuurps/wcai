library(shiny)
library(ggmap)
library(leaflet)
library(leaflet.extras)

##Run the following three lines to build dummy data set. Save to same directory
#data$lat <- 52.69636
#data$lng <- -1.781637
#write.csv(data, "wc_loc", row.names = F)


ui <- shinyUI(bootstrapPage(
  leafletOutput("map")
))




server <- shinyServer(function(input, output, session) {
  data <- fread("wc_loc")
  ## Make your initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      addMarkers(lng = data$lng, lat = data$lat) %>%
      setView(lng = -1.781637, lat = 53.581092, zoom = 6) %>%
      addTiles(group = "OSM") %>%
      addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2))
  })
  
  
  ## Observe mouse clicks and add circles
  observeEvent(input$map_click, {
    ## Get the click info like had been doing
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    tmp <- NULL
    tmp$lat <- clat
    tmp$lng <- clng
    data <- rbind(data, tmp)
    write.csv(data, "wc_loc", row.names = F)
    print(data)
    
      leafletProxy('map') %>%
        addMarkers(lng = clng, lat = clat)
    })
    
})

shinyApp(ui=ui, server=server)


