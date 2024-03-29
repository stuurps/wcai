library(shiny)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(data.table)
library(shinyjs)


# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

ui <- shinyUI(
  bootstrapPage(
    useShinyjs(),
    leafletOutput("map"),
    textInput("lati", labelMandatory("Latitude"), ""),
    textInput("loni", labelMandatory("Longitude"), ""),
    dateInput("date1", labelMandatory("Date:")),
    sliderInput("rating", labelMandatory("Rating"), 0, 10, 5, ticks = FALSE),
    textInput("free_text", labelMandatory("Write about your experience"), ""),
    textInput("email", labelMandatory("Email Address"),""),
    checkboxInput("contact", "Contact me about new spots", FALSE),
    actionButton("submit", "Submit")
    )
  )
  
server <- shinyServer(function(input, output, session) {
  
  # Enable the Submit button when all mandatory fields are filled out
  observe({toggleState(id = "submit", condition = nchar(input$email) >= 3 & nchar(input$lati) > 8)

  
  data <- fread("wc_locv2.csv")
  ## Make your initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      addMarkers(lng = data$lng, lat = data$lat, label = (data$rating)) %>%
      setView(lng = -1.781637,
              lat = 53.581092,
              zoom = 6) %>%
      addTiles(group = "OSM") %>%
      addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2))
  })
  
  

  ## Observe mouse clicks and add circles
  observeEvent(input$map_click, {
    ## Get the click info like had been doing
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    updateTextInput(session, "lati", value = paste("", clat))
    updateTextInput(session, "loni", value = paste("", clng))
    
    
  })
  
  observeEvent(input$submit,{
    tmp <- NULL
    tmp$lat <- as.numeric(input$lati)
    tmp$lng <- as.numeric(input$loni)
    tmp$upload_date <- as.character(Sys.Date())
    tmp$stay_date <- as.character(input$date1)
    tmp$rating <- input$rating
    tmp$free_text <- input$free_text
    tmp$email <- input$email
    tmp$opt_in <- input$contact
    
    clat <- as.numeric(input$lati)
    clng <- as.numeric(input$loni)
    data <- rbind(data, tmp)
    write.csv(data, "wc_locv2.csv", row.names = F)
    print(data)
    #saveData(formData())
    #leafletProxy('map') %>%
    #  addMarkers(lng = clng, lat = clat)
    
  })
})

})

shinyApp(ui = ui, server = server)
