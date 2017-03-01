library(shiny)
library(dplyr)
library(lubridate)
library(rmongodb)
library(leaflet)
mongo <- mongo.create(
  host     = Sys.getenv("MONGO_CNN_HOST"), 
  username = Sys.getenv("MONGO_CNN_USER"), 
  password = Sys.getenv("MONGO_CNN_PASSWORD"), 
  db       = Sys.getenv("MONGO_CNN_DB")
)
cnn_ns <- paste(Sys.getenv("MONGO_CNN_DB"), Sys.getenv("MONGO_CNN_COLLECTION"), sep = ".")
good_fields <- '{"_id": 0, "lat": 1, "lon": 1, "title": 1, "summary": 1, "url": 1}'

custom_updateSliderInput <- function (session, inputId, label = NULL, value = NULL, min = NULL, 
            max = NULL, step = NULL, ...)  {
    vals <- shiny:::dropNulls(list(value, min, max))
    type <- unique(lapply(vals, function(x) {
      if (inherits(x, "Date")) 
        "date"
      else if (inherits(x, "POSIXt")) 
        "datetime"
      else "number"
    }))
    if (length(type) > 1) {
      stop("Type mismatch for value, min, and max")
    }
    if ((length(type) == 1) && (type == "date" || type == "datetime")) {
      to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
      if (!is.null(min)) 
        min <- to_ms(min)
      if (!is.null(max)) 
        max <- to_ms(max)
      if (!is.null(value)) 
        value <- to_ms(value)
    }
    message <- shiny:::dropNulls(list(label = label, value = shiny:::formatNoSci(value), 
                                     min = shiny:::formatNoSci(min), max = shiny:::formatNoSci(max), 
                                     step = shiny:::formatNoSci(step), ...))
    session$sendInputMessage(inputId, message)
}

# articles <- cbind(articles, popup = apply( articles, 1, function(x) { 
#   as.character(a(href = x$url, paste0(strong(x$title), br(), x$summary))) 
# }))


ui <- fluidPage(
  leafletOutput("map"),
  textInput("searchText", "Keywords: ", placeholder = "politics", value = ""),
  checkboxInput("searchCase", "Case-sensitive", value = FALSE),
  checkboxInput("uncluster", "Show all?", value = FALSE),
  sliderInput("date", "Calendar range:", min = floor_date(Sys.Date(), "month"), max = Sys.Date(),
              value = c(max(Sys.Date() - days(7), floor_date(Sys.Date(), "month")),  Sys.Date()), 
              timeFormat = "%b %d, %y"),
  textOutput("days"),
  actionButton("searchButton", "search"),
  verbatimTextOutput("text")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    input$searchButton
    
    mongo_query <<- list("$text" = list("$search"        = isolate(input$searchText), 
                                       "$caseSensitive" = isolate(input$searchCase)),
                        "date"  = list("$gte" = as.POSIXct(isolate(input$date[1])),
                                       "$lte" = as.POSIXct(isolate(input$date[2]))))
    if (is.null(isolate(input$searchText)) || nchar(isolate(input$searchText)) < 1) {
      mongo_query <<- lapply(mongo_query["date"], function(x) {lapply(x, as.POSIXct)})
    }
    
    articles <- mongo.find.all(mongo, cnn_ns, mongo_query, fields = good_fields) %>% 
      Filter(f = function(a) {!is.null(a$lat)}) %>% 
      bind_rows() %>% 
      mutate(popup = paste0("<a href='", url, "'><strong>", title, "</strong></a><br>", summary))
    
    leaflet() %>% addTiles() %>% 
      addMarkers(data = articles, lng = ~lon, lat = ~lat, popup = ~popup, 
                 options = markerOptions(title= ~title), 
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE))
  })
  
  observe({
    if (difftime(floor_date(input$date[1], "month"), input$date[1], units = "days") <= 7) {
      updateSliderInput(session, "date", min = floor_date(input$date[1] - days(7), "month"))
    }
    # if (difftime(floor_date(input$date[1], "year"), floor_date(input$date[1], "month")) <= 21) {
    #   updateSliderInput(session, "date", min = floor_date(input$date[1] - days(21), "year"))
    # }
    # 
  })
  output$days <- renderText({
    capture.output(difftime(input$date[2], input$date[1]))
  })
  # uncluster <- reactive({input$uncluster})
  # 
  # observe({
  #   if (input$uncluster) {
  #     leafletProxy("map") %>% addMarkers(clusterOptions = NULL)
  #   }
  # })
  
  output$text <- renderText({ 
    a <- capture.output(mongo.bson.from.list(mongo_query))
    paste0(a, "\n")
    })
  
  session$onSessionEnded(function() {
    mongo.destroy(mongo);
    cat("Connection terminated.")
  })
}

shinyApp(ui = ui, server = server)
