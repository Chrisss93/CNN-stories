library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)
library(rmongodb)
options(useFancyQuotes = FALSE, scipen = 99)

mongo <- cnn.stories::mongoConnect()
ns    <- paste(Sys.getenv("MONGO_CNN_DB"), Sys.getenv("MONGO_CNN_COLLECTION"), sep = ".")
# mongo <- mongo.create()
# ns <- "test.junk"

ui <- dashboardPage(
    dashboardHeader(title = "Hey you"),
    dashboardSidebar(
        searchInput("search", NULL, placeholder = "Search...", btnSearch = icon("search")),
        checkboxInput("searchCase", "Case-sensitive", value = FALSE),
        dateRangeInput("date", "When?", Sys.Date() - 30, Sys.Date(), format = "M d, yyyy"),
        valueBoxOutput("info_articles_n", 12),
        valueBoxOutput("info_articles_loc", 12),
        valueBoxOutput("info_articles_src", 12)
    ),
    dashboardBody(
        fluidRow( leafletOutput("map") ),
        absolutePanel(right = 5, top = 60, width = 50, class = "floater",
                      dropdownButton(status = "success", icon = icon("gear"), right = TRUE,
                          tags$h4("Double-click the map to find stories within a radius of"),
                          tags$div(column(4, numericInput("max_dist", NULL, 200, width = "100%")),
                                   column(8, switchInput("km", NULL, TRUE, "km", "mi", inline = FALSE))
                                   ))),
        fluidRow(
            box(status = "primary",
                uiOutput("lt")
            ),
            box(title = "Topic distribution", status = "primary", solidHeader = TRUE,
                numericInput("topic_n", label = "# Topics", 6, 2, 15, width = "60px"),
                plotOutput("topic_dist"))
        ),
        fluidRow(box(title = "Diagnostics", status = "danger", solidHeader = TRUE, verbatimTextOutput("inn")))
    )
)

server <- function(input, output, session) {
    react_list <- reactiveValues(doubleClick = FALSE, lastClick = NA)
    observe(react_list$f <- ifelse(input$km, identity, function(x) x * 1.6))
    
    # Mongo query for articles based on natural language search or geo-distance
    articles <- reactive({
        validate(
            # need( nchar(input$search) > 1, "I need more than a letter"),
            need( !grepl("(?!')[[:punct:]]", input$search, perl = TRUE),
                  "No symbols permitted except apostrophes")
        )
        query <- list("$text" = list("$search"        = dQuote(input$search),
                                     "$caseSensitive" = input$searchCase),
                      "date"  = list("$gte" = as.POSIXct(input$date[1]),
                                     "$lte" = as.POSIXct(input$date[2])),
                      "topics" = list("$exists" = TRUE),
                      "center" = list("$ne" = NA))
        attr(query, "group_layer") <- input$search
        
        if (react_list$doubleClick) {
            double_coords <- unlist(input$map_click[2:1], use.names = FALSE)
            # while(any( i <- double_coords < -180 | double_coords > 180)) {
            #     double_coords[i] <- (double_coords[i] %% 180) * sign(double_coords)[i]
            # }
            query[["location"]] <- list("$near" = list(
                "$geometry" = list(type = "Point", coordinates = unlist(input$map_click[2:1], use.names = FALSE)),
                "$maxDistance" = react_list$f(input$max_dist) * 1000))
            query[["$text"]] <- NULL
            attr(query, "group_layer") <- paste(rev(double_coords), collapse = "_")
        }
        out <- mongo.find.all(mongo, ns, query,
                              fields = list("_id" = 0, url = 1, title = 1, topics = 1, source = 1,
                                            location.coordinates = 1, center = 1, links = 1,
                                            score = list("$meta" = "textScore")))
        names(out) <- sapply(out, "[[", "title")
        attr(out, "group_layer") <- attr(query, "group_layer")
        return(out)
    })

    output$lt <- renderUI({
        pickerInput("chosen_docs", NULL, choices = names(articles()), selected = names(articles()), multiple = TRUE,
                    options = list(
                        "actions-box"          = TRUE,
                        "dropup-auto"          = FALSE,
                        "live-search"          = TRUE,
                        "selected-text-format" = "count",
                        "count-selected-text"  = "{0}/{1} articles chosen"))
    })
	
    # Visualization of LDA topic mixture model for selected articles
    output$topic_dist <- renderPlot({
        validate(need(length(articles()) > 0, "No articles selected"))
        topics  <- lapply(articles(), function(a) a$topics[[as.character(input$topic_n)]] ) %>%
            bind_rows(.id = "title")
        lvl     <- topics %>% summarize_if(is.numeric, mean) %>% unlist() %>% sort() %>% names()
        plot_df <- topics %>%
            reshape2::melt(id.vars = "title", variable.name = "Topics", value.name = "Share") %>%
            mutate(Topics = factor(Topics, levels = lvl)) %>%
            filter(title %in% input$chosen_docs)

        ggplot(plot_df, aes(Topics, Share)) +
            geom_bar(stat = "summary", fun.y = "mean") +
            scale_y_continuous(labels = scales::percent) +
            coord_flip()
    })
	# RadarChart still unusable due to https://github.com/MangoTheCat/radarchart/issues/28
	# output$radar <- radarchart::renderChartJSRadar({
	#     validate(need(length(articles()) > 0, "No articles selected"))
	#     print("radar replies: 'I'm here!'")
	#     topics <- lapply(articles(), function(a) a$topics[[as.character(input$topic_n)]] ) %>%
	#         bind_rows() %>%
	#         bind_rows( summarize_all(., mean) ) %>%
	#         t() %>%
	#         as.data.frame() %>%
	#         setNames( c(names(articles()), "All") )
	#     print(row.names(topics))
	#     radarchart::chartJSRadar(topics[, "All", drop = FALSE], row.names(topics))
	# })
	
    # Record double-click event 
    observeEvent(input$map_click$.nonce, {
        react_list$doubleClick <- identical(react_list$lastClick, input$map_click[1:2])
        react_list$lastClick   <- input$map_click[1:2]
    })

    # Initialize leaflet
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(maxZoom = 6, doubleClickZoom = FALSE)) %>%
            addTiles() %>%
            addScaleBar("bottomleft") %>%
            setView(0, 0, 2)
    })
    
    # Populate leaflet with markers from selected articles
    observe({ if (length(articles()) > 0) {
        leaf_df <- lapply(articles(), function(a) {
            a$lon <- a$location$coordinates[1]
            a$lat <- a$location$coordinates[2]
            a[ setdiff(names(a), c("links", "topics", "location")) ]
        }) %>%
            bind_rows() %>%
            filter(title %in% input$chosen_docs) %>%
            mutate(pop = paste0("<a href='", url, "'>", title, "</a>")) %>%
            group_by(lat, lon, center) %>%
            mutate(N      = n(),
                   pop    = paste(pop, collapse = "</li><li>"),
                   pop    = replace(pop, N > 1, paste0("<ul><li>", pop, "</li></ul>")),
                   source = paste(unique(source), collapse = "/")) %>% 
			ungroup()
        if ( nrow(leaf_df) > 0 ) {
            leafletProxy("map") %>%
                clearMarkers() %>%
                addMarkers(data = leaf_df, lng = ~lon, lat = ~lat, popup = ~pop, group = ~source, label = ~center,
                           layerId = leaf_df$url,
                           options = markerOptions(riseOnHover = T), clusterId = "cluster")
        }
    }})
    
    # Draw perimeter on double-click event
    observeEvent(input$map_click[1:2], {
        if (react_list$doubleClick) {
            leafletProxy("map") %>%
                addCircles(input$map_click$lng, input$map_click$lat, react_list$f(input$max_dist) * 1000,
                    color = "red", fillOpacity = 0.1, label = "Click to find articles in this radius",
                    layerId = paste(input$map_click[c("lat", "lng")], collapse = "_"))
        }
    })
    # Remove perimeter on click
    observeEvent(input$map_shape_click, {
        leafletProxy("map") %>%
            removeShape(layerId = paste(input$map_shape_click[c("lat", "lng")], collapse = "_"))
    })
    
    # # Add auxilary markers and polylines for linked stories on marker click
    # observeEvent(input$map_marker_click, {
    #     clicked <- Filter(articles(), f = function(a) {
    #         identical( a$location$coordinates, as.numeric(input$map_marker_click[c("lng", "lat")]) )
    #     })
	# 	clicked_df <- filter(leaf_df, title %in% names(clicked))
    # 
    #     links <- as.list(paste0(cnn.stories::CNN, sapply(clicked, "[[", "links")))
    #     links <- links[links != paste0(cnn.stories::CNN, "list()")]
    #     link_articles <- mongo.find.all(mongo, ns, query = list("url" = list("$in" = links)),
    #                                     fields = list("_id" = 0, url = 1, lat = 1, lon = 1, title = 1, center = 1),
    #                                     data.frame = TRUE)
    # 
	# 	leafletProxy("map") %>% 
	# 		removeMarker(sapply(clicked, "[[", "url")) %>% 
	# 		addAwesomeMarkers(data = clicked_df, lng = ~lon, lat = ~lat, popup = ~pop, group = ~source, 
	# 						  label = ~center, layerId = leaf_df$url, icon = awesomeIcons(markerColor = "red"))
	# 						  browser()
	# 	if ( length(link_articles) > 0 ) {
	# 		link_df <- lapply(link_articles, function(a) {
	#             a$lon <- a$location$coordinates[1]
	#             a$lat <- a$location$coordinates[2]
	#             a[ setdiff(names(a), c("links", "topics", "location")) ]
	#         }) %>%
	#             bind_rows() %>%
	#             filter(title %in% input$chosen_docs) %>%
	#             mutate(pop = paste0("<a href='", url, "'>", title, "</a>")) %>%
	#             group_by(lat, lon, center) %>%
	#             mutate(N      = n(),
	#                    pop    = paste(pop, collapse = "</li><li>"),
	#                    pop    = replace(pop, N > 1, paste0("<ul><li>", pop, "</li></ul>")),
	#                    source = paste(unique(source), collapse = "/")) %>% 
	# 			ungroup()
    # 
	# 		polyline_df <- link_df[rep(seq(nrow(link_df)), each = 2), ]
	# 		polyline_df[seq(nrow(polyline_df)) %% 2 == 0, "lon"] <- input_map_marker_click["lng"]
	# 		polyline_df[seq(nrow(polyline_df)) %% 2 == 0, "lat"] <- input_map_marker_click["lat"]
    # 
	# 		leafletProxy("map") %>% 
	# 			addAwesomeMarkers(data = link_df, lng = ~lon, lat = ~lat, popup = ~pop, group = ~source, 
	# 							  label = ~center, layerId = leaf_df$url, 
	# 							  icon = awesomeIcons("link", markerColor = "green")) %>% 
	# 			addPolyLines(data = polyline_df, lng = ~lon, lat = ~lat)
	# 	}
    # })

    output$info_articles_n <- renderValueBox({
        valueBox(length(articles()), "Relevant articles", icon = icon("file-text-o"), color = "teal")
    })
    output$info_articles_loc <- renderValueBox({
        locs <- unique(sapply(articles(), "[[", "center"))
        valueBox(length(unique(locs)), "Different locations", icon = icon("map-pin"), color = "red")
    })
    output$info_articles_src <- renderValueBox({
        sources <- sapply(articles(), "[[", "source")
        valueBox(length(unique(sources)), "Sub-domains", icon = icon("navicon"), color = "green")
    })

    output$inn <- renderPrint({
        for (nm in setdiff(names(input), "chosen_docs")) {
            print(paste(nm, input[[nm]], sep = ": "))
        }
        if ( exists("doubleClick") ) {
            print(paste("Double click", doubleClick, sep = ":"))
        }
    })

    session$onSessionEnded(function() {
        doubleClick <- NULL
        lastClick   <- NULL
        mongo.destroy(mongo)
        cat("Connection terminated.")
    })
}

shinyApp(ui, server)
