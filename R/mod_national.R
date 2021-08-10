#' national UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_national_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2),
      
      column(
        2,
        div(
          class = "panel panel-default",
          style = "background-color:#f3f3f3;
                          box-shadow: 0 6.7px 5.3px rgba(0, 0, 0, 0.01),
                          0 44.6px 35.8px rgba(0, 0, 0, 0.01),
                          0 100px 80px rgba(0, 0, 0, 0.01);",
          div(
            class = "panel body",
            style = "background-color:#f3f3f3;
                            margin: 0;
                            box-shadow: 0 6.7px 5.3px rgba(0, 0, 0, 0.0075),
                            0 44.6px 35.8px rgba(0, 0, 0, 0.0075),
                            0 100px 80px rgba(0, 0, 0, 0.0075);",
            
            div(
              style = "margin: 10px;",
              
              
              selectInput(
                ns('team'),
                label = "Team:",
                choices = c(
                  "All",
                  "Consumer Freedom",
                  "Criminal Justice",
                  "Drug",
                  "Education",
                  "Energy/Enviro",
                  "Municipal Finance",
                  "Pensions",
                  "Privatization",
                  "Transportation"
                ),
                selected = "All"
              )
            )
          )
        )
      ),
      
      column(1),
      
      column(
        2,
        
        div(
          class = "panel panel-default",
          style = "background-color:#f3f3f3;
                          box-shadow: 0 6.7px 5.3px rgba(0, 0, 0, 0.01),
                          0 44.6px 35.8px rgba(0, 0, 0, 0.01),
                          0 100px 80px rgba(0, 0, 0, 0.01);",
          div(
            class = "panel body",
            style = "background-color:#f3f3f3;
                            margin: 0;
                            box-shadow: 0 6.7px 5.3px rgba(0, 0, 0, 0.0075),
                            0 44.6px 35.8px rgba(0, 0, 0, 0.0075),
                            0 100px 80px rgba(0, 0, 0, 0.0075);",
            
            div(
              style = "margin: 10px;",
              
              selectInput(
                ns('content'),
                label = "Content:",
                choices = c(
                  "All",
                  "Book",
                  "Legislative Briefing",
                  "Major Media Appearance",
                  "Notable Citation",
                  "Op-Ed (external)",
                  "Speaking Engagement",
                  "Testimony"
                ),
                selected = "All"
              )
            )
          )
        )
        
      ),
      
      column(1),
      
      column(
        2,
        
        div(
          class = "panel panel-default",
          style = "background-color:#f3f3f3;
                          box-shadow: 0 6.7px 5.3px rgba(0, 0, 0, 0.01),
                          0 44.6px 35.8px rgba(0, 0, 0, 0.01),
                          0 100px 80px rgba(0, 0, 0, 0.01);",
          div(
            class = "panel body",
            style = "background-color:#f3f3f3;
                            margin: 0;
                            box-shadow: 0 6.7px 5.3px rgba(0, 0, 0, 0.0075),
                            0 44.6px 35.8px rgba(0, 0, 0, 0.0075),
                            0 100px 80px rgba(0, 0, 0, 0.0075);",
            
            div(
              style = "margin: 10px;",
              
              selectInput(
                ns('vname'),
                label = "State:",
                choices = c(ship$product_name),
                selected = "United States"
              )
              
            )
            
          )
        )
      ),
      
      column(2)
      
    ),
    
    fluidRow(column(2),
             
             column(
               8,
               
               div(
                 class = "panel panel-default",
                 style = "border-radius: 0px;",
                 div(
                   class = "panel-body centerize",
                   style = "border-radius: 0px; margin: 0.15vw;",
                   align = "center",
                   
                   
                   leaflet::leafletOutput(ns("national_map"), width = '100%', height = 625)
                   
                 )
               )
               
             ),
             
             column(2))
    
  )
}

#' national Server Function
#'
#' @noRd
mod_national_server <- function(input, output, session) {
  ns <- session$ns
  
  # df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1oQTvCQMNZ44F_Gbp3oBtIpoPcijfhO0RP_KjwnNdh78/edit#gid=0") %>% as.data.frame()
  
  df <- readr::read_csv("df.csv")
  
  df <- df %>%
    rename(name = State,
           CONTENT = Content,
           TEAM = Team)
  
  
  file = paste("us-states.geojson", sep = ',')
  states <- geojsonio::geojson_read(file, what = "sp")
  
  states@data <- states@data %>%
    dplyr::filter(name != "Puerto Rico")
  
  
  
  
  a <- reactive({
    ship %>%
      dplyr::select(product_name, LON, LAT, Z, Z1)
  })
  
  
  observe({
    updateSelectInput(
      session,
      inputId = 'vname',
      choices = c("United States", a()$product_name)
    )
  })
  
  output$national_map <- leaflet::renderLeaflet({
    selection <- a() %>% dplyr::filter(product_name %in% input$vname)
    
    
    if (input$team == "All") {
      df <- df %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(
          total = sum(
            book,
            legislative,
            `major media appearance`,
            `notable citation`,
            `op-ed (external)`,
            `speaking engagement`,
            testimony,
            na.rm = TRUE
          )
        )
      
      states@data <- dplyr::left_join(states@data, df)
      
    } else if (input$team == "Consumer Freedom") {
      df <- df %>%
        dplyr::filter(TEAM == "Consumer Freedom") %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = T))
      
      states@data <- dplyr::left_join(states@data, df)
      
    } else if (input$team == "Criminal Justice") {
      df <- df %>%
        dplyr::filter(TEAM == "Criminal Justice") %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = T))
      
      states@data <- dplyr::left_join(states@data, df)
      
    } else if (input$team == "Drug") {
      df <- df %>%
        dplyr::filter(TEAM == "Drug") %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = T))
      
      states@data <- dplyr::left_join(states@data, df)
      
    } else if (input$team == "Education") {
      df <- df %>%
        dplyr::filter(TEAM == "Education") %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = T))
      
      states@data <- dplyr::left_join(states@data, df)
      
    } else if (input$team == "Energy/Enviro") {
      df <- df %>%
        dplyr::filter(TEAM == "Energy/Enviro") %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = T))
      
      states@data <- dplyr::left_join(states@data, df)
      
    } else if (input$team == "Municipal Finance") {
      df <- df %>%
        dplyr::filter(TEAM == "Municipal Finance") %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = T))
      
      states@data <- dplyr::left_join(states@data, df)
      
    } else if (input$team == "Pensions") {
      df <- df %>%
        dplyr::filter(TEAM == "Pensions") %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = T))
      
      states@data <- dplyr::left_join(states@data, df)
      
    } else if (input$team == "Privatization") {
      df <- df %>%
        dplyr::filter(TEAM == "Privatization") %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = T))
      
      states@data <- dplyr::left_join(states@data, df)
      
    } else {
      df <- df %>%
        dplyr::filter(TEAM == "Transportation") %>%
        dplyr::group_by(name) %>%
        dplyr::count(CONTENT) %>%
        tidyr::pivot_wider(names_from = "CONTENT", values_from = "n") %>%
        dplyr::mutate(total = rowSums(across(where(is.numeric)), na.rm = T))
      
      states@data <- dplyr::left_join(states@data, df)
      
    }
    
    
    
    if (input$content == "All") {
      bins <- c(0, 1, 2, 3, Inf)
      pal <-
        leaflet::colorBin("Blues", domain = states$N, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s %s",
        states$name,
        "Total Enagements: ",
        round(states$total)
      ) %>%
        lapply(htmltools::HTML)
      
      leaflet::leaflet(states,
                       options = leaflet::leafletOptions(attributionControl = F)) %>%
        leaflet::setView(-96, 37.8, 4) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Stamen.TonerBackground,
          options = leaflet::providerTileOptions(opacity = 0.2)
        ) %>%
        leaflet::addPolygons(
          color = "white",
          weight = 2,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 1,
          fillColor = ~ pal(total),
          highlightOptions = leaflet::highlightOptions(
            color = "#ff6633",
            weight = 3,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list(
              "font-family" = "'Open Sans', sans-serif",
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#d5d5d5"))
      
      
    } else if (input$content == "Book") {
      validate(
        need(
          "book" %in% as.list(names(df)) == T,
          "There is no activity for that Team and Content combination."
        )
      )
      
      bins <- c(0, 1, 2, 3, Inf)
      pal <-
        leaflet::colorBin("Blues", domain = states$N, bins = bins)
      
      labels <- sprintf("<strong>%s</strong><br/>%s %s",
                        states$name,
                        "Books: ",
                        round(states$book)) %>%
        lapply(htmltools::HTML)
      
      leaflet::leaflet(states) %>%
        leaflet::setView(-96, 37.8, 4) %>%
        leaflet::addPolygons(
          color = "white",
          weight = 2,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 1,
          fillColor = ~ pal(book),
          highlightOptions = leaflet::highlightOptions(
            color = "#ff6633",
            weight = 3,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#d5d5d5"))
      
    } else if (input$content == "Legislative Briefing") {
      bins <- c(0, 1, 2, 3, Inf)
      pal <-
        leaflet::colorBin("Blues", domain = states$N, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s %s",
        states$name,
        "Legislative Briefing: ",
        round(states$`legislative`)
      ) %>%
        lapply(htmltools::HTML)
      
      leaflet::leaflet(states) %>%
        leaflet::setView(-96, 37.8, 4) %>%
        leaflet::addPolygons(
          color = "white",
          weight = 2,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 1,
          fillColor = ~ pal(`legislative`),
          highlightOptions = leaflet::highlightOptions(
            color = "#ff6633",
            weight = 3,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#d5d5d5"))
      
    } else if (input$content == "Major Media Appearance") {
      bins <- c(0, 1, 2, 3, Inf)
      pal <-
        leaflet::colorBin("Blues", domain = states$N, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s %s",
        states$name,
        "Major Media Appearance: ",
        round(states$`major media appearance`)
      ) %>%
        lapply(htmltools::HTML)
      
      leaflet::leaflet(states) %>%
        leaflet::setView(-96, 37.8, 4) %>%
        leaflet::addPolygons(
          color = "white",
          weight = 2,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 1,
          fillColor = ~ pal(`major media appearance`),
          highlightOptions = leaflet::highlightOptions(
            color = "#ff6633",
            weight = 3,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#d5d5d5"))
      
    } else if (input$content == "Notable Citation") {
      bins <- c(0, 1, 2, 3, Inf)
      pal <-
        leaflet::colorBin("Blues", domain = states$N, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s %s",
        states$name,
        "Notable Citation: ",
        round(states$`notable citation`)
      ) %>%
        lapply(htmltools::HTML)
      
      leaflet::leaflet(states) %>%
        leaflet::setView(-96, 37.8, 4) %>%
        leaflet::addPolygons(
          color = "white",
          weight = 2,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 1,
          fillColor = ~ pal(`notable citation`),
          highlightOptions = leaflet::highlightOptions(
            color = "#ff6633",
            weight = 3,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#d5d5d5"))
      
    } else if (input$content == "Op-Ed (external)") {
      bins <- c(0, 1, 2, 3, Inf)
      pal <-
        leaflet::colorBin("Blues", domain = states$N, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s %s",
        states$name,
        "Op-Ed (external): ",
        round(states$`op-ed (external)`)
      ) %>%
        lapply(htmltools::HTML)
      
      leaflet::leaflet(states) %>%
        leaflet::setView(-96, 37.8, 4) %>%
        leaflet::addPolygons(
          color = "white",
          weight = 2,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 1,
          fillColor = ~ pal(`op-ed (external)`),
          highlightOptions = leaflet::highlightOptions(
            color = "#ff6633",
            weight = 3,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#d5d5d5"))
      
    } else if (input$content == "Speaking Engagement") {
      bins <- c(0, 1, 2, 3, Inf)
      pal <-
        leaflet::colorBin("Blues", domain = states$N, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s %s",
        states$name,
        "Speaking Engagement: ",
        round(states$`speaking engagement`)
      ) %>%
        lapply(htmltools::HTML)
      
      leaflet::leaflet(states) %>%
        leaflet::setView(-96, 37.8, 4) %>%
        leaflet::addPolygons(
          color = "white",
          weight = 2,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 1,
          fillColor = ~ pal(`speaking engagement`),
          highlightOptions = leaflet::highlightOptions(
            color = "#ff6633",
            weight = 3,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#d5d5d5"))
      
      
    } else {
      bins <- c(0, 1, 2, 3, Inf)
      pal <-
        leaflet::colorBin("Blues", domain = states$N, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s %s",
        states$name,
        "Testimony: ",
        round(states$testimony)
      ) %>%
        lapply(htmltools::HTML)
      
      leaflet::leaflet(states) %>%
        leaflet::setView(-96, 37.8, 4) %>%
        leaflet::addPolygons(
          color = "white",
          weight = 2,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 1,
          fillColor = ~ pal(testimony),
          highlightOptions = leaflet::highlightOptions(
            color = "#ff6633",
            weight = 3,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list("font-weight" = "normal",
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#d5d5d5"))
      
    }
    
  })
  
  
  
  observe({
    selection <- a() %>% dplyr::filter(product_name %in% input$vname)
    leaflet::leafletProxy("national_map") %>%
      leaflet::flyTo(lat = selection$LAT,
                     lng = selection$LON,
                     zoom = selection$Z)
    
  })
  
  
  
  
}

## To be copied in the UI
# mod_national_ui("national_ui_1")

## To be copied in the server
# callModule(mod_national_server, "national_ui_1")
