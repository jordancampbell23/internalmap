#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
loadwait <- tagList(
  waiter:: spin_ring() ,
  br(),br(),
  h3("Loading Funding Transparency Dashboard...",
     style = "color:#FFF; font-size: 16pt;")
)

disconnected <- sever::sever_default(
  title = "Session timed out...", 
  subtitle = "Click below to get back", 
  button = "Reload"
)

ship <- data.frame(
  product_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                   "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                   "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                   "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                   "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                   "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                   "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                   "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                   "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                   "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
                   "United States"),
  
  LON = c(-86.90, -153.37, -111.09, -92.20, -119.42, 
          -105.36, -72.70, -75.50, -81.76, -83.44, 
          -155.84, -114.74, -89.00, -86.13, -93.58,
          -98.00, -84.27, -92.33, -68.97, -76.64,
          -71.38, -84.51, -94.64, -90.00, -92.60,
          -109.53, -100.00, -117.20, -71.50, -74.87,
          -106.02, -75.00, -80.79, -100.44, -83.00,
          -96.92, -120.50, -77.19, -71.50, -81.16,
          -100.00, -86.66, -100.00, -111.95, -72.70,
          -78.03, -120.74, -80.50, -89.50, -107.29,
          -96.00),
  
  LAT = c(32.32, 66.16, 34.05, 34.80, 36.78,
          39.11, 41.60, 39.00, 28.00, 33.25, 
          19.74, 44.07, 40.00, 40.27, 42.03,
          38.50, 37.84, 30.40, 45.37, 39.05, 
          42.41, 44.18, 46.39, 33.00, 38.57,
          46.97, 41.50, 39.88, 44.00, 39.83,
          34.30, 43.00, 35.78, 47.65, 40.37,
          36.08, 44.00, 41.20, 41.70, 33.84,
          44.50, 35.86, 31.00, 39.42, 44.00,
          37.93, 47.75, 39.00, 44.50, 43.08,
          37.80),
  Z = c(6, 6, 6, 6, 6,
        6, 6, 6, 6, 6,
        6, 6, 6, 6, 6,
        6, 6, 6, 6, 6,
        6, 6, 6, 6, 6,
        6, 6, 6, 6, 6,
        6, 6, 6, 6, 6,
        6, 6, 6, 6, 6,
        6, 6, 6, 6, 6,
        6, 6, 6, 6, 6,
        4),
  Z1 = c(6, 6, 6, 6, 6,
         6, 6, 6, 6, 6,
         6, 6, 6, 6, 6,
         6, 6, 6, 6, 6,
         6, 6, 6, 6, 6,
         6, 6, 6, 6, 6,
         6, 6, 6, 6, 6,
         6, 6, 6, 6, 6,
         6, 6, 6, 6, 6,
         6, 6, 6, 6, 6,
         4),
  stringsAsFactors = F)

# states <- c("Wisconsin", "West Virginia", "Vermont", "Texas", "South Dakota",
#             "Rhode Island", "Oregon", "New York", "New Hampshire", "Nebraska",
#             "Kansas", "Mississippi", "Illinois", "Delaware", "Connecticut",
#             "Arkansas", "Indiana", "Missouri", "Florida", "Nevada", "Maine",
#             "Michigan", "Georgia", "Hawaii", "Alaska", "Tennessee", "Virginia",
#             "New Jersey", "Kentucky", "North Dakota", "Minnesota", "Oklahoma",
#             "Montana", "Washington", "Utah", "Colorado", "Ohio", "Alabama",
#             "Iowa", "New Mexico", "South Carolina", "Pennsylvania", "Arizona",
#             "Maryland", "Massachusetts", "California", "Idaho", "Wyoming",
#             "North Carolina", "Louisiana", "United States")
# 
# latitude <- c(44.5, 39, 44, 31, 44.5,
#               41.7, 44, 43, 44, 41.5,
#               38.5, 33, 40, 39, 41.6,
#               34.8, 40.27, 38.57, 28, 39.88,
#               45.37, 44.18, 33.25, 19.74, 66.16,
#               35.86, 37.93, 39.83, 37.84, 47.65,
#               46.39, 36.08, 46.97, 47.75, 39.42,
#               39.11, 40.37, 32.32, 42.03, 34.3,
#               33.84, 41.2, 34.05, 39.05, 42.41,
#               36.78, 44.07, 43.08, 35.78, 30.4,
#               37.8)
# 
# 
# longitude <- c(-89.5, -80.5, -72.7, -100, -100,
#                -71.5, -120.5, -75, -71.5, -100,
#                -98, -90, -89, -75.5, -72.7,
#                -92.2, -86.13, -92.6, -81.76, -117.2,
#                -68.97, -84.51, -83.44, -155.84, -153.37,
#                -86.66, -78.03, -74.87, -84.27, -100.44,
#                -94.64, -96.92, -109.53, -120.74, -111.95,
#                -105.36, -83, -86.9, -93.58, -106.02,
#                -81.16, -77.19, -111.09, -76.64, -71.38,
#                -119.42, -114.74, -107.29, -80.79, -92.33,
#                -96)



app_ui <- function(request) {
  options(spinner.color = "#FF6633", spinner.size = 1, spinner.type = 8)
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    waiter::use_waiter(), # dependencies
    waiter::waiter_show_on_load(loadwait, color = "#FF6633"),
    
    sever::use_sever(),
    
    fluidPage(
      id = 'test',

      
      fluidRow(
        column(1),
        
        column(10,
               h2("Tracker", align = "center")
        ),
        
        column(1)
      ),
      
      img(src = 'https://raw.githubusercontent.com/jordancampbell23/Texas/master/reason_logo_V.PNG',
          height = 60,
          width = 158,
          id = "logo"),
      
      mod_national_ui("national_ui_1")
      
    )
  )
}


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'internalmap'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

