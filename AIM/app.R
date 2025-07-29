library(shiny)

orion_platform <- readr::read_csv("orion_platform.csv", show_col_types = FALSE)
# orion_platform <- kdot::get_platform() 


# UI----
ui <- bslib::page_navbar( 
  
  shiny::includeCSS("www/custom.css"),
  shinyjs::useShinyjs(),
  
  title        = "Aspen Investing Menu", 
  window_title = "Aspen Investing Menu", 
  
  ## Search Platform----
  bslib::nav_panel(
    title = "Search Platform",
    icon  = shiny::icon("magnifying-glass"),
    search_platform_ui(orion_platform)
  ),
  
  ## Strategy Information----
  bslib::nav_panel(
    title = "Strategy Information",
    icon  = shiny::icon("pie-chart")
  )
  
) 

# Server----
server <- function(input, output, df = orion_platform) {
  
  ## Search Platform----
  search_platform_server(input, output, df = orion_platform)
  
  ## Strategy Information----
  

  
}

shinyApp(ui = ui, server = server)