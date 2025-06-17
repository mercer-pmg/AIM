mod_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  
  platform <- aws.s3::get_object(
    region = Sys.getenv("AWS_DEFAULT_REGION"),
    key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    
    object = "orion-platform.csv", 
    bucket = "aspen-investing-menu") |>
    readBin("character") |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::select(strategy, type, model, portfolio, tax_managed) |>
    dplyr::distinct() |>
    # dplyr::mutate(more = createLink(strategy)) |>
    dplyr::mutate(
      
      category = NA,
      
      category = dplyr::if_else(
        condition = type == "Blended Strategy",
        true = "Blended",
        false = category),
      
      category = dplyr::if_else(
        condition = type %in% c("Market Series", "Multifactor Series", "Income Series"),
        true      = "Risk-Based",
        false     = category),
      
      category = dplyr::if_else(
        condition = type %in% c("Equity Strategies", "Fixed Income Strategies", "Cash Strategies", "Alternative Strategies", "Special Situation Strategies"),
        true      = "Asset Class",
        false     = category)
    )
  
  shiny::div(
    class = "filters-container",
    shinyWidgets::checkboxGroupButtons(
      inputId = "strategy_category",
      label   = "Category",
      choices = platform$category |> unique(),
      justified = TRUE,
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    ),
    shiny::selectInput(
      inputId = ns("gender_filter"),
      label = "Gender", 
      choices = c("", "female", "male", "genderless", "unknown")
    ),
    shiny::selectInput(
      inputId = ns("status_filter"),
      label = "Status",
      choices = c("", "alive", "dead", "unknown")
    )
  )
}

mod_filters_server <- function(id) {
  shiny::moduleServer(
    id = id,
    function(input, output, session) {
      filter_settings <- reactive({
        
        list(
          name = input$name_filter,
          status = input$status_filter,
          gender = input$gender_filter
        )
      })
      
      return(filter_settings)
    }
  )
}