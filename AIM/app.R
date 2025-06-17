

library(shiny)

# createLink <- function(val) {
#   sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">More</a>',val)
# }

platform <- aws.s3::get_object(
  region = Sys.getenv("AWS_DEFAULT_REGION"),
  key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  
  object = "orion-platform.csv", 
  bucket = "aspen-investing-menu") |>
  readBin("character") |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::select(strategy, type, model, portfolio, tax_managed, model_agg) |>
  dplyr::distinct() |>
  # dplyr::mutate(more = createLink(strategy)) |>
  dplyr::mutate(
    
    category = NA,
    
    category = dplyr::if_else(
      condition = type == "Blended Strategy",
      true      = "Blended",
      false     = category),
    
    category = dplyr::if_else(
      condition = type %in% c("Market Series", "Multifactor Series", "Income Series"),
      true      = "Risk-Based",
      false     = category),
    
    category = dplyr::if_else(
      condition = type %in% c("Equity Strategies", "Fixed Income Strategies", "Cash Strategies", "Alternative Strategies", "Special Situation Strategies"),
      true      = "Asset Class",
      false     = category)
  ) |>
  
  # Classify models
  dplyr::mutate(
    model_group = "Other",
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Market"),
      true      = "Market Series",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Multifactor"),
      true      = "Multifactor Series",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Income"),
      true      = "Income Series",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Cash Mgmt"),
      true      = "Cash Mgmt",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "Quantitative Portfolio"),
      true      = "Quantitative Portfolios",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "MA Fixed Income"),
      true      = "Fixed Income",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "Ladder \\(ETF\\)$"),
      true      = "Fixed Income ETF Ladder",
      false     = model_group
    ),
    
    model_group = dplyr::if_else(
      condition = stringr::str_detect(model_agg, "BlackRock|Nuveen|PIMCO"),
      true      = "Third-Party Fixed Income SMA",
      false     = model_group
    )
    
  )

colors <- c("blue", "green", "red", "orange", "purple") # Expand if needed

category_list <- unique(platform$category)
type_list     <- unique(platform$type)

model_list <- platform |> 
  dplyr::select(model_group, model_agg) |> 
  dplyr::arrange(model_agg) |> 
  dplyr::distinct() |> 
  dplyr::group_by(model_group) |>
  dplyr::summarise(model_ls = list(model_agg)) |>
  tibble::deframe()

# Assign colors based on category order (cycling through if needed)
assigned_colors <- colors[seq_along(category_list) %% length(colors) + 1]


ui <- fluidPage(
  
  shiny::includeCSS("www/main.css"),
  
  tags$head(tags$script(src = "custom.js")),
  
  # Application title
  titlePanel("Aspen Investing Menu"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      shiny::checkboxGroupInput(
        inputId = "strategy_category",
        label   = "Category",
        choices = category_list),
      
      
      checkboxGroupInput(
        inputId = "strategy_types",
        label   = "Type",
        choices = type_list),
      
      shiny::sliderInput(
        input     = "equityAlloc",
        label     = "Equity Allocation Range",
        min       = 0,
        max       = 100,
        value     = c(0,100),
        step      = 10,
        dragRange = TRUE),
      
      checkboxGroupInput(
        inputId      = "tm_status",
        label        = "Tax-Managed",
        choiceNames  = c("Yes", "No"),
        choiceValues = c(TRUE, FALSE),
        inline       = TRUE),
      
      shiny::radioButtons(
        inputId  = "model_search",
        label    = "Search by Model",
        choices  = c("Any Model", "All Models"),
        selected = "Any Model",
        inline   = TRUE),
      
      shiny::selectInput(
        inputId   = "models",
        label     = NULL,
        choices   = model_list,
        multiple  = TRUE,
        selectize = TRUE
      )
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("selected_badges"),
      DT::DTOutput("strategies")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # UI updates on category selection
  shiny::observeEvent(input$strategy_category, {
    
    if(is.null(input$strategy_category)) {
      shiny::updateCheckboxGroupInput(
        inputId = "strategy_types",
        choices = type_list
      )
      
      shiny::updateSelectInput(
        inputId = "models",
        choices = model_list
      )
    } else {
      
      shiny::updateCheckboxGroupInput(
        inputId = "strategy_types",
        choices = platform |> 
          dplyr::filter(category %in% input$strategy_category) |> 
          dplyr::pull(type) |>
          unique()
      )
      
      shiny::updateSelectInput(
        inputId = "models",
        choices = platform |>
          dplyr::filter(category %in% input$strategy_category) |>
          dplyr::select(model_group, model_agg) |> 
          dplyr::arrange(model_agg) |> 
          dplyr::distinct() |> 
          dplyr::group_by(model_group) |>
          dplyr::summarise(model_ls = list(model_agg)) |>
          tibble::deframe()
      )
      
    }
    
    
  },
  
  ignoreNULL = FALSE
  )
  
  # UI updates on type selection
  shiny::observeEvent(
    eventExpr = input$strategy_types, 
    handlerExpr = {
      
      if(is.null(input$strategy_types)) {
        shiny::updateCheckboxGroupInput(
          inputId = "strategy_types",
          choices = type_list
        )
        
        shiny::updateSelectInput(
          inputId = "models",
          choices = model_list
        )
      }
      
      shiny::updateSelectInput(
        inputId = "models",
        choices = platform |>
          dplyr::filter(type %in% input$strategy_types) |>
          dplyr::select(model_group, model_agg) |> 
          dplyr::arrange(model_agg) |> 
          dplyr::distinct() |> 
          dplyr::group_by(model_group) |>
          dplyr::summarise(model_ls = list(model_agg)) |>
          tibble::deframe()
      )
    }
  )
  
  
  
  # Filtering functions
  cat <- reactive({
    
    if(length(input$strategy_category) == 0) {
      unique(platform$category)
    } else {
      input$strategy_category
    }
    
  })
  
  
  
  types <- reactive({
    
    if(length(input$strategy_types) == 0) {
      unique(platform$type)} else {
        input$strategy_types
      }
    
  })
  
  equity_allo <- reactive({
    
    eq_range <- seq(
      from = input$equityAlloc[1], 
      to   = input$equityAlloc[2], 
      by   = 10)
    
    c(NA, eq_range)
    
  })
  
  tax_mgmt <- reactive({
    
    if(length(input$tm_status) == 0) {
      unique(platform$tax_managed)
    } else {
      input$tm_status
    }
    
  })
  
  models <- reactive({
    
    if(length(input$models) == 0) {
      unique(platform$model_agg)
    } else {
      input$models
    }
    
  })
  
  # Filtering platform based on filtering functions
  dat <- reactive({
    
    df <- platform |>
      dplyr::filter(category %in% cat()) |>
      dplyr::filter(type %in% types()) |>
      dplyr::filter(portfolio %in% equity_allo()) |>
      dplyr::filter(tax_managed %in% tax_mgmt()) |>
      dplyr::filter(model_agg %in% models())
    
    if(input$model_search == "All Models") {
      
      models_n <- length(models())
      
      
      xx <- df |>
        dplyr::select(strategy, model_agg) |>
        dplyr::group_by(strategy) |>
        dplyr::summarise(n = dplyr::n()) |>
        dplyr::filter(n == models_n) |>
        dplyr::pull(strategy)
      
      df <- df |> 
        dplyr::filter(strategy %in% xx)
      
      
    }
    
    df <- df |>
      dplyr::select(strategy) |>
      dplyr::distinct()
    
  })
  
  output$strategies <- DT::renderDataTable({
    DT::datatable(
      dat(), 
      options = list(paging = FALSE),
      escape  = FALSE)
    
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
