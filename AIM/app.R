library(shiny)

orion_platform <- readr::read_csv("orion_platform.csv")

ui <- bslib::page_navbar( 
  
  # shiny::includeCSS("./www/custom.css"),

  title = "Aspen Investing Menu", 
  id    = "page", 
  
  bslib::nav_panel(
    title = "Search",
    icon  = shiny::icon("magnifying-glass"),
    theme = bslib::bs_theme(bootswatch = "minty"),
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        width = 3,
        
        shiny::selectInput(
          inputId   = "strategies",
          label     = "Search by strategy name",
          choices   = orion_platform$strategy |> unique(),
          multiple  = TRUE,
          selectize = TRUE),
        
        shinyWidgets::numericInputIcon(
          inputId = "account_value",
          label   = "Account Value",
          value   = NULL,
          min     = 0,
          icon    = "$"
        ),
        
        shiny::checkboxGroupInput(
          inputId = "strategy_category",
          label   = "Category",
          choices = c("Market Series", "Multifactor Series", "Income Series",
                      "Equity Strategies", "Fixed Income Strategies", 
                      "Alternative Strategies", "Cash Strategies", 
                      "Special Situation Strategies", "Blended Strategy")),
        
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
        
        shiny::selectInput(
          inputId   = "models",
          label     = "Search by model",
          choices   = orion_platform$model_agg |> unique(),
          multiple  = TRUE,
          selectize = TRUE)
      ),
      
      shiny::mainPanel(
        shiny::uiOutput(outputId = "boxes")
        
      )
    )
    
  ),
  
  bslib::nav_panel(
    title = "Market Series"),
  
  bslib::nav_panel(
    title = "Multifactor Series", 
    
    shiny::fluidPage(
      gt::gt_output(outputId = "table1"),
      gt::gt_output(outputId = "table2")
    )
  ), 
  
  bslib::nav_panel(
    title = "Income Series", 
    "Income Series content"), 
  
  
) 

server <- function(input, output) {
  
  orion_platform <- readr::read_csv("orion_platform.csv")
  
  strat <- reactive({
    
    if(length(input$strategies) == 0) {
      unique(orion_platform$strategy)
    } else {
      input$strategies
    }
    
  })
  
  acct_min <- reactive({
    if(is.null(input$acct_value)){
      Inf
    } else {
      input$acct_value
    }
  })

  cat <- reactive({
    
    if(length(input$strategy_category) == 0) {
      unique(orion_platform$type)
    } else {
      input$strategy_category
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
      unique(orion_platform$tax_managed)
    } else {
      input$tm_status
    }
    
  })
  
  models <- reactive({
    
    if(length(input$models) == 0) {
      unique(orion_platform$model_agg)
    } else {
      input$models
    }
    
  })
  
  output$table1 <- gt::render_gt(
    expr  = suite_table("Multifactor (ETF)", platform = orion_platform), 
    width = "100%")
  output$table2 <- gt::render_gt(
    expr  = suite_table("Multifactor TM (ETF)", platform = orion_platform), 
    width = "100%")
  
  output$boxes <- renderUI({

    cards <- orion_platform |>
      dplyr::filter(strategy %in% strat()) |>
      dplyr::filter(minimum <= acct_min()) |>
      dplyr::filter(type %in% cat()) |>
      dplyr::filter(portfolio %in% equity_allo()) |>
      dplyr::filter(tax_managed %in% tax_mgmt()) |>
      dplyr::filter(model_agg %in% models()) |>
      dplyr::select(strategy) |>
      dplyr::distinct() |>
      dplyr::pull() |>
      head(200) |>
      purrr::map(strategyCardUI, platform = orion_platform)
  
    bslib::layout_column_wrap(
      width         = "200px",
      heights_equal = "all",
      !!!cards)

  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
