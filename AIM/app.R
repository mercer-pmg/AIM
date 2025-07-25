library(shiny)

orion_platform <- readr::read_csv("orion_platform.csv", show_col_types = FALSE)

ui <- bslib::page_navbar( 
  

  
  # shiny::includeCSS("./www/custom.css"),
  
  title = "Aspen Investing Menu", 
  id    = "page", 
  
  bslib::nav_panel(
    title = "Search",
    icon  = shiny::icon("magnifying-glass"),
    # theme = bslib::bs_theme(bootswatch = "minty"),
    
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
          inputId = "status",
          label   = "Strategy Type",
          choices = c("Recommended", "Approved")
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
          selectize = TRUE),
        
        shiny::actionButton(
          inputId = "search_go",
          label   = "Search"
        )
      ),
      
      shiny::mainPanel(
        
        shiny::selectInput(
          inputId = "order_by",
          label   = "Order By",
          choices = c(
            "Strategy Type"                     = 1,
            "Acct Min - Highest to Lowest"      = 2,
            "Acct Min - Lowest to Highest"      = 3,
            "Expense Ratio - Highest to Lowest" = 4,
            "Expense Ratio - Lowest to Highest" = 5,
            "Yield - High to Low"               = 6,
            "Yield - Low to High"               = 7)
        ),
        
        shinycssloaders::withSpinner(
          shiny::uiOutput(outputId = "boxes")
        )
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
  
  includeCSS("www/card-reveal-full-screen.css")
  
  
) 

server <- function(input, output) {
  
  orion_platform <- readr::read_csv("orion_platform.csv", show_col_types = FALSE)
  
  order_strategies <- function(ls, order_type, platform) {
    
    strategies <- names(ls$children[[1]])
    
    print(strategies)
    
    platform <- platform |>
      dplyr::filter(strategy %in% strategies) |>
      dplyr::select(strategy, fee, yield, minimum, status) |>
      dplyr::distinct()
    
    ordered_strategies <- platform 
    
    if(order_type == 1) {
      ordered_strategies <- platform |> dplyr::arrange(dplyr::desc(status))
    }
    
    if(order_type == 2){
      ordered_strategies <- platform |> dplyr::arrange(dplyr::desc(minimum))
    }
    
    if(order_type == 3) {
      ordered_strategies <- platform |> dplyr::arrange(minimum)
    }
    
    if(order_type == 4) {
      ordered_strategies <- platform |> dplyr::arrange(dplyr::desc(fee))
    }
    
    if(order_type == 5) {
      ordered_strategies <- platform |> dplyr::arrange(fee)
    }
    
    if(order_type == 6) {
      ordered_strategies <- platform |> dplyr::arrange(dplyr::desc(yield))
    }
    
    if(order_type == 7) {
      ordered_strategies <- platform |> dplyr::arrange(yield)
    }
    
    ordered_strategies <-  ordered_strategies |> dplyr::pull(strategy)
    
    print(ordered_strategies)
    
    ls$children[[1]] <- ls$children[[1]][ordered_strategies]
    
    ls
    
  }
  
  strat <- reactive({
    
    if(length(input$strategies) == 0) {
      unique(orion_platform$strategy)
    } else {
      input$strategies
    }
    
  })
  
  acct_min <- reactive({
    
    print(input$account_value)
    
    if(is.na(input$account_value)){
      Inf
    } else {
      input$account_value
    }
  })
  
  status <- reactive({
    if(length(input$status) == 0) {
      c("Approved", "Recommended")
    } else {
      input$status
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
  
  
  search_menu <- shiny::eventReactive(input$search_go, {
    
    cards <- orion_platform |>
      # order_strategies(input$order_by) |>
      dplyr::filter(strategy %in% strat()) |>
      dplyr::filter(minimum <= acct_min()) |>
      dplyr::filter(status %in% status()) |>
      dplyr::filter(type %in% cat()) |>
      dplyr::filter(portfolio %in% equity_allo()) |>
      dplyr::filter(tax_managed %in% tax_mgmt()) |>
      dplyr::filter(model_agg %in% models()) |>
      dplyr::select(strategy) |>
      dplyr::distinct() |>
      dplyr::pull() 
    
  }
  )
  
  strategy_boxes <- reactive({
    
    strategy_names <- search_menu()
    
    boxes <- strategy_names |>
      purrr::map(strategyCardUI, platform = orion_platform)
    
    bslib::layout_column_wrap(
      width         = "200px",
      heights_equal = "all",
      !!!boxes
    )
  })
  
  
  
  output$boxes <- renderUI({
    
    xx <- strategy_boxes()
    
    names(xx$children[[1]]) <- search_menu()
    
    xx |> order_strategies(input$order_by, orion_platform)
  })
  
  output$table1 <- gt::render_gt(
    expr  = suite_table("Multifactor (ETF)", platform = orion_platform), 
    width = "100%")
  output$table2 <- gt::render_gt(
    expr  = suite_table("Multifactor TM (ETF)", platform = orion_platform), 
    width = "100%")
  
}


# Run the application 
shinyApp(ui = ui, server = server)
