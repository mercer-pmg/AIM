search_platform_ui <- function(orion_platform) {
  
  shiny::tagList(
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        width = 3,
        
        shinyWidgets::numericInputIcon(
          inputId = "account_value",
          label   = shiny::strong("Strategy Minimum"),
          value   = NULL,
          min     = 0,
          icon    = "$"
        ),
        
        checkboxGroupInput(
          inputId      = "tm_status",
          label        = shiny::strong("Tax-Managed"),
          choiceNames  = c("Yes", "No"),
          choiceValues = c(TRUE, FALSE),
          inline       = TRUE),
        
        shiny::checkboxGroupInput(
          inputId  = "status",
          label    = shiny::strong("Investment Committtee Status"),
          choices  = c(
            "Recommended", 
            "Approved"),
          inline = TRUE,
          selected = "Recommended"
        ),
        
        shiny::checkboxGroupInput(
          inputId = "strategy_type",
          label   = shiny::strong("Strategy Type"),
          choices = c(
            "Risk-Based Strategies",
            "Asset Class Strategies",
            "Other Strategies"),
          inline = TRUE
        ),
        
        shiny::checkboxGroupInput(
          inputId = "strategy_subtype",
          label   = shiny::strong("Strategy Subtype"),
          choices = c(
            "Market Series", 
            "Multifactor Series", 
            "Income Series",
            "Equity Strategies", 
            "Fixed Income Strategies", 
            "Alternative Strategies", 
            "Cash Strategies", 
            "Special Situation Strategies", 
            "Blended Strategy")
        ),
        
        shinyjs::hidden(
          shiny::sliderInput(
            input     = "equityAlloc",
            label     = shiny::strong("Equity Allocation Range"),
            min       = 0,
            max       = 100,
            value     = c(0,100),
            step      = 10,
            dragRange = TRUE
          )
        ),
        
        
        shiny::selectInput(
          inputId   = "models",
          label     = shiny::strong("Search by Model"),
          choices   = orion_platform$model_agg |> unique(),
          multiple  = TRUE,
          selectize = TRUE),
        
        shiny::actionButton(
          inputId = "search_go",
          label   = "Search"
        )
      ),
      
      shiny::mainPanel(
        
        shiny::fluidRow(
          shiny::column(
            width = 9,
            shiny::selectInput(
              inputId = "order_by",
              label   = "Order By",
              choices = c(
                "Investment Committee Status"       = 1,
                "Acct Min - Highest to Lowest"      = 2,
                "Acct Min - Lowest to Highest"      = 3,
                "Expense Ratio - Highest to Lowest" = 4,
                "Expense Ratio - Lowest to Highest" = 5,
                "Yield - High to Low"               = 6,
                "Yield - Low to High"               = 7)
            )
          ),
          
          shiny::column(
            width = 3,
            shiny::textOutput("message")
          )
        ),
        
        shinycssloaders::withSpinner(
          shiny::uiOutput(outputId = "boxes")
        )
      )
    )
  )
  
  
  
}

search_platform_server <- function(input, output, df = orion_platform) {
  
  # Define strategy type/subtype relationships
  risk_based <- c("Market Series", 
                  "Multifactor Series", 
                  "Income Series")
  
  asset_class <- c("Equity Strategies", 
                   "Fixed Income Strategies", 
                   "Cash Strategies", 
                   "Alternative Strategies")
  
  other <- c("Special Situation Strategies",
             "Blended Strategy")
  
  
  orion_platform <- df |>
    
    dplyr::mutate(
      
      subtype = type,
      
      type = "Other Strategies",
      
      type = dplyr::if_else(
        condition = subtype %in% risk_based,
        true      = "Risk-Based Strategies",
        false     = type
      ),
      
      type = dplyr::if_else(
        condition = subtype %in% asset_class,
        true      = "Asset Class Strategies",
        false     = type
      ),
      
      type = dplyr::if_else(
        condition = subtype %in% other,
        true      = "Other Strategies",
        false     = type
      )
    )
  
  # Filter platform based on sidebar inputs
  search_menu <- shiny::eventReactive(input$search_go, {
    
    cards <- orion_platform |>
      dplyr::filter(strategy %in% strat()) |>
      dplyr::filter(minimum <= acct_min()) |>
      dplyr::filter(status %in% status()) |>
      dplyr::filter(type %in% strat_type()) |>
      dplyr::filter(subtype %in% strat_subtype()) |>
      dplyr::filter(portfolio %in% equity_allo()) |>
      dplyr::filter(tax_managed %in% tax_mgmt())
    
    if(!is.null(models())) {
      cards <- cards |> search_models_all(models())
    }
    
    cards <- cards |>
      dplyr::select(strategy) |>
      dplyr::distinct() |>
      dplyr::pull()
    
  })
  
  # Create value boxes
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
  
  # Order value boxes based on input$order_by
  output$boxes <- renderUI({
    
    xx <- strategy_boxes()
    
    names(xx$children[[1]]) <- search_menu()
    
    xx |> order_strategies(input$order_by, orion_platform)
    
  })
  
  output$message <- renderText(
    paste(length(search_menu()), "strategies returned."))
  
  
  
  # Filtering functions----
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
  
  strat_type <- reactive({
    
    if(is.null(input$strategy_type)) {
      unique(orion_platform$type)
    } else {
      input$strategy_type
    }
    
  })
  
  strat_subtype <- reactive({
    
    if(length(input$strategy_subtype) == 0) {
      unique(orion_platform$subtype)
    } else {
      input$strategy_subtype
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
      unique(df$tax_managed)
    } else {
      input$tm_status
    }
    
  })
  
  models <- reactive({
    
    if(is.null(input$models)) {
      NULL
    } else {
      input$models
    }
    
  })
  
  
  
  
  # UI updates----
  
  # Update the list of available strategy sub types
  shiny::observeEvent(
    
    ignoreNULL = FALSE,
    
    eventExpr   = input$strategy_type, 
    handlerExpr = {
      
      if(is.null(input$strategy_type)) {
        
        shiny::updateCheckboxGroupInput(
          inputId = "strategy_subtype",
          choices = c(risk_based, asset_class, other)
        )
        
      } else {
        
        subtypes <- c()
        
        if("Risk-Based Strategies" %in% input$strategy_type) {
          subtypes <- c(subtypes, risk_based)
          
        } 
        
        if("Asset Class Strategies" %in% input$strategy_type) {
          subtypes <- c(subtypes, asset_class)
        }
        
        if("Other Strategies" %in% input$strategy_type) {
          subtypes <- c(subtypes, other)
        }
        
        shiny::updateCheckboxGroupInput(
          inputId = "strategy_subtype",
          choices = subtypes
        )
        
      }
    }
  )
  
  # Show/hide equity allocation range widget
  shiny::observeEvent(
    
    ignoreNULL = FALSE,
    
    eventExpr   = input$strategy_type,
    handlerExpr = {
      
      if(is.null(input$strategy_type)) {
        shinyjs::show(id = "equityAlloc")
      } else {
        if("Risk-Based Strategies" %in% input$strategy_type) {
          shinyjs::show(id = "equityAlloc")
        } else {
          shinyjs::hide(id = "equityAlloc")
        }
      }
    }
    
  )
  
  shiny::observeEvent(
    
    ignoreNULL  = FALSE,
    eventExpr   = input$strategy_subtype,
    handlerExpr = {
      
      if(is.null(input$strategy_subtype)) {
        shinyjs::show(id = "equityAlloc")
      } else {
        if("Market Series" %in% input$strategy_subtype) {
          shinyjs::show(id = "equityAlloc")
        } else {
          if("Multifactor Series" %in% input$strategy_subtype) {
            shinyjs::show(id = "equityAlloc")
          } else {
            if("Income Series" %in% input$strategy_subtype) {
              shinyjs::show(id = "equityAlloc")
            } else {
              shinyjs::hide(id = "equityAlloc")
            }
          }
        }
      }
    }
  )
}
