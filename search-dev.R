library(shiny)

strategy_aum <- aws.s3::get_object(
  region = Sys.getenv("AWS_DEFAULT_REGION"),
  key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  
  object = "strategies-aum.csv", 
  bucket = "aspen-investing-menu") |>
  readBin("character") |>
  readr::read_csv(show_col_types = FALSE)

orion_platform <- kdot::get_platform()

orion_platform <- orion_platform |>
  
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

orion_platform <- dplyr::left_join(
  x = orion_platform,
  y = strategy_aum,
  by = dplyr::join_by("strategy" == "Strategy Name")
)


category_list <- unique(orion_platform$category)
type_list     <- unique(orion_platform$type)
model_list    <- orion_platform |>
  dplyr::select(model_group, model_agg) |>
  dplyr::arrange(model_agg) |>
  dplyr::distinct() |>
  dplyr::group_by(model_group) |>
  dplyr::summarise(model_ls = list(model_agg)) |>
  tibble::deframe()

category_list <- unique(platform$type)

## Module functions
suiteTableUI <- function(id) {
  shiny::tagList(
    gt::gt_output(outputId = shiny::NS(id, "table"))
  )
}

suiteTableServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    suite_table  <- function(suite_name) {
      print(suite_name)
      
      if(stringr::str_detect(suite_name, "Market")){
        primary_col   <- "#7673DC"
        secondary_col <- "#E9E9FF"
      } else {
        if(stringr::str_detect(suite_name, "Multifactor")){
          primary_col   <- "#C00686"
          secondary_col <- "#FFD8EF"
        } else {
          if(stringr::str_detect(suite_name, "Income")) {
            primary_col   <- "#454759"
            secondary_col <- "#F3F4F6"
          }
        }
      }
      
      suite <- platform |>
        dplyr::filter(model == suite_name) |>
        dplyr::mutate(
          product = paste0(product, " (", ticker, ")"),
          target  = target/100*agg_target/100) |>
        dplyr::arrange(dplyr::desc(portfolio))
      
      model_aggs <- suite |>
        dplyr::select(model_agg, product) |>
        dplyr::distinct() |>
        dplyr::group_by(model_agg) |>
        dplyr::summarise(products = dplyr::n()) |>
        dplyr::mutate(
          end_row   = products |> cumsum(),
          start_row = end_row |> dplyr::lag() |> tidyr::replace_na(0),
          start_row = start_row + 1
        )
      
      suite_wide <- suite |>
        dplyr::select(model_agg, portfolio, product, target) |>
        tidyr::pivot_wider(
          id_cols     = c(model_agg, product),
          names_from  = portfolio,
          values_from = target,
          values_fill = 0)
      
      
      tbl <- gt::gt(
        data          = suite_wide,
        rowname_col   = "product",
        groupname_col = "model_agg") |>
        gt::tab_header(title = suite_name) |>
        gt::fmt_percent() |>
        gt::cols_width(
          product ~ gt::px(75),
          dplyr::everything() ~ gt::pct(5))
      
      tbl <- tbl |>
        gt::opt_table_lines(extent = "none") |>
        gt::opt_table_outline(color = primary_col) |>
        
        # Table title style
        gt::tab_style(
          style     = list(
            gt::cell_fill(color = primary_col),
            gt::cell_text(color  = "white",
                          font   = "Merriweather",
                          size   = gt::px(35),
                          align  = "left",
                          weight = "bold")),
          locations = gt::cells_title()
        ) |>
        
        # Column labels
        gt::tab_style(
          style = list(
            gt::cell_text(
              font = "Merriweather",
              size = gt::px(20),
              weight = "bold")
          ),
          locations = list(gt::cells_column_labels())
        ) |>
        
        
        # Rowname group style
        gt::tab_style(
          style = list(
            gt::cell_fill(color = secondary_col),
            gt::cell_text(weight = "bold")
          ),
          locations = gt::cells_row_groups()
        ) |>
        
        # Stub style
        gt::tab_style(
          style = list(gt::cell_text(font = "IBM Plex Sans")),
          locations = gt::cells_stub()
        ) |>
        
        gt::tab_stub_indent(indent = 2, rows = gt::everything())
      
      
      tbl <- tbl |>
        gt::cols_align("center", columns = 3:ncol(suite_wide))
      
      
      return(tbl)
    }
    
    output$table <- gt::render_gt(expr = suite_table(id), width = "100%")
    
  }
  )
}


# Strategy card module functions
strategyCardUI <- function(id) {
  
  print(id)
  
  if(stringr::str_detect(id, "Market")){
    primary_col   <- "#7673DC"
    secondary_col <- "#E9E9FF"
  } else {
    if(stringr::str_detect(id, "Multifactor")){
      
      primary_col   <- "#C00686"
      secondary_col <- "#FFD8EF"
    } else {
      if(stringr::str_detect(id, "Income")) {
        primary_col   <- "#454759"
        secondary_col <- "#F3F4F6"
      }
    }
  }
  
    
    bslib::value_box(
      title = id,
      value = "XX",
      fill  = TRUE,
      p("Yield:"),
      p("Expense Ratio:"),
      p("Minimum:"),
      showcase = "M",
      full_screen = TRUE,
      theme = bslib::value_box_theme(
        bg = primary_col,
        fg = secondary_col
      )
    )
}

strategyCardServer <- function(id) {
  strategy_box <- renderUI({
    
    bslib::value_box(
      title = id,
      value = "XX",
      fill  = TRUE,
      p("Yield:"),
      p("Expense Ratio:"),
      p("Minimum:"),
      showcase = "M",
      full_screen = TRUE,
      theme = bslib::value_box_theme(
        bg = primary_col,
        fg = secondary_col
      )
    )
    
  })
}


ui <- bslib::page_navbar( 
  
  title = "Aspen Investing Menu", 
  id = "page", 
  
  # Search tab
  bslib::nav_panel(
    title = "Search",
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        "Search parameters",
        
        shiny::checkboxGroupInput(
          inputId = "strategy_category",
          label   = "Category",
          choices = category_list),
        
        shiny::sliderInput(
          input     = "equityAlloc",
          label     = "Equity Allocation Range",
          min       = 0,
          max       = 100,
          value     = c(0,100),
          step      = 10,
          dragRange = TRUE)
          
        ),
      
      shiny::mainPanel(
        bslib::layout_column_wrap(
          width = "200px",
          heights_equal = "all",

          !!!platform |>
            dplyr::filter(type %in% c("Market Series", "Multifactor Series", "Income Series")) |>
            dplyr::select(strategy) |>
            dplyr::distinct() |>
            dplyr::pull() |>
            head(50) |>
            purrr::map(strategyCardUI)
          
        )
      )
    )
  ),
  
  
  # Market Series tab
  bslib::nav_panel(
    title = "Market Series",
    shiny::fluidPage(
      platform |>
        dplyr::select(model) |>
        dplyr::filter(stringr::str_detect(model, "^Market")) |>
        dplyr::distinct() |>
        dplyr::pull() |>
        purrr::map(suiteTableUI)
    )),
  
  # Multifactor Series tab
  bslib::nav_panel(
    "Multifactor Series", 
    shiny::fluidPage(
      
      platform |>
        dplyr::select(model) |>
        dplyr::filter(stringr::str_detect(model, "^Multifactor")) |>
        dplyr::distinct() |>
        dplyr::pull() |>
        purrr::map(suiteTableUI)
      
      
    )
  ), 
  
  # Income Series tab
  bslib::nav_panel(
    "Income Series", 
    shiny::fluidPage(
      platform |>
        dplyr::select(model) |>
        dplyr::filter(stringr::str_detect(model, "^Income")) |>
        dplyr::distinct() |>
        dplyr::pull() |>
        purrr::map(suiteTableUI)
    )), 
  
  
) 

server <- function(input, output) {
  
  # Filtering functions
  equity_allo <- reactive({
    
    eq_range <- seq(
      from = input$equityAlloc[1], 
      to   = input$equityAlloc[2], 
      by   = 10)
    
    c(NA, eq_range)
    
  })
  
  output$strategies <- renderUI({
    
  })
  


}


# Run the application 
shinyApp(ui = ui, server = server)
