

library(shiny)

createLink <- function(val) {
  sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">More</a>',val)
}

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

ui <- fluidPage(
  
  # Application title
  titlePanel("Aspen Investing Menu"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      checkboxGroupInput(
        inputId = "strategy_category",
        label   = "Category",
        choices = platform$category |> unique(),
        inline  = TRUE
      ),
      
      checkboxGroupInput(
        
        inputId = "strategy_types", 
        
        label   = "Type", 
        
        choices = c(
          "Market Series",
          "Multifactor Series",
          "Income Series",
          "Equity Strategies",
          "Fixed Income Strategies")),
      
      checkboxGroupInput(
        inputId = "tm_status",
        label   = "Tax-Managed",
        choiceNames = c("Yes", "No"),
        choiceValues = c(TRUE, FALSE),
        inline = TRUE)
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      DT::DTOutput("strategies")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # UI updates
  shiny::observeEvent(input$strategy_category, {
    shiny::updateCheckboxGroupInput(
      inputId = "strategy_types",
      choices = platform |> 
        dplyr::filter(category %in% input$strategy_category) |> 
        dplyr::pull(type) |>
        unique()
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
  
  tax_mgmt <- reactive({
    
    if(length(input$tm_status) == 0) {
      unique(platform$tax_managed)
    } else {
      input$tm_status
    }
    
  })
  
  # Filtering platform based on filtering functions
  dat <- reactive({
    
    df <- platform |>
      dplyr::filter(category %in% cat()) |>
      dplyr::filter(type %in% types()) |>
      dplyr::filter(tax_managed %in% tax_mgmt()) 
    
    df
    
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
