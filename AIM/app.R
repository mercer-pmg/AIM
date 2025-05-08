

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Aspen Investing Menu"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        
        inputId = "strategy_types", 
        
        label   = "Type", 
        
        choiceNames = c(
          "Market", 
          "Multifactor", 
          "Income", 
          "Equity", 
          "Fixed Income"),
        
        choiceValues = c(
          "Market Series", 
          "Multifactor Series", 
          "Income Series", 
          "Equity Strategies", 
          "Fixed Income Strategies"),
        
        inline = TRUE),
      
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
  
  
  
  platform <- aws.s3::get_object(
    region = Sys.getenv("AWS_DEFAULT_REGION"),
    key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    
    object = "orion-platform.csv", 
    bucket = "aspen-investing-menu") |>
    readBin("character") |>
    readr::read_csv(show_col_types = FALSE) |>
    dplyr::select(strategy, type, model, portfolio, tax_managed) |>
    dplyr::distinct()
  
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
  
  dat <- reactive({
    df <- platform |>
      dplyr::filter(type %in% types()) |>
      dplyr::filter(tax_managed %in% tax_mgmt())
    
    df
  })
  
  output$strategies <- DT::renderDataTable(
    DT::datatable(dat(), options = list(paging = FALSE))
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
