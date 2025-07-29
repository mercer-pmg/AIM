library(shiny)

orion_platform <- readr::read_csv("orion_platform.csv", show_col_types = FALSE)

ui <- bslib::page_navbar( 
  
  tags$script(HTML("
  document.addEventListener('shiny:connected', function() {
    document.querySelectorAll('.bslib-value-box').forEach(function(box) {
      const id = box.getAttribute('data-id');
      if (!id) return;

      const observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
          if (mutation.attributeName === 'data-full-screen') {
            const isFullScreen = box.getAttribute('data-full-screen') === 'true';
            Shiny.setInputValue('value_box_full_screen_' + id, isFullScreen, {priority: 'event'});
          }
        });
      });

      observer.observe(box, { attributes: true });
    });
  });
")),
  
  
  title = "Aspen Investing Menu", 
  id    = "page", 
  
  bslib::nav_panel(
    title = "Search",
    icon  = shiny::icon("magnifying-glass"),
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        width = 3,
        
        shiny::checkboxGroupInput(
          inputId = "strategy_category",
          label   = "Category",
          choices = c("Market Series", "Multifactor Series", "Income Series",
                      "Equity Strategies", "Fixed Income Strategies", 
                      "Alternative Strategies", "Cash Strategies", 
                      "Special Situation Strategies", "Blended Strategy")),
      
        
        shiny::actionButton(
          inputId = "search_go",
          label   = "Search"
        )
      ),
      
      shiny::mainPanel(
        
        shinycssloaders::withSpinner(
          shiny::uiOutput(outputId = "boxes")
        )
      )
    )
  )
) 

server <- function(input, output) {
  
  cat <- reactive({
    
    if(length(input$strategy_category) == 0) {
      unique(orion_platform$type)
    } else {
      input$strategy_category
    }
    
  })

  
  
  search_menu <- shiny::eventReactive(input$search_go, {
    
    cards <- orion_platform |>
      dplyr::filter(type %in% cat()) |>
      dplyr::select(strategy) |>
      dplyr::distinct() |>
      dplyr::pull() 
    
  }
  )
  
  output$boxes <- renderUI({
    strategy_names <- search_menu()
    
    # Force reactivity on all full screen inputs
    lapply(strategy_names, function(id) {
      input[[paste0("value_box_full_screen_", id)]]
    })
    
    primary_col   <- "#1E90FF"
    secondary_col <- "#D5EEFF"
    
    boxes <- lapply(strategy_names, function(id) {
      local({
        box_id <- id
        is_full_screen <- input[[paste0("value_box_full_screen_", box_id)]] %||% FALSE
        
        tagAppendAttributes(
          bslib::value_box(
            title = box_id,
            value = " ",
            fill  = TRUE,
            showcase = if (is_full_screen) "two" else "one",
            full_screen = TRUE,
            theme = bslib::value_box_theme(
              bg = primary_col,
              fg = secondary_col
            ),
            card_body(class = "card-reveal-full-screen", p("Details shown in full screen."))
          ),
          `data-id` = box_id
        )
      })
    })
    
    bslib::layout_column_wrap(
      width = "200px",
      heights_equal = "all",
      !!!boxes
    )
  })
  
  
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
