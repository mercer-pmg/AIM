# Strategy card module functions
strategyCardUI <- function(id, platform) {
  
  print(id)
  
  # platform <- readr::read_csv("orion_platform.csv")
  
  fee <- platform |>
    dplyr::filter(strategy == id) |>
    dplyr::mutate(fee = scales::label_percent(accuracy = 0.01)(fee)) |>
    dplyr::select(fee) |>
    dplyr::distinct() |>
    dplyr::pull()
  
  yield <- platform |>
    dplyr::filter(strategy == id) |>
    dplyr::mutate(yield = scales::label_percent(accuracy = 0.01)(yield)) |>
    dplyr::select(yield) |>
    dplyr::distinct() |>
    dplyr::pull() 
  
  minimum <- platform |>
    dplyr::filter(strategy == id) |>
    dplyr::mutate(minimum = scales::label_currency()(minimum)) |>
    dplyr::select(minimum) |>
    dplyr::distinct() |>
    dplyr::pull()
  
  primary_col <- "#1E90FF"
  secondary_col <- "#D5EEFF"
  
  if(stringr::str_detect(id, "^Market")){
    primary_col   <- "#7673DC"
    secondary_col <- "#E9E9FF"
  } else {
    if(stringr::str_detect(id, "^Multifactor")){
      
      primary_col   <- "#C00686"
      secondary_col <- "#FFD8EF"
    } else {
      if(stringr::str_detect(id, "^Income")) {
        primary_col   <- "#454759"
        secondary_col <- "#F3F4F6"
      } else {
        if(stringr::str_detect(id, "Blended")) {
          primary_col <- "#800080"
          secondary_col <- "#d9d2e9"
        }
      }
    }
  }
  
  
  bslib::value_box(
    title = id,
    value = " ",
    fill  = TRUE,
    p(paste0("Yield: ", yield)),
      p(paste0("Expense Ratio: ", fee)),
      p(paste0("Minimum: ", minimum)),
      showcase = shiny::icon("star"),
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
      showcase = "star",
      full_screen = TRUE,
      theme = bslib::value_box_theme(
        bg = primary_col,
        fg = secondary_col
      )
    )
    
  })
}