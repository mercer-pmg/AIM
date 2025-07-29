order_strategies <- function(ls, order_type, orion_platform) {
  
  strategies <- names(ls$children[[1]])
  
  print(strategies)
  
  platform <- orion_platform |>
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
  
  return(ls)
  
}


search_models_all <- function(df, model_list) {
  
  df <- df
  
  if(length(model_list) > 0 ) {
    for(i in 1:length(model_list)) {
      
      dat <- df |> 
        dplyr::filter(model_agg == model_list[i]) |> 
        dplyr::distinct() |> 
        dplyr::pull(strategy) 
      
      df <- df |> dplyr::filter(strategy %in% dat)
      
    }
  } 
  
  return(df)
  
}

# Define strategy type/subtype relationships
risk_based <- c("Market Series",
                "Multifactor Series",
                "Income Series")

asset_class <- c("Equity Strategies",
                 "Fixed Income Strategies",
                 "Cash Strategies",
                 "Alternative Strategies")

other <- c("Special Situation Strategies",
           "Blended Strategies")