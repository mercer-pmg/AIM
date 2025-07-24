orion_platform <- aws.s3::get_object(
  region = Sys.getenv("AWS_DEFAULT_REGION"),
  key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),

  object = "orion-platform.csv",
  bucket = "aspen-investing-menu") |>
  readBin("character") |>
  readr::read_csv(show_col_types = FALSE)

models <- orion_platform |> 
  dplyr::filter(type == "Multifactor Series") |>
  dplyr::select(model) |>
  dplyr::distinct() |>
  dplyr::arrange(model) |>
  dplyr::pull()

suite_table <- function(suite_name) {
  primary_col   <- "#C00686"
  secondary_col <- "#FFD8EF"
  
  suite <- platform |> 
    dplyr::filter(model == suite_name) |>
    dplyr::mutate(
      product = paste0(product, " (", ticker, ")"),
      target  = target/100*agg_target/100) |>
    dplyr::arrange(dplyr::desc(portfolio))
  
  suite_stats <- suite |>
    dplyr::select(strategy, portfolio) |>
    dplyr::left_join(stats, by = "strategy") |>
    dplyr::select(-strategy) |>
    
    tidyr::pivot_longer(
      cols = -portfolio,
      names_to = "product",
      values_to = "value"
    ) |>
    
    dplyr::distinct() |>
    tidyr::pivot_wider(
      id_cols = "product",
      names_from = "portfolio",
      values_from = "value"
    )
  
  
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
    gt::fmt_percent() 
  
  tbl <- tbl |>
    gt::opt_table_lines(extent = "none") |>
    gt::cols_width(product ~ gt::px(500)) |>
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

