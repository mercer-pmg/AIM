## Module functions
suiteTableUI <- function(id) {
  shiny::tagList(
    gt::gt_output(outputId = shiny::NS(id, "table"))
  )
}

suiteTableServer <- function(id, platform = orion_platform) {
  shiny::moduleServer(id, function(input, output, session) {
    gt::render_gt(expr = suite_table(id), width = "100%")
  }
  )
}