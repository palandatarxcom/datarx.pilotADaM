#' @title Server that facilitates the mapping of a single data element (column or field) with a simple select UI
#'
#' @param input Shiny input object
#' @param output  Shiny output object
#' @param session Shiny session object
#' @param params parameters object with `data` and `settings` options.
#' @param id Shiny module id
#' @param current_id current selected id
#'
#' @return A reactive containing the selected column
#'
#' @export

OverviewServer <- function(id, params, current_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate list of domains in select
    domains <- reactive({
      req(params()$data)
      names(params()$data)
    })

    observe({
      updateSelectizeInput(
        session,
        inputId = "domainSelect",
        choices = domains()
      )
    })

    id_col <- reactive({
      params()$settings$dm$id_col
    })

    # Make a simple Demographics summary
    demogData <- reactive({
      params()$data$dm |> filter(!!sym(id_col()) == current_id())
    })

    demogHTML <- reactive({
      demogCols <- params()$settings$dm[grep("_col", names(params()$settings$dm))]
      names <- names(demogCols)
      vals <- list(demogCols |> purrr::map_chr(~ as.character(demogData()[1, .x]))) |> unlist()
      names(vals) <- names
      lis <- vals |> purrr::imap(function(val, name) {
        tags$li(
          tags$small(name, class = "dlabel"),
          tags$strong(val, class = "dvalue")
        )
      })
      return(tags$ul(lis, class = "dlist"))
    })

    output$demogList <- renderUI({
      demogHTML()
    })

    # Render table for selected domain/ID
    domain_choice <- reactive({
      req(params()$data)
      req(input$domainSelect)

      params()$data[[input$domainSelect]]
    })
    output$overview <- DT::renderDT({
      domain_choice() |> dplyr::filter(!!sym(id_col()) == current_id())
    })
  })
}
