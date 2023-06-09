#' Safety Profile Module - Server
#'
#' @param input module input
#' @param output module output
#' @param session module session
#' @param id Shiny module id
#' @param params parameters object with `data` and `settings` options. {reactive}
#' @param ptid ID to select when module is initialized {reactive}
#'
#' @return returns shiny module Server function
#'
#' @import shiny
#' @export


profile_server <- function(id, params, ptid = reactive({
  NULL
})) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cat("starting server")

    ## set up some basic reactives for convenience
    id_col <- reactive({
      params()$settings$dm$id_col
    })

    ids <- reactive({
      req(params()$data$dm)
      unique(params()$data$dm[[id_col()]])
    })

    ## Update ID Select
    observe({
      updateSelectizeInput(
        session,
        inputId = "idSelect",
        choices = ids(),
        selected = ptid()
      )
    })

    current_id <- reactive({
      input$idSelect
    })

    ## Call  Modules
    OverviewServer("overview", params, current_id)
    ae_plot_server("ae_plot", params, current_id)
    react_server("react", params, current_id)

    return(current_id)
  })
}
