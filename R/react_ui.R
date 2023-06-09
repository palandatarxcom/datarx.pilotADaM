#' UI that facilitates the Safety reactable tab that includes safety reactable
#'
#' @param id module id
#'
#' @return LB reactable tab UI
#'
react_UI <- function(id){
  ns <- NS(id)

  div(
    h5(htmlOutput(ns("Lab Tests"))),
    reactableOutput(ns("react"))
  )
}
