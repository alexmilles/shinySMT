#Start the SMT Shiny App

#' @export
smt_app <- function(){
  shiny::shinyApp(smt_ui(), smt_server)
}
