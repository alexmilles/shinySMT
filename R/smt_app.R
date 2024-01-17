#Start the SMT Shiny App

#' @export
smt_app <- function(){
  Sys.setenv(tz="Etc/GMT-1")
  shiny::shinyApp(smt_ui(), smt_server)
}
