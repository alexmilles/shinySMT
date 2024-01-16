
smt_ui <- function(){
ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel("SMT-Stationen und weitere Messgeräte der FAWF", "FAWF Messstationen"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      conditionalPanel("output.login_successful == 0",
                       h5("Login-Daten für Zugang zur Testdatenbank_Mike auf MSQL-Server 10.82.6.88 eingeben"),
                       textInput(inputId = "user",
                                 label =  "Nutzername",
                                 placeholder = "Nutzername für Datenbank"),
                       passwordInput(inputId = "pwd",
                                     label = "Passwort",
                                     placeholder = "Passwort für Datenbank"),
                       span(textOutput(outputId = "login_error"),
                            style="color:red"),
                       actionButton(inputId = "login",
                                    label =  "Login")
      ),
      conditionalPanel("output.login_successful == 1",
                       dateRangeInput(inputId = "Datum",
                                      label = "Datum",
                                      start = Sys.time() - 5 * 86500,
                                      end =Sys.time(),
                                      language = "de",
                                      min = "2023-03-01",
                                      max =Sys.time()
                                      ),
                       selectInput(inputId = "Stat",
                                   label = "Stationen",
                                   choices = NULL,
                                   multiple = FALSE),
                       selectInput(inputId = "Para",
                                   label = "Messparameter",
                                   choices = NULL,
                                   multiple = FALSE),
                       conditionalPanel("output.length_spot > 1",
                                        selectInput(
                                          inputId = "Spot",
                                          label = "Messspot",
                                          choices = NULL,
                                          multiple = TRUE)),
                       conditionalPanel("output.length_messposition > 1",
                                        selectInput(inputId = "Messposition",
                                                    label =  "Messposition",
                                                    choices = NULL,
                                                    multiple = TRUE)),
                       conditionalPanel("output.mw_info_true > 0",
                                        actionButton(inputId = "download_mw",
                                                     label = "Messdaten aus Datenbank abrufen",
                                                     icon = icon("download")
                                                     )
                                        )
                       )
      ),
    mainPanel = mainPanel(
      conditionalPanel("output.plot_created > 0",
      shinycssloaders::withSpinner(
        plotOutput(outputId = "mw_plot")
        ),
        downloadButton(
          outputId = "download_local",
          label = "Messdaten herunterladen"
          )
      )
    )

  ),
  h6("Anmerkungen und Fragen an alexander.milles@wald-rlp.de"),
  h6("Code unter https://github.com/alexmilles/shinySMT"),
  imageOutput("fawf_logo")
)
return(ui)

}
