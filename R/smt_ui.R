
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
                                      start = Sys.time() - 14 * 86500,
                                      end =Sys.time(),
                                      language = "de",
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
                       shinyWidgets::dropdownButton(
                         inputId = "showAdvancedOptions",
                         label = "Erweitere Optionen",
                         icon = icon("sliders"),
                         circle = FALSE,
                         checkboxInput(inputId = "Quali",
                                       label = "Nur Rohdaten",
                                       value = TRUE),
                         selectInput(inputId = "agg_fun",
                                     label = "Aggregierung",
                                     choices = c("none", "mean", "min", "max") |>
                                       setNames(c("Keine Aggregierung",
                                                  "Mittelwert",
                                                  "Minimum",
                                                  "Maximum")),
                                     selected = "none"),
                         conditionalPanel("input.agg_fun != 'none'",
                                          numericInput(inputId = "agg_time",
                                                       label = "Zeitliche Auflösung (Tage)",
                                                       value = 1,
                                                       min = 1,
                                                       max = 365)
                         )
                       ),
                       conditionalPanel("output.mw_info_true > 0",
                                        actionButton(inputId = "download_mw",
                                                     label = "Datenbankabfrage",
                                                     icon = icon("database"),
                                                     width = "100%"
                                                     )
                                        )
                       ),
      width = 2
      ),
    mainPanel = mainPanel(
      conditionalPanel("output.plot_created > 0",
                       tabsetPanel(
                         tabPanel(
                           title = "Messwerte",
                           shinycssloaders::withSpinner(
                             plotOutput(outputId = "mw_plot",
                                        height = "700px")
                           )
                           ),
                         tabPanel(
                            title = "Schnellübersicht: Gerätestatus (Tage seit letzter Übertragung)",
                            checkboxInput("include_old",
                                          label = "Signale älter als 14 Tage anzeigen (langsamer)?",
                                          value = TRUE),
                            shinycssloaders::withSpinner(
                              plotOutput(outputId = "health_plot", height = "1200px")
                              )
                            ),
                         tabPanel(
                           title = "Schnellübersicht: Aktuellste Messwerte",
                           checkboxInput("include_old",
                                         label = "Signale älter als 14 Tage anzeigen (langsamer)?",
                                         value = TRUE),
                           shinycssloaders::withSpinner(
                             ggiraph::girafeOutput(outputId = "last_value_plot", height = "1200px")
                           )
                         )
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
