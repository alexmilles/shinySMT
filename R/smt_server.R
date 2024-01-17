smt_server <- function(input, output, session){

  # render logo
  output$fawf_logo <- renderImage({
    list(src = system.file("www/FAWF_Schriftzug_Transparent.png",
                           package = "shinySMT"),
         width = 350,
         height = 50)},
    deleteFile = FALSE)

  # define login credentials
  login_credentials <- reactiveValues(df = data.frame(
    server = "klikomz",
    database = "Testdatenbank_Mike",
    user = NA,
    pwd = NA,
    local = FALSE
  ))

  # handle user provided login information
  login_successful <- reactiveVal(FALSE)

  # set login_successful to true or false depending on login_credentials
  observe({
    login_credentials$df$pwd <- input$pwd
    login_credentials$df$user <- input$user

    if(canConnect(login_credentials$df)){
      login_successful(TRUE)
    }else{
      output$login_error <- renderText("Fehlerhafte Login-Daten!")
      login_successful(FALSE)
    }
  }) |>
    bindEvent(input$login)

  # return output to update conditional panel in UI
  output$login_successful <- reactive(as.numeric(login_successful()))
  outputOptions(output, "login_successful", suspendWhenHidden = FALSE)

  # download static tables after succesful login
  stbls <- reactive({
    if(login_successful()){
      download_static_tables(login_credentials = login_credentials$df)
    }
  })

  # update Stat based on stbls
  observe({
     updateSelectInput(session,
                       inputId =  "Stat",
                       label = "Stationen",
                       choices = stbls()$Tab_Stat$ID_Stat |>
                         setNames(stbls()$Tab_Stat$Stationsname),
                       selected = 18)
  })

  ### GET AVAILABLE OPTIONS BASED ON SELECTED STAT
  mw_info_init <- reactive({
    if(login_successful()){
      download_MW_info(Stat = input$Stat,
                       start_date = input$Datum[1],
                       end_date = input$Datum[2],
                       login_credentials = login_credentials$df) |>
        dplyr::left_join(stbls()$Tab_Messposition) |>
        dplyr::left_join(stbls()$Tab_Para) |>
        dplyr::mutate(Messposition = ifelse(is.na(Messposition),
                                            "ohne",
                                            Messposition)
        )
    }
  })

  last_signal <- reactive({
    if(login_successful()){
      download_last_signal(Stat = input$Stat,
                           login_credentials = login_credentials$df,
                           include_old = input$include_old)
    }
  })

  # UPDATE SELECTION
  observe({
  updateSelectInput(session = session,
                    inputId = "Para",
                    choices = mw_info_init()$ID_Para |>
                      setNames(mw_info_init()$Parameter),
                    selected = mw_info_init()$ID_Para[1])
  }) |>
    bindEvent(mw_info_init())

  ### GET AVAILABLE OPTIONS BASED ON SELECTED STAT AND PARA
  mw_info <- reactive({
    if(login_successful()){
      mw_info_init() |>
        dplyr::filter(ID_Para == input$Para)
    }
  })

  # UPDATE SELECTION
  observe({
    updateSelectInput(session = session,
                      inputId = "Spot",
                      choices = mw_info()$ID_Spot,
                      selected = mw_info()$ID_Spot[1])
    updateSelectInput(session = session,
                      inputId = "Messposition",
                      choices = mw_info()$ID_Messposition |>
                        setNames(mw_info()$Messposition),
                      selected = mw_info()$ID_Messposition[1])
  }) |>
    bindEvent(mw_info())


  # ONLY SHOW SELECTION FOR SPOT AND MESSPOSITION IF LENGTH > 1 (see UI conditional panel)
  output$length_messposition <- reactive({
    length(unique(mw_info()$Messposition))
  })

  output$length_spot <- reactive({
    length(unique(mw_info()$ID_Spot))
  })

  output$mw_info_true <- reactive({
    as.numeric(isTruthy(mw_info()) & nrow(mw_info() > 0))
  })

  outputOptions(output, "mw_info_true", suspendWhenHidden = FALSE)
  outputOptions(output, "length_spot", suspendWhenHidden = FALSE)
  outputOptions(output, "length_messposition", suspendWhenHidden = FALSE)


  # DOWNLOAD SELECTION IF BUTTON IS CLICKED
  mw <- reactive({
      download_MW(Stat = input$Stat,
                  Para = input$Para,
                  Spot = input$Spot,
                  Messposition = input$Messposition,
                  start_date = input$Datum[1],
                  end_date = input$Datum[2],
                  Quali = ifelse(input$Quali, 1,-999),
                  login_credentials = login_credentials$df) |>
        dplyr::left_join(stbls()$Tab_Messposition)
  }) |>
    bindEvent(input$download_mw)

  # VISUALIZE SELECTION
  mw_plot <- reactive({
    plot_mw(
      mw = mw(),
      mw_info = mw_info(),
      input = input
      )
  }) |>
    bindEvent(mw())

  health_plot <- reactive({
    plot_health(
      last_signal = last_signal(),
      stbls = stbls()
    )
  }) |>
    bindEvent(mw())

  output$mw_plot <- renderPlot({
    mw_plot()
  })

  output$health_plot <- renderPlot({
    health_plot()
  })

  # check if data has been downloaded already (see conditional panel)
  output$plot_created <- reactive({as.numeric(isTruthy(mw()))})
  outputOptions(output, "plot_created", suspendWhenHidden = FALSE)

  # create detailed filename for download
  dlname <- reactive({
    paste0("Stat", input$Stat, "_",
           stringr::str_trim(mw_info()$Parameter[1]), "_",
           input$Datum[1], "to", input$Datum[2],
           "Spot", paste(input$Spot, collapse = "_"), "_",
           "Messposition", paste(input$Messposition, collapse = "_"), "_",
           ifelse(input$Quali, "DatenqualitaetRohdaten", "DatenqualitaetAlle"))
  })

  #handle download to local download folder
  output$download_local <- downloadHandler(
    filename = function(){
      paste0(dlname(), ".zip")
    },
    content = function(file){
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      #remove previous files
      file.remove(list.files(pattern = "Messposition"))

      #save plot
      ggplot2::ggsave(filename = paste0(dlname(), ".png"),
                      plot = mw_plot(),
                      width = 7,
                      height = 4*length(unique(mw()$Messposition)))

      #write datatable
      readr::write_csv2(mw(), paste0(dlname(), ".csv"))

      files <- list.files(pattern = dlname())

      #create the zip file
      utils::zip(file,
                 files)
    }
  )

}
