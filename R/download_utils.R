
#' @title Connects to database via data.frame
#' Wrapper for db_connect
#'
#' @param login_credentials data.frame, contains login information
connect <- function(login_credentials){
  frwfutils::db_connect(
    server = login_credentials$server,
    database = login_credentials$database,
    user = login_credentials$user,
    pwd = login_credentials$pwd,
    local = login_credentials$local)
}

#' @title Checks whether login information is valid
#' Wrapper for dbCanConnect
#'
#' @param login_credentials data.frame, contains login information
canConnect <- function(login_credentials){
  frwfutils::dbCanConnect(
    server = login_credentials$server,
    database = login_credentials$database,
    user = login_credentials$user,
    pwd = login_credentials$pwd,
    local = login_credentials$local)
}

#' @title download datatable
#' connects to database to download all content of a table and disconnects after finishing or error

#' @param table_name character, name of the database table
#' @param login_credentials data.frame, contains login information

full_download <- function(table_name = "Tab_Stat", login_credentials){

  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  download_table <-
    dplyr::tbl(con, table_name) |>
    dplyr::collect()

  return(download_table)
}


#' @title download LI datatable with filter
#' connects to database to download LI table with filter and disconnects after finishing or error

#' @param Stat integer, id of the stat
#' @param login_credentials data.frame, contains login information
download_LI <- function(Stat, login_credentials){
  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  download_table <-
    dplyr::tbl(con, "Tab_LI") |>
    dplyr::filter(ID_Stat == Stat) |>
    dplyr::collect()

  return(download_table)
}

#' @title download information on which Spots, Paramaters, Messpositions exist for a given Stat and time frame
#' connects to database to get information and disconnects after finishing or error

#' @param Stat integer, id of the stat
#' @param Spot integer, if of the measurement location
#' @param start_date posixct, lower range of the time frame
#' @param end_date posixct, upper range of the time frame
#' @param login_credentials data.frame, contains login information
#' #'
#' @export
download_MW_info <- function(Stat, Spot = -999, start_date = Sys.time() - 7 * 86400, end_date = Sys.time(), login_credentials){
  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  mw_info <-
    dplyr::tbl(con, "Tab_MW") |>
    dplyr::filter(ID_Stat == Stat) |>
    dplyr::filter(Datum > start_date) |>
    dplyr::filter(Datum < end_date) |>
    dplyr::select(ID_Stat, ID_Spot, ID_Para, ID_Messposition, ID_Wdh) |>
    dplyr::distinct() |>
    dplyr::collect()

  return(mw_info)
}

#' @title download information on which Spots, Paramaters, Messpositions exist for a given Stat and time frame
#' connects to database  to get information and disconnects after finishing or error

#' @param Stat integer, id of the stat
#' @param Spot integer, if of the measurement location
#' @param Para integer, id of the parameter
#' @param Messposition integer, id of vertical location of measurement, can be above or below ground
#' @param start_date posixct, lower range of the time frame
#' @param end_date posixct, upper range of the time frame
#' @param Quali, integer, id value of dataqualitaet (1 = raw data)
#' @param login_credentials data.frame, contains login information
#' @param agg_fun character, function used for aggregation, "none" if no aggregation is to be applied
#' @param agg_time numeric, temporal resolution of aggregation
#'
#' @export
download_MW <- function(Stat,
                        Spot = -999,
                        Para = -999,
                        Messposition = -999,
                        start_date = Sys.time() - 7 * 86400,
                        end_date = Sys.time(),
                        Quali = -999,
                        login_credentials,
                        agg_fun = "none",
                        agg_time = NULL){
  #login_credentials <- data.frame(server = "klikomz", database = "Testdatenbank_Mike", user = "fawfuser", pwd = "odbc", local = FALSE)
  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  mw <-
    dplyr::tbl(con, "Tab_MW") |>
    dplyr::filter(Datum > start_date) |>
    dplyr::filter(Datum <  end_date) |>
    dplyr::filter(ID_Stat == Stat) |>
    dplyr::filter(ID_Spot %in% Spot | -999 %in% Messposition) |>
    dplyr::filter(ID_Quali %in% Quali | -999 %in% Quali) |>
    dplyr::filter(ID_Para == Para| Para == -999) |>
    dplyr::filter(ID_Messposition %in% Messposition | -999 %in% Messposition) |>
    apply_agg_fun(agg_fun = agg_fun, agg_time = agg_time) |>
    dplyr::collect() |>
    dplyr::mutate(Datum = ifelse(rep(is.numeric(Datum), dplyr::n()),
                                 as.POSIXct(Datum * agg_time * 86400, origin = "1900-01-01"),
                                 as.POSIXct(Datum))) |>
    dplyr::mutate(Datum = as.POSIXct(Datum,
                                     origin = "1970-01-01"))  |>
    dplyr::relocate(Messwert, .before = ID_Quali) |>
    dplyr::ungroup()

  return(mw)
}

#' Function aggregate mw data with a given temporal resolution and function
#'
#' @param mw dbplyr database connection tbl
#' @param agg_fun character, name of the aggregation function - may either be "mean", "min", "max" or "none"
#' @param agg_time numeric, temporal resolution of the aggregation
apply_agg_fun <- function(mw, agg_fun, agg_time){
  {if(agg_fun != "none"){
    mw |>
      dplyr::select(dplyr::everything()) |>
    dplyr::group_by(ID_Stat,
                    ID_Spot,
                    ID_Para,
                    ID_Messposition,
                    ID_Messverfahren,
                    ID_Wdh,
                    Baumnr,
                    ID_Orientierung,
                    ID_BA,
                    ID_Alter,
                    Datum = floor(as.numeric(Datum) / agg_time),
                    ID_Quali) |>
      dplyr::summarise(Messwert = switch(agg_fun,
                                         mean = mean(Messwert),
                                         min = min(Messwert),
                                         max = max(Messwert)
      )
      )
  }else{
    mw
  }}
}

#' @title download tables that contain static information
#' connects to databaseto get informationr and disconnects after finishing or error

#' @param login_credentials data.frame, contains login information
#'
#' @export
download_static_tables <- function(login_credentials){
  tbls <- c("Tab_Stat", "Tab_Logger", "Tab_Messposition", "Tab_Messverfahren", "Tab_Para", "Tab_Quali")

  tbl_list <- vector("list", length(tbls)) |> setNames(tbls)

  for(tbl in tbls){
    tbl_list[[tbl]] <- full_download(tbl, login_credentials = login_credentials)
  }

  return(tbl_list)
}

download_last_signal <- function(Stat,
                                 include_old = FALSE,
                                 login_credentials,
                                 start_date =Sys.time() - 14 * 86500){
  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

    last_signal <-
    dplyr::tbl(con, "Tab_MW") |>
    dplyr::filter(Datum > start_date) |>
    dplyr::filter(ID_Stat == Stat) |>
    dplyr::group_by(ID_Para, ID_Spot, ID_Messposition, ID_Wdh) |>
    dplyr::filter(Datum == max(Datum)) |>
    dplyr::collect() |>
    dplyr::filter(seq(dplyr::n()) == 1)

  if(include_old){
    all_measurements <- dplyr::tbl(con, "Tab_MW") |>
      dplyr::filter(ID_Stat == Stat) |>
      dplyr::select(ID_Para, ID_Spot, ID_Messposition, ID_Wdh) |>
      dplyr::distinct() |>
      dplyr::collect()

    last_signal <-
      all_measurements |>
      dplyr::left_join(last_signal) |>
      dplyr::mutate(Datum = ifelse(is.na(Datum), (start_date - 86400), Datum)) |>
      dplyr::mutate(Datum = as.POSIXct(Datum))
  }

  return(last_signal)
}

# coordinate data currently insufficient
# download_coords <- function(){
#   # login_credentials = data.frame(server = "klikomz", database = "Testdatenbank_Mike", user = "fawfuser", pwd = "odbc", local = FALSE)
#   # con <- connect(login_credentials = login_credentials)
#   # on.exit(DBI::dbDisconnect(con))
#   #
#   # odbc::dbListTables(con) |>
#   #   as.data.frame() |>
#   #   setNames("names") |>
#   #   dplyr::filter(stringr::str_detect(names, "Tab"))
#   #
#   # full_download("Tab_SL", login_credentials)
#
# }

