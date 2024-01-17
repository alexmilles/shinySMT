
#' @title Connects to database via data.frame
#' Wrapper for frwfutils::db_connect
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
#' Wrapper for frwfutils::dbCanConnect
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

#' @param Stat character, name of the stat
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

#' @param Stat character, name of the stat
#' @param Spot character, name of the measurement locaiton
#' @param start_date posixct, lower range of the time frame
#' @param end_date posixct, upper range of the time frame
#' @param login_credentials data.frame, contains login information
#' #'
#' @export
download_MW_info <- function(Stat, Spot = -999, start_date = Sys.time() - 7 * 86400, end_date = Sys.time(), login_credentials){
  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  download_table <-
    dplyr::tbl(con, "Tab_MW") |>
    dplyr::filter(ID_Stat == Stat) |>
    dplyr::filter(Datum > dbplyr::sql_escape_datetime(con, start_date)) |>
    dplyr::filter(Datum < dbplyr::sql_escape_datetime(con, end_date)) |>
    dplyr::select(ID_Stat, ID_Spot, ID_Para, ID_Messposition, ID_Wdh) |>
    dplyr::distinct() |>
    dplyr::collect()

  return(download_table)
}

#' @title download information on which Spots, Paramaters, Messpositions exist for a given Stat and time frame
#' connects to database  to get information and disconnects after finishing or error

#' @param Stat character, name of the stat
#' @param Spot character, name of the measurement locaiton
#' @param Para character, name of the measurement parameter
#' @param Messposition numeric, vertical location of measurement, can be above or below ground
#' @param start_date posixct, lower range of the time frame
#' @param end_date posixct, upper range of the time frame
#' @param Quali, integer, value of dataqualitaet (1 = raw data)
#' @param login_credentials data.frame, contains login information
#'
#' @export
download_MW <- function(Stat, Spot = -999, Para = -999, Messposition = -999, start_date = Sys.time() - 7 * 86400, end_date = Sys.time(), Quali = -999, login_credentials){
  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  download_table <-
    dplyr::tbl(con, "Tab_MW") |>
    dplyr::filter(ID_Stat == Stat) |>
    dplyr::filter(Datum > dbplyr::sql_escape_datetime(con, start_date)) |>
    dplyr::filter(Datum < dbplyr::sql_escape_datetime(con, end_date)) |>
    dplyr::filter(ID_Spot %in% Spot | -999 %in% Messposition) |>
    dplyr::filter(ID_Quali %in% Quali | -999 %in% Quali) |>
    dplyr::filter(ID_Para == Para| Para == -999) |>
    dplyr::filter(ID_Messposition %in% Messposition | -999 %in% Messposition) |>
    dplyr::collect()
  return(download_table)
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


