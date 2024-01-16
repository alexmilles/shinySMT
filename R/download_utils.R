
connect <- function(login_credentials){
  frwfutils::db_connect(
    server = login_credentials$server,
    database = login_credentials$database,
    user = login_credentials$user,
    pwd = login_credentials$pwd,
    local = login_credentials$local)
}

canConnect <- function(login_credentials){
  frwfutils::dbCanConnect(
    server = login_credentials$server,
    database = login_credentials$database,
    user = login_credentials$user,
    pwd = login_credentials$pwd,
    local = login_credentials$local)
}

full_download <- function(table_name = "Tab_Stat", login_credentials){

  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  download_table <-
    dplyr::tbl(con, table_name) |>
    dplyr::collect()

  return(download_table)
}

download_LI <- function(Stat, login_credentials){
  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  download_table <-
    dplyr::tbl(con, "Tab_LI") |>
    dplyr::filter(ID_Stat == Stat) |>
    dplyr::collect()

  return(download_table)
}

download_MW_info <- function(Stat, Spot = -999, start_date = Sys.time() - 7 * 86400, end_date = Sys.time(), login_credentials){
  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  download_table <-
    dplyr::tbl(con, "Tab_MW") |>
    dplyr::filter(ID_Stat == Stat) |>
    dplyr::filter(Datum > dbplyr::sql_escape_datetime(con, start_date)) |>
    dplyr::filter(Datum < dbplyr::sql_escape_datetime(con, end_date)) |>
    dplyr::select(ID_Stat, ID_Spot, ID_Para, ID_Messposition) |>
    dplyr::distinct() |>
    dplyr::collect()

  return(download_table)
}

download_MW <- function(Stat, Spot = -999, Para = -999, Messposition = -999, start_date = Sys.time() - 7 * 86400, end_date = Sys.time(), Quali = 1, login_credentials){
  con <- connect(login_credentials = login_credentials)

  on.exit(DBI::dbDisconnect(con))

  download_table <-
    dplyr::tbl(con, "Tab_MW") |>
    dplyr::filter(ID_Stat == Stat) |>
    dplyr::filter(Datum > dbplyr::sql_escape_datetime(con, start_date)) |>
    dplyr::filter(Datum < dbplyr::sql_escape_datetime(con, end_date)) |>
    dplyr::filter(ID_Spot %in% Spot | -999 %in% Messposition) |>
    dplyr::filter(ID_Quali %in% Quali) |>
    dplyr::filter(ID_Para == Para| Para == -999) |>
    dplyr::filter(ID_Messposition %in% Messposition |  -999 %in% Messposition) |>
    dplyr::collect()
  return(download_table)
}


download_static_tables <- function(login_credentials){
  tbls <- c("Tab_Stat", "Tab_Logger", "Tab_Messposition", "Tab_Messverfahren", "Tab_Para", "Tab_Quali")

  tbl_list <- vector("list", length(tbls)) |> setNames(tbls)

  for(tbl in tbls){
    tbl_list[[tbl]] <- full_download(tbl, login_credentials = login_credentials)
  }

  return(tbl_list)
}


