message("enter login credentials for Testdatenbank_Mike on Klimakompetenzzentrum Server")
user <- readline("user")
pwd <- readline("password")
df = data.frame(
  server = "klikomz",
  database = "Testdatenbank_Mike",
  user = user,
  pwd = pwd,
  local = FALSE
)

test_that("full download works", {
  expect_no_error(full_download("Tab_Stat", login_credentials = df))
  expect_no_error(full_download("Tab_LI", login_credentials = df))
  expect_error(full_download("not_a_table", login_credentials = df))
  expect_s3_class(full_download("Tab_Stat", login_credentials = df), "data.frame")
})

test_that("download LI works", {
  expect_no_error(download_LI(Stat = 1, login_credentials = df))
  expect_no_error(download_LI(Stat = 99, login_credentials = df))
  expect_equal(nrow(download_LI(Stat = 99, login_credentials = df)),0)
  expect_s3_class(download_LI(Stat = 1, login_credentials = df), "data.frame")
})

test_that("download MW works", {
  expect_no_error(download_MW(Stat = 18, Para = 8, login_credentials = df))
  expect_equal(nrow(download_MW(Stat = -1, Para = 8, login_credentials = df)),0)
  expect_s3_class(download_MW(Stat = 18, Para = 8, login_credentials = df), "data.frame")
})


test_that("download MW with multiple Spots and Messpositions works", {
  expect_no_error(download_MW(Stat = 18, Para = 1, Messposition = 1, Spot = c(1,2), login_credentials = df))

  expect_s3_class(download_MW(Stat = 18, Para = 3, Messposition = c(7,13), Spot = c(1,2), login_credentials = df), "data.frame")
  expect_s3_class(download_MW(Stat = 18, Para = 1, Messposition = 1, Spot = c(1,2), login_credentials = df), "data.frame")
})


