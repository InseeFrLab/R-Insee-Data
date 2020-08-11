testthat::context("class and output tests")
library(testthat)
library(insee)

test_that("test that a query returns a dataframe",{
  skip_on_travis()
  skip_on_cran()

  insee_link = "http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM"
  insee_query = file.path(insee_link, paste0("010539365","?", "firstNObservations=1"))

  expect_is(get_date("2010-05", "M"), "Date")
  expect_is(get_insee(insee_query), "data.frame")
  expect_is(get_idbank_list(), "data.frame")
  expect_is(add_insee_title(get_idbank_list()[1,]), "data.frame")
  expect_is(get_insee_idbank("001558315"), "data.frame")
  expect_is(get_insee_title("001558315"), "character")
  expect_is(search_insee("gdp"), "data.frame")
  expect_is(get_insee_dataset("CNA-2014-CPEB",
                              filter = "A.CNA_CPEB.A38-CB.VAL.D39.VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT",
                              lastNObservations = 1), "data.frame")
})

test_that("test that a wrong query returns null",{
  skip_on_cran()
  skip_on_travis()
  # expect_error(get_insee())
  # expect_error(get_insee(""))
  # expect_output(nrow(add_insee_title(get_idbank_list()[1,])), 1)
  # expect_output(get_date(date = "2010-05", freq = "M"), as.Date("2010-05-01"))
  expect_output(get_insee_idbank(), NULL)
  # expect_output(get_insee_dataset(), NULL)
  expect_output(get_insee_idbank(rep("001558315", 1201)), NULL)
})

