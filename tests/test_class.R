testthat::context("class test")
library(testthat)
library(insee)

test_that("test that a query returns a dataframe",{
  insee_link = "http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM"
  insee_query = file.path(insee_link, paste0("010539365","?", "firstNObservations=1"))

  expect_is(get_insee(insee_query), "data.frame")
  expect_is(get_idbank_list(), "data.frame")
  expect_is(get_dataset_list(), "data.frame")
  expect_is(get_insee_idbank("001558315"), "data.frame")
  expect_is(get_insee_dataset("CNA-2014-CPEB",
                              filter = "A.CNA_CPEB.A38-CB.VAL.D39.VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT",
                              lastNObservations = 1), "data.frame")
})

test_that("test that a wrong query returns null",{
  # expect_error(get_insee())
  # expect_error(get_insee(""))
  expect_output(get_insee_idbank(), NULL)
  # expect_output(get_insee_dataset(), NULL)
  expect_output(get_insee_idbank(rep("001558315", 1201)), NULL)
})

