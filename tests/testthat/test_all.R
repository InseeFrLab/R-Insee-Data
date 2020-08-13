testthat::context("class and output tests")
library(testthat)
library(insee)

test_that("class tests",{
  # skip_on_cran()

  expect_warning(insee:::.onLoad(), regexp = NA)

  expect_is(get_idbank_list(), "data.frame")
  expect_is(get_idbank_list(), "data.frame")

  idbank_test1 = get_idbank_list()[1,"idbank"]
  idbank_test2 = get_idbank_list()[2,"idbank"]

  expect_is(get_idbank_list("CNA-2014-CPEB"), "data.frame")
  expect_is(get_dataset_list(), "data.frame")

  insee_link = "http://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM"
  insee_query = file.path(insee_link, paste0(idbank_test1,"?", "firstNObservations=1"))

  expect_is(get_insee(insee_query), "data.frame")
  expect_is(get_insee(), "NULL")
  expect_is(get_insee(""), "NULL")

  expect_is(get_insee_idbank(idbank_test1), "data.frame")
  expect_is(get_insee_idbank(), "NULL")

  expect_is(get_insee_dataset(), "NULL")
  expect_is(get_insee_dataset("CNA-2014-CPEB",
                              filter = "A.CNA_CPEB.A38-CB.VAL.D39.VALEUR_ABSOLUE.FE.EUROS_COURANTS.BRUT",
                              lastNObservations = 1), "data.frame")
  expect_error(get_insee_dataset(1))
  expect_error(get_insee_dataset(c("a", "b")))

  expect_is(get_insee_title(idbank_test1), "character")
  expect_is(get_insee_title(list(idbank_test1, idbank_test2)), "character")
  expect_is(get_insee_title(), "NULL")

  expect_is(add_insee_title(get_idbank_list()[1,]), "data.frame")
  expect_is(add_insee_title(get_idbank_list()[1,], lang = ""), "data.frame")

  expect_is(get_date("2010-05", "M"), "Date")

  expect_is(search_insee("gdp"), "data.frame")
})

test_that("output tests",{
  # skip_on_cran()

  idbank_test1 = get_idbank_list()[1,"idbank"]
  idbank_test401 = unique(get_idbank_list()[1:401,"idbank"])
  idbank_test1201 = unique(get_idbank_list()[1:1201,"idbank"])

  expect_equal(get_date(1, ""), 1)
  expect_equal(get_date("2010-05", "M"), as.Date("2010-05-01"))
  expect_equal(get_date("2010", "A"), as.Date("2010-01-01"))
  expect_equal(get_date("2010-Q1", "T"), as.Date("2010-01-01"))
  expect_equal(get_date("2010-S1", "S"), as.Date("2010-01-01"))
  expect_equal(get_date("2010-B1", "B"), as.Date("2010-01-01"))


  expect_equal(nrow(split_title(get_insee_idbank(idbank_test1, firstNObservations = 1))), 1)
  expect_equal(nrow(split_title(get_insee_idbank(idbank_test1, firstNObservations = 1), lang = "fr")), 1)
  expect_equal(nrow(get_insee_idbank(idbank_test401, firstNObservations = 1)), length(idbank_test401))

  expect_output(get_insee_idbank(), NULL)
  expect_output(get_insee_idbank(idbank_test1201, firstNObservations = 1), NULL)
  expect_equal(nrow(get_insee_idbank(idbank_test1201, firstNObservations = 1, limit = FALSE)), length(idbank_test1201))

})

