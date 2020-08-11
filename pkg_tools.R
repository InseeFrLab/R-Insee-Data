# Good practices
checks = goodpractice::all_checks()
checks_to_omit = c("lintr_assignment_linter")
checks = setdiff(checks, checks_to_omit)
(gp=goodpractice::gp(checks=checks))

# vignette
pkgdown::build_site()

# Run code coverage on a single file and show a nice report
Sys.setenv("NOT_CRAN"="true") # Make not-CRAN tests run

# Run coverage for the entire package, tests only
covr::report(covr::package_coverage())

# Run coverage for the entire package, including examples and vignettes
covr::report(covr::package_coverage(type='all'))


vec = revdep("tidyr", dependencies = "Imports")

vec2 = revdep("covr", dependencies = "Suggests")

pkg_dpd_tidyr = data.frame(pkg = vec[vec %in% vec2])
