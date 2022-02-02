usethis::use_build_ignore("dev_history.R")

usethis::use_build_ignore("deploy_to_RSPM.R")

usethis::use_r("_disable_autoload")
usethis::use_build_ignore("R/_disable_autoload.R")

renv::init()

attachment::att_amend_desc()
renv::status()
renv::snapshot()

usethis::use_vignette("designApp")


usethis::use_data_raw("donnees_geo_election_bdx")
usethis::use_data_raw("sample_DACI_bdx")



vignette <- FALSE
devtools::check(document = TRUE, vignettes = vignette)
devtools::build(vignettes = vignette)
devtools::install(build_vignettes = vignette)

usethis::use_test("occupation_compute_xtradata_request_parameters")


pkgload::load_all()

golem::sanity_check()

golem::run_dev()

prefixer::show_nonascii_file()
