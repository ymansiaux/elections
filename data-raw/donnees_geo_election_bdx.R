## code to prepare `bureaux_de_votes` dataset goes here
library(sf)

bureaux_votes_bdx <- st_read("/data/data_elections/Shape/el_burv_s.shp")
quartiers_bdx <- st_read("/data/data_elections/Shape/bor_sigquartiers.shp")
lieux_votes_bdx <- st_read("/data/data_elections/Shape/el_lieuv_p.shp")

bureaux_votes_bdx <- st_transform(bureaux_votes_bdx, crs = st_crs(quartiers_bdx))

usethis::use_data(bureaux_votes_bdx, overwrite = TRUE)
usethis::use_data(quartiers_bdx, overwrite = TRUE)
usethis::use_data(lieux_votes_bdx, overwrite = TRUE)

