## code to prepare `bureaux_de_votes` dataset goes here
library(sf)
library(xtradata)
library(dplyr)

bureaux_votes_bdx <- xtradata_requete_features(key = Sys.getenv("XTRADATA_KEY"), typename = "el_bureauvote_s") %>% st_transform(crs = 4326)
quartiers_bdx <- xtradata_requete_features(key = Sys.getenv("XTRADATA_KEY"), typename = "se_quart_s")  %>% st_transform(crs = 4326)
lieux_votes_bdx <- xtradata_requete_features(key = Sys.getenv("XTRADATA_KEY"), typename = "el_lieuvote_p")  %>% st_transform(crs = 4326)

secteurs_votes_bdx <- bureaux_votes_bdx %>%
  group_by(rs_el_lieuvote_p) %>% 
  summarize(geometry = st_union(geometry))

# bureaux_votes_bdx <- st_read("/data/data_elections/Shape/el_burv_s.shp")
# quartiers_bdx <- st_read("/data/data_elections/Shape/bor_sigquartiers.shp")
# lieux_votes_bdx <- st_read("/data/data_elections/Shape/el_lieuv_p.shp")
# 
# bureaux_votes_bdx <- st_transform(bureaux_votes_bdx, crs = st_crs(quartiers_bdx))

usethis::use_data(bureaux_votes_bdx, overwrite = TRUE)
usethis::use_data(quartiers_bdx, overwrite = TRUE)
usethis::use_data(lieux_votes_bdx, overwrite = TRUE)
usethis::use_data(secteurs_votes_bdx, overwrite = TRUE)

