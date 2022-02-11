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

usethis::use_data(bureaux_votes_bdx, overwrite = TRUE)
usethis::use_data(quartiers_bdx, overwrite = TRUE)
usethis::use_data(lieux_votes_bdx, overwrite = TRUE)
usethis::use_data(secteurs_votes_bdx, overwrite = TRUE)

