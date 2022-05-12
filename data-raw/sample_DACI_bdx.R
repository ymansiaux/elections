## code to prepare `sample_DACI_bdx` dataset goes here
sample_DACI_bdx <- data.table::fread("/data/data_elections/DACI/DACI_PIVOT_ELECTION_UTF8.csv", encoding = 'UTF-8')

usethis::use_data(sample_DACI_bdx, overwrite = TRUE)


## TEST de la v2

sample_DACI_bdx <- data.table::fread("/data/data_elections/DACI/DACI_PIVOT_ELECTION_v2.csv", encoding = 'UTF-8')

usethis::use_data(sample_DACI_bdx, overwrite = TRUE)
