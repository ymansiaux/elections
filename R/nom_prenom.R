# get_last_name <- function(full_name) {
#   str_extract(pattern = "^[:alpha:]{1,}", string = full_name)
# }
# 
# get_first_name <- function(full_name, last_name) {
#   gsub(pattern = last_name, replacement = "", x = full_name) %>% gsub(pattern = " ", replacement = "", x = .)
# }

get_first_name <- function(full_name) {
  str_extract(string = full_name, pattern = "[:alpha:]{1,}[:punct:]{0,}[:alpha:]{0,}$")
}

get_last_name <- function(full_name, first_name) {
  str_trim(str_replace(string = full_name, pattern = first_name, replacement = ""))
}