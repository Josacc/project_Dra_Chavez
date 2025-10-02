# Project Dra. Montserrat Ch.
# By Josacc

analysis_files <- list.files("main/", pattern = "^\\d.+\\.R$", full.names = TRUE)
invisible(purrr::map(analysis_files, source))
