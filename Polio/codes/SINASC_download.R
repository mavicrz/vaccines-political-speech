# Download SINASC-------------------------------------
# Author: Maria Vitória Cruz
# Date: 16/08/2022
# Description: This code download data from SINASC
# from 2006 to 2020 for the states of Brazil
# SINASC is the data for born babies in Brazil :)

# 0. Settings ---------------------------------------
rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('tidyverse', 'read.dbc',
                   'foreign','tibble','stringr'), install=T)

# 1. Function -----------------------------------------------------
sinasc_download <- function(year, state){
  prefix <- 'DN'
  glue_url <- stringr::str_c('ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/',
                             prefix,state,year,'.dbc')
  glue_file <- stringr::str_c('input/sinasc/',prefix,state,year)
  download.file(url = glue_url, destfile = str_c(glue_file, '.dbc'), mode = 'wb')
  read.dbc::read.dbc(str_c(glue_file, '.dbc')) %>% 
    write.csv(., file = str_c(glue_file, '.csv'))
}
# 2. Data -----------------------------------------------------
sinasc <- tibble::tibble(year = c(rep(2006:2020, each = 1, times =27)),
                 state = c(rep(c("RO", "AC", "AM", "RR", "PA",
                                 "AP", "TO", "MA", "PI", "CE",
                                 "RN", "PB", "PE", "AL", "SE",
                                 "BA", "MG", "ES", "RJ", "SP",
                                 "PR", "SC", "RS", "MS", "MT",
                                 "GO", "DF"), each = 15, times = 1)))

# 3. Run the function ------------------------------------------
sinasc %>% 
  purrr::pmap(sinasc_download)
