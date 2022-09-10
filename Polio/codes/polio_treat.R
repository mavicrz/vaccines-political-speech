# Treat polio data--------------------------------------------------------------
# Author: Maria Vitória Cruz
# Date: 20/07/2022
# Description: This code treats data from polio
# vaccines applied between 2004 to 2021 monthly by 
# municipality in Brazil

# 0. Settings-------------------------------------------------------------------
rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('tidyverse', 'purrr', 'stringr', 'readr','janitor'), install=T)

# 1. Function to read and treat data -------------------------------------------

polio_func <- function(name, path){
  list.files(path = path, pattern = '*.csv', full.names = T) %>% 
    purrr::map(.x = ., .f= ~vroom::vroom(file = .x,delim = ';',
                                         .name_repair = ~str_remove(string = ., pattern = '/'),
                                         locale = locale(encoding = 'latin1'))) %>% 
    purrr::reduce(full_join, by= 'Município') %>%
    dplyr::rename_with(.fn = ~stringr::str_to_lower(.),
                       .cols = everything()) %>%
    dplyr::select(-contains('total')) %>% 
    tidyr::pivot_longer(cols = -c('município'),
                        names_to = 'date',
                        values_to = str_c('vacc_',name)) %>% 
    dplyr::mutate(year = str_extract(date,'[0-9]+'),
                  month = str_extract(date, '[a-zA-Z]+'),
                  cod_mun = str_extract(município, '[0-9]+')) %>% 
    dplyr::select(-c('date','município')) %>%
    dplyr::filter(is.na(cod_mun) == F) %>% 
    write.csv(file = str_c('output/polio_',name,'.csv'),row.names = F)}


# 2. Data to treat in the function ---------------------------------------------
list(name = c('vop_3_dose', 'vip_3_dose', 'hx_3_dose'),
       path = c('input/polio_mun_mensal/vop/3_dose',
                'input/polio_mun_mensal/vip/3_dose',
                'input/polio_mun_mensal/hx/3_dose')) %>% 
  purrr::pmap(polio_func)
