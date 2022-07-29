# Analysis polio data-----------------------------------
# Author: Maria Vitória Cruz
# Date: 20/07/2022
# Description: This code treats data from polio inject 
# and oral vaccines applied between 2004 to 2022 monthly
# by municipality in Brazil

# 0. Settings ---------------------------------------
rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('tidyverse', 'purrr', 'stringr', 'stargazer'), install=T)

# Input
files_polio <- list.files(path = 'output/', pattern = '*.csv')
file_pop_4 <- 'input/outros/pop_0_4.csv'
file_cob <- 'input/outros/cobertura_anual.csv'

# 1. Read data -----------------------------------------------------
polio_base <- files_polio %>% 
  purrr::map(., ~read.csv(file = stringr::str_c('output/',.),
                          sep = ',')) %>% 
  purrr::reduce(left_join) %>% 
  dplyr::select(cod_mun, year, month, vacc_vip_1_dose, vacc_vip_2_dose,
                vacc_vop_1_dose, vacc_vop_2_dose, vacc_vop_3_dose)

pop <- read.csv(file_pop_4) %>% 
  dplyr::select(ano, codmun, pop0a4) %>% 
  purrr::set_names(., c('year', 'cod_mun', 'pop0to4'))

polio_cob_year <- polio_base %>%
  dplyr::group_by(year, cod_mun) %>% 
  dplyr::summarise(across(.cols= starts_with('vacc'), ~sum(.,na.rm = T)), .groups = 'drop') %>% 
  dplyr::mutate(vacc_total_vip = vacc_vip_1_dose + vacc_vip_2_dose,
                vacc_total_vop = vacc_vop_1_dose + vacc_vop_2_dose + vacc_vop_3_dose,
                vacc_total =vacc_total_vop + vacc_total_vip) %>% 
  dplyr::filter(year > 2009)



# 3. Summary -----------------------------------------------------

polio_base %>% 
  dplyr::select(vacc_vip, vacc_vop) %>%
  as.data.frame() %>% 
  stargazer(type = "text", title = "Análise das variáveis",
            summary.stat = c("mean", "sd", 'median'))

