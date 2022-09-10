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
xfun::pkg_attach(c('tidyverse', 'purrr', 
                   'stringr', 'stargazer',
                   'fixest', 'ggpubr', 
                   'lubridate', 'wesanderson',
                   'MetBrewer'), install=T)

# Input
files_polio <- list.files(path = 'output/', pattern = '*.csv')

# 1. Prepare data ;) -----------------------------------------------------
polio_base <- files_polio %>% 
  purrr::map(., ~read.csv(file = stringr::str_c('output/',.),
                          sep = ',')) %>% 
  purrr::reduce(full_join) %>% 
  dplyr::select(cod_mun, year, month,
                vacc_hx_3_dose, vacc_vip_3_dose, vacc_vop_3_dose) %>% 
  dplyr::filter(year > 2015) %>% 
  dplyr::mutate(month_number = case_when(month == 'jan' ~ 1,
                                         month == 'fev' ~ 2,
                                         month == 'mar' ~ 3,
                                         month == 'abr' ~ 4,
                                         month == 'mai' ~ 5,
                                         month == 'jun' ~ 6,
                                         month == 'jul' ~ 7,
                                         month == 'ago' ~ 8,
                                         month == 'set' ~ 9,
                                         month == 'out' ~ 10,
                                         month == 'nov' ~ 11,
                                         month == 'dez' ~ 12))
  

time <- polio_base%>%
  dplyr::select(year, month_number) %>% 
  dplyr::distinct(year, month_number) %>% 
  dplyr::arrange(year, month_number) %>% 
  dplyr::mutate(time = row.names(.))

doses_cv <- readr::read_csv2(file = 
                               'input/doses_aplicadas_cv/doses_cv_2006_2021_brasil.csv') %>%
  purrr::set_names(c('year', 'vacc_cob_polio', 'total')) %>% 
  dplyr::select(-total) %>% 
  dplyr::filter(year > 2015 & year != 'Total') %>% 
  dplyr::mutate(across(.cols = everything(), ~as.numeric(.)))

doses_cv_mun <- vroom::vroom(file = 'input/doses_aplicadas_cv/doses_cv_2006_2021.csv',delim = ';',
                             .name_repair = ~str_remove(string = ., pattern = '/'),
                             locale = locale(encoding = 'latin1')) %>% 
  dplyr::rename_with(.fn = ~stringr::str_to_lower(.),
                     .cols = everything()) %>%
  dplyr::select(-contains('total')) %>% 
  tidyr::pivot_longer(cols = -c('município'),
                      names_to = 'date',
                      values_to = str_c('vacc_cob_polio')) %>% 
  dplyr::mutate(year = str_extract(date,'[0-9]+'),
                cod_mun = str_extract(município, '[0-9]+')) %>% 
  dplyr::select(-c('date','município')) %>%
  dplyr::mutate(across(.cols = everything(), ~as.numeric(.)))

polio_cob_year_br <- polio_base %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(across(.cols= starts_with('vacc'),
                          ~sum(.,na.rm = T)), .groups = 'drop')  %>% 
  dplyr::full_join(doses_cv, by = 'year') %>% 
  dplyr::mutate(vacc_total = vacc_vip_3_dose+ vacc_vop_3_dose,
                diff_vacc = vacc_total - vacc_cob_polio,
                perc_diff = (diff_vacc/vacc_cob_polio)*100)

polio_cob_year_mun <- polio_base %>%
  dplyr::group_by(year,cod_mun) %>%
  dplyr::summarise(across(.cols= starts_with('vacc'),
                          ~sum(.,na.rm = T)), .groups = 'drop')  %>% 
  dplyr::full_join(doses_cv_mun, by = c('year','cod_mun')) %>% 
  dplyr::mutate(vacc_total = vacc_vip_3_dose+ vacc_vop_3_dose,
                diff_vacc = vacc_total - vacc_cob_polio,
                perc_diff = (diff_vacc/vacc_cob_polio)*100,
                vacc_total_k = vacc_total/1000,
                vacc_cob_polio_k = vacc_cob_polio/1000)

panel_polio <- polio_base %>%
  dplyr::full_join(time, by = c('year','month_number')) %>% 
  dplyr::mutate(vacc_total = vacc_vip_3_dose+ vacc_vop_3_dose)

# 2. Is this a good data? yep!! :) -----------------------------------------------------
corr_doses <- polio_cob_year_mun %>%
  ggpubr::ggscatter(data = ., x = "vacc_total_k", y = "vacc_cob_polio_k", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Doses aplicadas (mensais)", 
            ylab = "Doses de cobertura (anuais)", 
            add.params = list(color = "#5c66a8", fill = "lightgray", size = 1),
            title = 'Correlação entre doses aplicadas e doses de cobertura vacinal (em milhares)') +
  theme_minimal() + ylim(0,150) + 
  theme(title = element_text(size = 8)) + labs(caption = 'Nota: "Doses aplicadas" refere-se aos dados de doses aplicadas da vacina para poliomielite (oral e injetável) que são disponibilizadas mensalmente no TabNet.\n "Doses de cobertura" refere-se ao número de doses que foram utilizadas para o cálculo de cobertura vacinal da poliomielite e que são disponibilizadas anualmente no TabNet.')
  
ggsave(corr_doses,
       filename = 'output/plot/correlation_doses.png',
       width = 20, height = 12, device = 'png', bg = 'white',
      units = 'cm')

# 3. Summary is what? FUNDEMENTAL -----------------------------------------------------
panel_polio %>% 
  dplyr::select(vacc_vip_3_dose, vacc_vop_3_dose) %>%
  as.data.frame() %>% 
  stargazer(title = "Análise das variáveis",
            summary.stat = c("mean", "sd", 'median'))

evolu_doses <- panel_polio %>% 
  dplyr::group_by(year,month_number) %>%
  dplyr::summarise(across(.cols= starts_with('vacc'),
                          ~sum(.,na.rm = T)), .groups = 'drop') %>%
  dplyr::mutate(Ano = factor(year),
                vacc_total_k = vacc_total/1000) %>% 
  ggplot2::ggplot(mapping = aes(x= month_number, y =vacc_total_k,
                                color = Ano))+
  geom_line(size= 1) + scale_x_continuous(breaks = 1:12, name = 'Mês') +
  ylab('Doses aplicadas') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Doses aplicadas de vacina para Poliomielite (em milhares)') +
  scale_color_manual(values = c("#cf3a36", "#FAD510","#5c66a8",
                                "#F8AFA8","#0B775E", "#E2D200")) + 
  labs(caption = 'Nota: Evolução de doses aplicadas com dados do TabNet para vacina de Poliomielite (oral e injetável)')

ggsave(evolu_doses,
       filename = 'output/plot/evolution_doses.png',
       width = 20, height = 12, device = 'png', bg = 'white',
       units = 'cm')
