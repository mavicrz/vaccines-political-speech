# Analysis polio data-----------------------------------------------------------
# Author: Maria Vitória Cruz
# Date: 20/07/2022
# Description: This code treats data from polio inject 
# and oral vaccines applied between 2004 to 2022 monthly
# by municipality in Brazil

# 0. Settings ------------------------------------------------------------------
rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('tidyverse', 'purrr', 
                   'stringr', 'stargazer',
                   'fixest', 'ggpubr', 
                   'lubridate', 'wesanderson',
                   'MetBrewer','vroom'), install=T)

# Input
files_polio <- list.files(path = 'output/', pattern = '*.csv')

# 1. Prepare data ;) -----------------------------------------------------------
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

pop_0to4 <- vroom::vroom(file='input/pop_residente_0_4/pop_estimativas_municipios.csv',
                         locale = locale(encoding = 'latin1'), delim = ';', skip = 5,
                         n_max = 5570) %>%
  janitor::clean_names() %>%  
  tidyr::pivot_longer(cols = -c('municipio'),
                      names_to = 'year',
                      values_to = 'pop') %>% 
  dplyr::mutate(cod_mun = as.numeric(str_extract(municipio, '[0-9]+')),
                year =as.numeric(str_extract(year, '[0-9]+'))) %>% 
  dplyr::select(year,cod_mun,pop)

votos_bolso <- vroom::vroom(file= 'input/votos_bolsonaro.csv') %>% 
  dplyr::mutate(cod_mun = as.numeric(stringr::str_sub(
    string= id_municipio,start = 1,end = 6))) %>% 
  dplyr::select(-id_municipio)

speech <- readxl::read_xlsx(path = 'input/comunicacao_bolsonaro.xlsx')

panel_polio <- polio_base %>%
  dplyr::full_join(time, by = c('year','month_number')) %>% 
  dplyr::full_join(pop_0to4, by=c('year','cod_mun')) %>% 
  dplyr::mutate(vacc_total = vacc_vip_3_dose+ vacc_vop_3_dose,
                goal_month = pop/12,
                goal_accum = goal_month*month_number) %>% 
  dplyr::group_by(year,cod_mun) %>% 
  dplyr::mutate(vacc_accum = purrr::accumulate(.x=vacc_total,.f=sum),
                cob_accum = vacc_accum/goal_accum,
                cob_month = vacc_total/goal_month,
                time= as.numeric(time)) %>% 
  dplyr::ungroup()%>% 
  dplyr::full_join(votos_bolso, by='cod_mun') %>% 
  dplyr::full_join(speech, by = c('month_number', 'year')) %>% 
  dplyr::mutate(bolso_70 = case_when(
    perc_bolso > 0.7 ~ 1,
    T ~ 0),
    bolso_60 = case_when(
      perc_bolso > 0.6 ~ 1,
      T ~ 0))

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

# 4. Summary is what? FUNDEMENTAL -----------------------------------------------------
panel_polio %>% 
  dplyr::select(vacc_vip_3_dose, vacc_vop_3_dose) %>%
  as.data.frame() %>% 
  stargazer(title = "Análise das variáveis",
            summary.stat = c("mean", "sd", 'median'))

## 4.1 Doses aplicadas -----------------------------------------------------
evolu_doses <- panel_polio %>% 
  dplyr::filter(year %in% 2016:2021) %>% 
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

## 4.2 Cobertura mensal -----------------------------------------------------
evolu_cob_month <-  panel_polio %>% 
  dplyr::filter(year %in% 2016:2021) %>% 
  dplyr::group_by(year,time, month_number) %>%
  dplyr::summarise(across(.cols= c('vacc_total', 'pop'),
                          ~sum(.,na.rm = T)), .groups = 'drop') %>% 
  mutate(
  time = as.numeric(time),
  goal_month = pop/12,
  goal_accum = (pop/12)*month_number
  ) %>%
  arrange(time) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(vacc_accum = purrr::accumulate(.x=vacc_total,.f=sum),
                cob_month = vacc_total/goal_month,
                cob_accum = vacc_accum/goal_accum) %>%
  tidyr::drop_na() %>% 
  mutate(
    date = str_c(month_number, year, sep = '-') %>% lubridate::my()
  ) %>% 
  ggplot2::ggplot(mapping = aes(x= date, y =cob_month*100))+
  geom_line(size= 1, colour = '#5c66a8')  +
  geom_vline(xintercept = as.Date('2019-01-01'),
             linetype = 'dashed', colour = "#cf3a36", size = 1) +
  ylab('Cobertura mensal acumulada') +
  scale_x_date(date_breaks = '6 month', limits = c(as.Date('2016-01-01'),
                                                   as.Date('2021-12-01'))) + 
  ylab('Cobertura Mensal (%)') + xlab('Data') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Cobertura mensal de vacina para Poliomielite (%)') +
  labs(caption = 'Nota: Cobertura mesal é calculada com o número de doses aplicadas dividido pela meta mensal de crianças. \n A meta mensal é número total de crianças de 0 a 4 anos dividido por 12. \n A data em vermelho se refere ao mês de posse da presidência de Jair Bolsonaro.')

ggsave(evolu_cob_month,
       filename = 'output/plot/evolution_cob_month.png',
       width = 30, height = 20, device = 'png', bg = 'white',
       units = 'cm')

## 4.3 Cobertura mensal acumulada -----------------------------------------------------
evolu_cob_accum <-  panel_polio %>% 
  dplyr::filter(year %in% 2016:2021) %>% 
  dplyr::group_by(year,time, month_number) %>%
  dplyr::summarise(across(.cols= c('vacc_total', 'pop'),
                          ~sum(.,na.rm = T)), .groups = 'drop') %>% 
  mutate(
    time = as.numeric(time),
    goal_month = pop/12,
    goal_accum = (pop/12)*month_number
  ) %>%
  arrange(time) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(vacc_accum = purrr::accumulate(.x=vacc_total,.f=sum),
                cob_month = vacc_total/goal_month,
                cob_accum = vacc_accum/goal_accum) %>%
  tidyr::drop_na() %>% 
  mutate(
    date = str_c(month_number, year, sep = '-') %>% lubridate::my()
  ) %>% 
  ggplot2::ggplot(mapping = aes(x= date, y =cob_accum*100))+
  geom_line(size= 1, colour ='#0B775E')  +
  geom_vline(xintercept = as.Date('2019-01-01'),
             linetype = 'dashed', colour = "#cf3a36", size = 1) +
  ylab('Cobertura mensal acumulada (%)' ) +
  scale_x_date(date_breaks = '6 month') + xlab('Data') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Cobertura mensal acumulada de vacina para Poliomielite (%)') +
  labs(caption = 'Nota: Cobertura mesal acumulada é calculada com o número de doses aplicadas até aquele mês dividido pela meta mensal acumulada \n A meta mensal acumulada é número total de crianças de 0 a 4 anos dividido por 12 vezes o número de meses. \n A data em vermelho se refere ao mês de posse da presidência de Jair Bolsonaro.')

ggsave(evolu_cob_accum,
       filename = 'output/plot/evolution_cob_accum.png',
       width = 30, height = 20, device = 'png', bg = 'white',
       units = 'cm')

## 4.4 Cobertura mensal por votos no Bolsonaro -----------------------------------------------------
evol_cob_month_votes <- panel_polio %>% 
  dplyr::filter(year %in% 2016:2021) %>% 
  dplyr::group_by(year,time, month_number,win_bolso) %>%
  dplyr::summarise(across(.cols= c('vacc_total', 'pop'),
                          ~sum(.,na.rm = T)), .groups = 'drop') %>% 
  mutate(
    time = as.numeric(time),
    goal_month = pop/12,
    goal_accum = (pop/12)*month_number
  ) %>%
  arrange(time) %>% 
  dplyr::group_by(year, win_bolso) %>% 
  dplyr::mutate(vacc_accum = purrr::accumulate(.x=vacc_total,.f=sum),
                cob_month = vacc_total/goal_month,
                cob_accum = vacc_accum/goal_accum,
                Votos = case_when(win_bolso == 1 ~ 'Mais que 50% dos votos para Bolsonaro',
                                  win_bolso == 0 ~ 'Menos que 50% dos votos para Bolsonaro')) %>% 
  tidyr::drop_na() %>% 
  mutate(
    date = str_c(month_number, year, sep = '-') %>% lubridate::my()
  ) %>% 
  ggplot2::ggplot(mapping = aes(x= date, y =cob_month*100, color=factor(Votos)))+
  geom_line(size= 1)  +
  geom_vline(xintercept = as.Date('2019-01-01'),
             linetype = 'dashed', colour = '#cf3a36', size = 1) +
  labs(x='Data', y='Cobertura mensal (%)', color = '')+
  scale_x_date(date_breaks = '6 month') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Cobertura mensal para Poliomielite por votos no presidente eleito em 2018 (%)') +
  scale_color_manual(values = c("#F8AFA8", "#E2D200"))

ggsave(evol_cob_month_votes,
       filename = 'output/plot/evol_cob_month_votes.png',
       width = 35, height = 20, device = 'png', bg = 'white',
       units = 'cm')


## 4.5 Cobertura mensal acumulada por votos no Bolsonaro -----------------------------------------------------
evol_cob_accum_votes <- panel_polio %>% 
  dplyr::filter(year %in% 2016:2021) %>% 
  dplyr::group_by(year,time, month_number,win_bolso) %>%
  dplyr::summarise(across(.cols= c('vacc_total', 'pop'),
                          ~sum(.,na.rm = T)), .groups = 'drop') %>% 
  mutate(
    time = as.numeric(time),
    goal_month = pop/12,
    goal_accum = (pop/12)*month_number
  ) %>%
  arrange(time) %>% 
  dplyr::group_by(year, win_bolso) %>% 
  dplyr::mutate(vacc_accum = purrr::accumulate(.x=vacc_total,.f=sum),
                cob_month = vacc_total/goal_month,
                cob_accum = vacc_accum/goal_accum,
                Votos = case_when(win_bolso == 1 ~ 'Mais que 50% dos votos para Bolsonaro',
                                  win_bolso == 0 ~ 'Menos que 50% dos votos para Bolsonaro')) %>% 
  tidyr::drop_na() %>% 
  mutate(
    date = str_c(month_number, year, sep = '-') %>% lubridate::my()
  ) %>% 
  ggplot2::ggplot(mapping = aes(x= date, y =cob_accum*100, color=factor(Votos)))+
  geom_line(size= 1)  +
  geom_vline(xintercept = as.Date('2019-01-01'),
             linetype = 'dashed', colour = '#cf3a36', size = 1) +
  labs(x='Data', y='Cobertura mensal acumulada (%)', color = '')+
  scale_x_date(date_breaks = '6 month') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Cobertura mensal acumulada para Poliomielite por votos no presidente eleito em 2018 (%)') +
  scale_color_manual(values = c("#F8AFA8", "#E2D200"))

ggsave(evol_cob_accum_votes,
       filename = 'output/plot/evol_cob_accum_votes.png',
       width = 35, height = 20, device = 'png', bg = 'white',
       units = 'cm')


## 4.6 Cobertura mensal acumulada por votos no Bolsonaro + Falas -----------------------------------------------------
evolu_cob_month <-  panel_polio %>% 
  dplyr::filter(year %in% 2016:2021) %>% 
  dplyr::group_by(year,time, month_number) %>%
  dplyr::summarise(across(.cols= c('vacc_total', 'pop'),
                          ~sum(.,na.rm = T)), .groups = 'drop') %>% 
  mutate(
    time = as.numeric(time),
    goal_month = pop/12,
    goal_accum = (pop/12)*month_number
  ) %>%
  arrange(time) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(vacc_accum = purrr::accumulate(.x=vacc_total,.f=sum),
                cob_month = vacc_total/goal_month,
                cob_accum = vacc_accum/goal_accum) %>%
  tidyr::drop_na() %>% 
  mutate(
    date = str_c(month_number, year, sep = '-') %>% lubridate::my()
  ) %>% 
  ggplot2::ggplot(mapping = aes(x= date, y =cob_month*100))+
  geom_line(size= 1, colour = '#5c66a8')  +
  geom_vline(xintercept = c(as.Date('2020-12-17'),as.Date('2021-01-22'),
                            as.Date('2021-02-11'), as.Date('2021-03-04'),
                            as.Date('2021-06-09'), as.Date('2021-06-17'),
                            as.Date('2021-09-02'), as.Date('2021-12-02'),
                            as.Date('2021-12-07'), as.Date('2021-12-07'),
                            as.Date('2021-12-24')),
             linetype = 'dashed', colour = "#cf3a36", size = 1) +
  ylab('Cobertura mensal acumulada') +
  scale_x_date(date_breaks = '3 month', limits = c(as.Date('2020-01-01'),
                                                   as.Date('2021-12-01'))) + 
  ylab('Cobertura Mensal (%)') + xlab('Data') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Cobertura mensal de vacina para Poliomielite (%)') 

ggsave(evolu_cob_month,
       filename = 'output/plot/evolution_cob_month.png',
       width = 30, height = 20, device = 'png', bg = 'white',
       units = 'cm')
