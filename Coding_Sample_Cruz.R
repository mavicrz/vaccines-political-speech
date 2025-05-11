# Introduction -----------------------------------------------------------------
# This code contais three separete scripts from the project
# "https://github.com/mavicrz/vaccines-political-speech"

# The exercise aimed to understand the impact of political anti-vaccine speech 
# on parents' decisions to vaccinate their children against polio, as children’s 
# vaccination rates in Brazil dropped after the pandemic. 
# The idea was to use the percentage of votes to this president as a mechanism 
# for favouritism and likelihood of accepting the anti-vaccine speech.

# The hypothesis is that polio vaccination rates in municipalities where 
# more than half of the population voted for then-president Bolsonaro 
# would decrease after his speech against COVID-19 vaccines. 
# I aimed to test this using a difference-in-differences framework 
# to assess the impact of his speech on monthly vaccination rates.

# Monthly vaccination rates at the municipal level were not provided 
# by the Ministry of Health—only the number of vaccines administered 
# each month. Therefore, I constructed a proxy for the vaccination rate 
# using the number of polio vaccines administered in a given month 
# and the estimated number of children eligible for vaccination that month. 
# To estimate age eligibility, I used the number of births and tracked their 
# age progression over time.

# This was a personal project I worked on in 2022, during my third year of undergrad. 
# It was not part of my thesis, coursework, or research assistantships. 
# Since I didn’t find a strong correlation between the president’s speech 
# and changes in vaccination rates, I eventually stopped working on it. 
# However, I believe the project still demonstrates my skills in coding 
# and causal inference.

# Download births data (SINASC) from DataSUS -----------------------------------
# Author: Maria Vitória Cruz
# Date: 16/08/2022
# Description: This code downloads data from SINASC
# from 2006 to 2020 for the states of Brazil

## 0. Settings ------------------------------------------------------------------
rm(list=ls())
gc()

## Libraries
xfun::pkg_attach(c('tidyverse', 'read.dbc',
                   'foreign','tibble','stringr'), install=T)

## 1. Function ------------------------------------------------------------------
sinasc_download <- function(year, state){
  prefix <- 'DN'
  glue_url <- stringr::str_c('ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/',
                             prefix,state,year,'.dbc')
  glue_file <- stringr::str_c('input/sinasc/',prefix,state,year)
  download.file(url = glue_url, destfile = str_c(glue_file, '.dbc'), mode = 'wb')
  read.dbc::read.dbc(str_c(glue_file, '.dbc')) %>% 
    write.csv(., file = str_c(glue_file, '.csv'))
}
## 2. Data ----------------------------------------------------------------------
sinasc <- tibble::tibble(year = c(rep(2006:2020, each = 1, times =27)),
                         state = c(rep(c("RO", "AC", "AM", "RR", "PA",
                                         "AP", "TO", "MA", "PI", "CE",
                                         "RN", "PB", "PE", "AL", "SE",
                                         "BA", "MG", "ES", "RJ", "SP",
                                         "PR", "SC", "RS", "MS", "MT",
                                         "GO", "DF"), each = 15, times = 1)))

## 3. Run the function ----------------------------------------------------------
sinasc %>% 
  purrr::pmap(sinasc_download)



# Treat Polio data--------------------------------------------------------------
# Author: Maria Vitória Cruz
# Date: 20/07/2022
# Description: This code treats data from polio
# vaccines applied between 2004 to 2021 monthly by 
# municipality in Brazil

## 0. Settings-------------------------------------------------------------------
rm(list=ls())
gc()

# Libraries
xfun::pkg_attach(c('tidyverse', 'purrr', 'stringr', 'readr','janitor'), install=T)

## 1. Function to read and treat data -------------------------------------------

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


## 2. Data to treat in the function ---------------------------------------------
list(name = c('vop_3_dose', 'vip_3_dose', 'hx_3_dose'),
     path = c('input/polio_mun_mensal/vop/3_dose',
              'input/polio_mun_mensal/vip/3_dose',
              'input/polio_mun_mensal/hx/3_dose')) %>% 
  purrr::pmap(polio_func)

# Analysis polio data-----------------------------------------------------------
# Author: Maria Vitória Cruz
# Date: 20/07/2022
# Description: This code treats data from polio inject 
# and oral vaccines applied between 2004 to 2022 monthly
# by municipality in Brazil

## 0. Settings ------------------------------------------------------------------
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

## 1. Prepare data -----------------------------------------------------------
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
  purrr::set_names(c('year', 'vacc_cov_polio', 'total')) %>% 
  dplyr::select(-total) %>% 
  dplyr::filter(year > 2015 & year != 'Total') %>% 
  dplyr::mutate(across(.cols = everything(), ~as.numeric(.)))

doses_cv_mun <- vroom::vroom(file = 'input/doses_aplicadas_cv/doses_cv_2006_2021.csv',delim = ';',
                             .name_repair = ~str_remove(string = ., pattern = '/'),
                             locale = locale(encoding = 'latin1')) %>% 
  dplyr::rename_with(.fn = ~stringr::str_to_lower(.),
                     .cols = everything()) %>%
  dplyr::select(-contains('total')) %>% 
  tidyr::pivot_longer(cols = -c('municipio'),
                      names_to = 'date',
                      values_to = str_c('vacc_cov_polio')) %>% 
  dplyr::mutate(year = str_extract(date,'[0-9]+'),
                cod_mun = str_extract(municipio, '[0-9]+')) %>% 
  dplyr::select(-c('date','municipio')) %>%
  dplyr::mutate(across(.cols = everything(), ~as.numeric(.)))

polio_cov_year_br <- polio_base %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(across(.cols= starts_with('vacc'),
                          ~sum(.,na.rm = T)), .groups = 'drop')  %>% 
  dplyr::full_join(doses_cv, by = 'year') %>% 
  dplyr::mutate(vacc_total = vacc_vip_3_dose+ vacc_vop_3_dose,
                diff_vacc = vacc_total - vacc_cov_polio,
                perc_diff = (diff_vacc/vacc_cov_polio)*100)

polio_cov_year_mun <- polio_base %>%
  dplyr::group_by(year,cod_mun) %>%
  dplyr::summarise(across(.cols= starts_with('vacc'),
                          ~sum(.,na.rm = T)), .groups = 'drop')  %>% 
  dplyr::full_join(doses_cv_mun, by = c('year','cod_mun')) %>% 
  dplyr::mutate(vacc_total = vacc_vip_3_dose+ vacc_vop_3_dose,
                diff_vacc = vacc_total - vacc_cov_polio,
                perc_diff = (diff_vacc/vacc_cov_polio)*100,
                vacc_total_k = vacc_total/1000,
                vacc_cov_polio_k = vacc_cov_polio/1000)

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

votes_bolso <- vroom::vroom(file= 'input/votos_bolsonaro.csv') %>% 
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
                cov_accum = vacc_accum/goal_accum,
                cov_month = vacc_total/goal_month,
                time= as.numeric(time)) %>% 
  dplyr::ungroup()%>% 
  dplyr::full_join(votes_bolso, by='cod_mun') %>% 
  dplyr::full_join(speech, by = c('month_number', 'year')) %>% 
  dplyr::mutate(bolso_70 = case_when(
    perc_bolso > 0.7 ~ 1,
    T ~ 0),
    bolso_60 = case_when(
      perc_bolso > 0.6 ~ 1,
      T ~ 0))

## 2. Correlation between different data sources -------------------------------
corr_doses <- polio_cob_year_mun %>%
  ggpubr::ggscatter(data = ., x = "vacc_total_k", y = "vacc_cob_polio_k", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "Doses administered (monthly)", 
                    ylab = "Coverage doses (annual)", 
                    add.params = list(color = "#5c66a8", fill = "lightgray", size = 1),
                    title = 'Correlation between administered doses and vaccination coverage doses (in thousands)') +
  theme_minimal() + ylim(0,150) + 
  theme(title = element_text(size = 8)) + labs(caption = 'Note: "Administered doses" refers to data on polio vaccine doses (oral and injectable) that are made available monthly on TabNet.\n "Coverage doses" refers to the number of doses used to calculate polio vaccination coverage that are made available annually on TabNet.')

ggsave(corr_doses,
       filename = 'output/plot/correlation_doses.png',
       width = 20, height = 12, device = 'png', bg = 'white',
       units = 'cm')

## 3. Summary -------------------------------------------------------------------
panel_polio %>% 
  dplyr::select(vacc_vip_3_dose, vacc_vop_3_dose) %>%
  as.data.frame() %>% 
  stargazer(title = "Variable analysis",
            summary.stat = c("mean", "sd", 'median'))

## Graphs ----------------------------------------------------------------------
### 4.1 Administered doses -----------------------------------------------------
dose_evolution <- panel_polio %>% 
  dplyr::filter(year %in% 2016:2021) %>% 
  dplyr::group_by(year,month_number) %>%
  dplyr::summarise(across(.cols= starts_with('vacc'),
                          ~sum(.,na.rm = T)), .groups = 'drop') %>%
  dplyr::mutate(Year = factor(year),
                vacc_total_k = vacc_total/1000) %>% 
  ggplot2::ggplot(mapping = aes(x= month_number, y =vacc_total_k,
                                color = Year))+
  geom_line(size= 1) + scale_x_continuous(breaks = 1:12, name = 'Month') +
  ylab('Administered doses') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Administered doses of polio vaccine (in thousands)') +
  scale_color_manual(values = c("#cf3a36", "#FAD510","#5c66a8",
                                "#F8AFA8","#0B775E", "#E2D200")) + 
  labs(caption = 'Note: Evolution of administered doses with TabNet data for polio vaccine (oral and injectable)')

ggsave(dose_evolution,
       filename = 'output/plot/evolution_doses.png',
       width = 20, height = 12, device = 'png', bg = 'white',
       units = 'cm')

### 4.2 Monthly coverage -----------------------------------------------------
monthly_cov_evolution <-  panel_polio %>% 
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
  ylab('Monthly coverage (%)') +
  scale_x_date(date_breaks = '6 month', limits = c(as.Date('2016-01-01'),
                                                   as.Date('2021-12-01'))) + 
  ylab('Monthly Coverage (%)') + xlab('Date') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Monthly coverage of polio vaccine (%)') +
  labs(caption = 'Note: Monthly coverage is calculated with the number of administered doses divided by the monthly target of children. \n The monthly target is the total number of children aged 0 to 4 years divided by 12. \n The red date refers to the month of inauguration of President Jair Bolsonaro.')

ggsave(monthly_cov_evolution,
       filename = 'output/plot/evolution_cob_month.png',
       width = 30, height = 20, device = 'png', bg = 'white',
       units = 'cm')

### 4.3 Monthly Cumulative Vaccination Coverage ---------------------------------
cumulative_cov_evolution <-  panel_polio %>% 
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
  ylab('Monthly cumulative coverage (%)' ) +
  scale_x_date(date_breaks = '6 month') + xlab('Date') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Monthly cumulative coverage of polio vaccine (%)') +
  labs(caption = 'Note: Monthly cumulative coverage is calculated with the number of doses administered up to that month divided by the accumulated monthly target \n The accumulated monthly target is the total number of children aged 0 to 4 years divided by 12 times the number of months. \n The red date refers to the month of inauguration of President Jair Bolsonaro.')

ggsave(cumulative_cov_evolution,
       filename = 'output/plot/evolution_cob_accum.png',
       width = 30, height = 20, device = 'png', bg = 'white',
       units = 'cm')

### 4.4 Monthly Vaccination Coverage by Bolsonaro Vote Share --------------------
monthly_cov_votes_evolution <- panel_polio %>% 
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
                Votes = case_when(win_bolso == 1 ~ 'More than 50% votes for Bolsonaro',
                                  win_bolso == 0 ~ 'Less than 50% votes for Bolsonaro')) %>% 
  tidyr::drop_na() %>% 
  mutate(
    date = str_c(month_number, year, sep = '-') %>% lubridate::my()
  ) %>% 
  ggplot2::ggplot(mapping = aes(x= date, y =cob_month*100, color=factor(Votes)))+
  geom_line(size= 1)  +
  geom_vline(xintercept = as.Date('2019-01-01'),
             linetype = 'dashed', colour = '#cf3a36', size = 1) +
  labs(x='Date', y='Monthly coverage (%)', color = '')+
  scale_x_date(date_breaks = '6 month') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Monthly polio vaccination coverage by votes for the elected president in 2018 (%)') +
  scale_color_manual(values = c("#F8AFA8", "#E2D200"))

ggsave(monthly_cov_votes_evolution,
       filename = 'output/plot/evol_cob_month_votes.png',
       width = 35, height = 20, device = 'png', bg = 'white',
       units = 'cm')


### 4.5 Monthly Cumulative Vaccination Coverage by Bolsonaro Vote Share ---------
cumulative_cov_votes_evolution <- panel_polio %>% 
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
                Votes = case_when(win_bolso == 1 ~ 'More than 50% votes for Bolsonaro',
                                  win_bolso == 0 ~ 'Less than 50% votes for Bolsonaro')) %>% 
  tidyr::drop_na() %>% 
  mutate(
    date = str_c(month_number, year, sep = '-') %>% lubridate::my()
  ) %>% 
  ggplot2::ggplot(mapping = aes(x= date, y =cob_accum*100, color=factor(Votes)))+
  geom_line(size= 1)  +
  geom_vline(xintercept = as.Date('2019-01-01'),
             linetype = 'dashed', colour = '#cf3a36', size = 1) +
  labs(x='Date', y='Monthly cumulative coverage (%)', color = '')+
  scale_x_date(date_breaks = '6 month') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Monthly cumulative polio vaccination coverage by votes for the elected president in 2018 (%)') +
  scale_color_manual(values = c("#F8AFA8", "#E2D200"))

ggsave(cumulative_cov_votes_evolution,
       filename = 'output/plot/evol_cob_accum_votes.png',
       width = 35, height = 20, device = 'png', bg = 'white',
       units = 'cm')


## 4.6 Monthly Cumulative Vaccination Coverage by Bolsonaro Vote Share 
# and Presidential Speeches ----------------------------------------------------
monthly_cov_evolution <-  panel_polio %>% 
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
  ylab('Monthly cumulative coverage') +
  scale_x_date(date_breaks = '3 month', limits = c(as.Date('2020-01-01'),
                                                   as.Date('2021-12-01'))) + 
  ylab('Monthly Coverage (%)') + xlab('Date') +
  theme(axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        title = element_text(size = 10)) + theme_minimal() +
  ggtitle('Monthly coverage of polio vaccine (%)') 

ggsave(monthly_cov_evolution,
       filename = 'output/plot/evolution_cob_month.png',
       width = 30, height = 20, device = 'png', bg = 'white',
       units = 'cm')