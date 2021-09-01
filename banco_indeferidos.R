# pacotes -----------------------------------------------------------------------------------------------------------------
library(tidyverse)

# download dos datasets ---------------------------------------------------------------------------------------------------
urls <- c(
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-04-01T18%3A14%3A03.739Z/indeferidos012019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-04-01T18%3A18%3A14.815Z/indeferidos022019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-04-02T21%3A05%3A02.296Z/indeferidos-03-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-05-31T20%3A40%3A31.141Z/indeferidos-04-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-06-28T14%3A52%3A03.892Z/indeferidos-05-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-07-25T17%3A47%3A33.471Z/indeferidos-06-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-09-16T17%3A57%3A57.137Z/indeferidos-07-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-09-27T17%3A30%3A13.308Z/indeferidos-08-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-12-03T13%3A10%3A50.222Z/indeferidos-09-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-12-03T13%3A11%3A59.586Z/indeferidos-10-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2021-01-05T20%3A45%3A26.964Z/beneficios-indeferidos-11-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-01-21T13%3A28%3A44.758Z/indeferidos-12-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T13%3A59%3A15.088Z/beneficios-indeferidos-01-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T14%3A00%3A58.797Z/beneficios-indeferidos-02-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T14%3A02%3A42.051Z/beneficios-indeferidos-03-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T14%3A03%3A56.548Z/beneficios-indeferidos-04-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T14%3A05%3A19.708Z/beneficios-indeferidos-05-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T14%3A06%3A34.319Z/beneficios-indeferidos-06-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T14%3A07%3A59.921Z/beneficios-indeferidos-07-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T14%3A09%3A26.610Z/beneficios-indeferidos-08-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T14%3A10%3A45.282Z/beneficios-indeferidos-09-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-17T14%3A12%3A34.210Z/beneficios-indeferidos-10-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2021-01-08T14%3A53%3A05.108Z/beneficios-indeferidos-11-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2021-01-28T14%3A51%3A42.043Z/beneficios-indeferidos-12-2020.csv'
  
)

mes <- c(
  'jan',
  'fev',
  'mar',
  'abr',
  'mai',
  'jun',
  'jul',
  'ago',
  'set',
  'out',
  'nov',
  'dez'
)

ano <- c(
  '2019',
  '2020'
)


cont <- 1
for (i in ano){
  for (j in mes){
    var <- paste0('df_indef_', j, '_', i)
    assign(
      var, 
      read_csv2(url(urls[cont]), local = locale(encoding = "latin1")) %>% 
        janitor::clean_names()
    )
    cont <- cont + 1
  }
}



# Tabela com os totais ----------------------------------------

df_2019_indef <- tibble(
  ano = rep(2019, 12),
  mes = c('jan','fev','mar','abr','mai','jun','jul','ago','set','out','nov','dez'),
  mes_num = 1:12,
  tot_indef = c(dim(df_indef_jan_2019)[1],
                dim(df_indef_fev_2019)[1],
                dim(df_indef_mar_2019)[1],
                dim(df_indef_abr_2019)[1],
                dim(df_indef_mai_2019)[1],
                dim(df_indef_jun_2019)[1],
                dim(df_indef_jul_2019)[1],
                dim(df_indef_ago_2019)[1],
                dim(df_indef_set_2019)[1],
                dim(df_indef_out_2019)[1],
                dim(df_indef_nov_2019)[1],
                dim(df_indef_dez_2019)[1])

)

df_2020_indef <- tibble(
  ano = rep(2020, 12),
  mes = c('jan','fev','mar','abr','mai','jun','jul','ago','set','out','nov','dez'),
  mes_num = 1:12,
  tot_indef = c(dim(df_indef_jan_2020)[1],
                dim(df_indef_fev_2020)[1],
                dim(df_indef_mar_2020)[1],
                dim(df_indef_abr_2020)[1],
                dim(df_indef_mai_2020)[1],
                dim(df_indef_jun_2020)[1],
                dim(df_indef_jul_2020)[1],
                dim(df_indef_ago_2020)[1],
                dim(df_indef_set_2020)[1],
                dim(df_indef_out_2020)[1],
                dim(df_indef_nov_2020)[1],
                dim(df_indef_dez_2020)[1])
  
)


df_indef_total <- rbind(df_2019_indef, df_2020_indef)
df_indef_total$`X1` = 1:24

View(df_indef_total)

df <- read_csv('dados/dados_concedidos_2019-2020_igor.csv')

df <- left_join(df, df_indef_total, by='X1')

df %>% glimpse()




write.csv(df, 'dados/dados_concedidos_2019-2020_igor.csv')




