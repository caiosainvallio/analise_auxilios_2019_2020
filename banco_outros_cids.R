# pacotes -----------------------------------------------------------------------------------------------------------------
library(tidyverse)

# download dos datasets ---------------------------------------------------------------------------------------------------
urls <- c(
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-03-29T20%3A09%3A19.021Z/concedidos-01-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-03-29T20%3A11%3A16.272Z/concedidos-02-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-04-02T21%3A02%3A34.847Z/concedidos-03-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-05-27T19%3A00%3A54.628Z/concedidos-04-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-06-28T14%3A49%3A12.623Z/concedidos-05-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-07-25T17%3A43%3A41.238Z/concedidos-06-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-09-16T17%3A55%3A22.772Z/concedidos-07-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-09-27T17%3A27%3A16.781Z/concedidos-08-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-12-03T13%3A07%3A30.257Z/concedidos-09-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-12-03T13%3A09%3A17.363Z/concedidos-10-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2019-12-19T19%3A18%3A22.806Z/concedidos-11-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-01-21T13%3A30%3A43.262Z/concedidos-12-2019.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T14%3A22%3A53.002Z/beneficios-concedidos-01-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T14%3A24%3A39.980Z/beneficios-concedidos-02-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T14%3A26%3A44.754Z/beneficios-concedidos-03-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T14%3A27%3A57.491Z/beneficios-concedidos-04-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T14%3A29%3A28.163Z/beneficios-concedidos-05-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T14%3A30%3A51.923Z/beneficios-concedidos-06-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T14%3A58%3A55.684Z/beneficios-concedidos-07-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T15%3A01%3A06.708Z/beneficios-concedidos-08-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T15%3A02%3A51.748Z/beneficios-concedidos-09-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2020-12-16T15%3A04%3A30.284Z/beneficios-concedidos-10-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2021-01-08T14%3A38%3A20.008Z/beneficios-concedidos-11-2020.csv',
  'http://dadosabertos.dataprev.gov.br/storage/f/2021-01-28T14%3A55%3A42.046Z/beneficios-concedidos-12-2020.csv'
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
    var <- paste0('df_', j, '_', i)
    assign(
      var, 
      read_csv2(url(urls[cont]), local = locale(encoding = "latin1")) %>% 
        janitor::clean_names()
    )
    cont <- cont + 1
  }
}


'M54' # Dorsalgia
'M543' # Ciatica
'M544' # Lumbago com ciatica
'I21' # infarto agudo do miocardio'
'C61' # Neoplasi malignina da protata
'J18' # Pneumonia



#  2019 -------------------------------------------------------------------------------------------------------------------


# janeiro 2019 ------------------------------------------------------------------------------------------------------------
df_jan_2019 

# limpeza e analise de dados
df_jan_2019 <- janitor::clean_names(df_jan_2019)

## filtro para CIDs
m54 <- df_jan_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_jan_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_jan_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_jan_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_jan_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_jan_2019 %>% filter(cid == 'J18') %>% dim()



### adicionar linha
df <- tibble(
  ano = 2019,
  mes = 'janeiro',
  mes_num = 1,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# fevereiro 2019 ----------------------------------------------------------------------------------------------------------
df_fev_2019

# limpeza e analise de dados
df_fev_2019 <- janitor::clean_names(df_fev_2019)

## filtro para CIDs
m54 <- df_fev_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_fev_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_fev_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_fev_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_fev_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_fev_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'fevereiro',
  mes_num = 2,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)



# marco 2019 --------------------------------------------------------------------------------------------------------------
df_mar_2019

# limpeza e analise de dados
df_mar_2019 <- janitor::clean_names(df_mar_2019)

## filtro para CIDs
m54 <- df_mar_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_mar_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_mar_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_mar_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_mar_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_mar_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'marco',
  mes_num = 3,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)

# abril 2019 --------------------------------------------------------------------------------------------------------------
df_abr_2019

# limpeza e analise de dados
df_abr_2019 <- janitor::clean_names(df_abr_2019)

## filtro para CIDs
m54 <- df_abr_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_abr_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_abr_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_abr_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_abr_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_abr_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'abril',
  mes_num = 4,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# maio 2019 ---------------------------------------------------------------------------------------------------------------
df_mai_2019

# limpeza e analise de dados
df_mai_2019 <- janitor::clean_names(df_mai_2019)

## filtro para CIDs
m54 <- df_mai_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_mai_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_mai_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_mai_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_mai_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_mai_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'maio',
  mes_num = 5,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)

# junho 2019 --------------------------------------------------------------------------------------------------------------
df_jun_2019

# limpeza e analise de dados
df_jun_2019 <- janitor::clean_names(df_jun_2019)

## filtro para CIDs
m54 <- df_jun_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_jun_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_jun_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_jun_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_jun_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_jun_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'junho',
  mes_num = 6,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# julho 2019 --------------------------------------------------------------------------------------------------------------
df_jul_2019

# limpeza e analise de dados
df_jul_2019 <- janitor::clean_names(df_jul_2019)

## filtro para CIDs
m54 <- df_jul_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_jul_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_jul_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_jul_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_jul_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_jul_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'julho',
  mes_num = 7,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)



# agosto 2019 -------------------------------------------------------------------------------------------------------------
df_ago_2019

# limpeza e analise de dados
df_ago_2019 <- janitor::clean_names(df_ago_2019)



## filtro para CIDs
m54 <- df_ago_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_ago_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_ago_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_ago_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_ago_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_ago_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'agosto',
  mes_num = 8,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# setembro 2019 -----------------------------------------------------------------------------------------------------------
df_set_2019

# limpeza e analise de dados
df_set_2019 <- janitor::clean_names(df_set_2019)

## filtro para CIDs
m54 <- df_set_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_set_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_set_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_set_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_set_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_set_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'setembro',
  mes_num = 9,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# outubro 2019 ------------------------------------------------------------------------------------------------------------
df_out_2019

# limpeza e analise de dados
df_out_2019 <- janitor::clean_names(df_out_2019)


## filtro para CIDs
m54 <- df_out_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_out_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_out_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_out_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_out_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_out_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'outubro',
  mes_num = 10,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# novembro 2019 -----------------------------------------------------------------------------------------------------------
df_nov_2019

# limpeza e analise de dados
df_nov_2019 <- janitor::clean_names(df_nov_2019)

## filtro para CIDs
m54 <- df_nov_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_nov_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_nov_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_nov_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_nov_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_nov_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'novembro',
  mes_num = 11,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# dezembro 2019 -----------------------------------------------------------------------------------------------------------
df_dez_2019

# limpeza e analise de dados
df_dez_2019 <- janitor::clean_names(df_dez_2019)

## filtro para CIDs
m54 <- df_dez_2019 %>% filter(cid == 'M54') %>% dim()
m543 <- df_dez_2019 %>% filter(cid == 'M543') %>% dim()
m544 <- df_dez_2019 %>% filter(cid == 'M544') %>% dim()
i21 <- df_dez_2019 %>% filter(cid == 'I21') %>% dim()
c61 <- df_dez_2019 %>% filter(cid == 'C61') %>% dim()
j18 <- df_dez_2019 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'dezembro',
  mes_num = 12,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)



# dados 2019 --------------------------------------------------------------------------------------------------------------



#  2020 -------------------------------------------------------------------------------------------------------------------
# janeiro 2020 ------------------------------------------------------------------------------------------------------------
df_jan_2020

# limpeza e analise de dados
df_jan_2020 <- janitor::clean_names(df_jan_2020)

## filtro para CIDs
m54 <- df_jan_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_jan_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_jan_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_jan_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_jan_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_jan_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'janeiro',
  mes_num = 1,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)



# devereiro 2020 ----------------------------------------------------------------------------------------------------------
df_fev_2020

# limpeza e analise de dados
df_fev_2020 <- janitor::clean_names(df_fev_2020)


## filtro para CIDs
m54 <- df_fev_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_fev_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_fev_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_fev_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_fev_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_fev_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'fevereiro',
  mes_num = 2,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# marco 2020 --------------------------------------------------------------------------------------------------------------
df_mar_2020

# limpeza e analise de dados
df_mar_2020 <- janitor::clean_names(df_mar_2020)

## filtro para CIDs
m54 <- df_mar_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_mar_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_mar_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_mar_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_mar_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_mar_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'marco',
  mes_num = 3,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# abril 2020 --------------------------------------------------------------------------------------------------------------
df_abr_2020

# limpeza e analise de dados
df_abr_2020 <- janitor::clean_names(df_abr_2020)

## filtro para CIDs
m54 <- df_abr_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_abr_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_abr_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_abr_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_abr_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_abr_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'abril',
  mes_num = 4,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# maio 2020 ---------------------------------------------------------------------------------------------------------------
df_mai_2020

# limpeza e analise de dados
df_mai_2020 <- janitor::clean_names(df_mai_2020)


## filtro para CIDs
m54 <- df_mai_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_mai_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_mai_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_mai_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_mai_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_mai_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'maio',
  mes_num = 5,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# junho 2020 --------------------------------------------------------------------------------------------------------------
df_jun_2020

# limpeza e analise de dados
df_jun_2020 <- janitor::clean_names(df_jun_2020)


## filtro para CIDs
m54 <- df_jun_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_jun_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_jun_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_jun_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_jun_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_jun_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'junho',
  mes_num = 6,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)

# julho 2020 --------------------------------------------------------------------------------------------------------------
df_jul_2020

# limpeza e analise de dados
df_jul_2020 <- janitor::clean_names(df_jul_2020)

## filtro para CIDs
m54 <- df_jul_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_jul_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_jul_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_jul_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_jul_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_jul_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'julho',
  mes_num = 7,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# agosto 2020 -------------------------------------------------------------------------------------------------------------
df_ago_2020

# limpeza e analise de dados
df_ago_2020 <- janitor::clean_names(df_ago_2020)

## filtro para CIDs
m54 <- df_ago_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_ago_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_ago_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_ago_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_ago_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_ago_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'agosto',
  mes_num = 8,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# setembro 2020 -----------------------------------------------------------------------------------------------------------
df_set_2020

# limpeza e analise de dados
df_set_2020 <- janitor::clean_names(df_set_2020)

## filtro para CIDs
m54 <- df_set_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_set_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_set_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_set_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_set_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_set_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'setembro',
  mes_num = 9,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# outubro 2020 ------------------------------------------------------------------------------------------------------------
df_out_2020

# limpeza e analise de dados
df_out_2020 <- janitor::clean_names(df_out_2020)

## filtro para CIDs
m54 <- df_out_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_out_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_out_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_out_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_out_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_out_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'outubro',
  mes_num = 10,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)


# novembro 2020 -----------------------------------------------------------------------------------------------------------
df_nov_2020

# limpeza e analise de dados
df_nov_2020 <- janitor::clean_names(df_nov_2020)

## filtro para CIDs
m54 <- df_nov_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_nov_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_nov_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_nov_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_nov_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_nov_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'novembro',
  mes_num = 11,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)




# dezembro 2020 -----------------------------------------------------------------------------------------------------------

df_dez_2020

# limpeza e analise de dados
df_dez_2020 <- janitor::clean_names(df_dez_2020)

## filtro para CIDs
m54 <- df_dez_2020 %>% filter(cid == 'M54') %>% dim()
m543 <- df_dez_2020 %>% filter(cid == 'M543') %>% dim()
m544 <- df_dez_2020 %>% filter(cid == 'M544') %>% dim()
i21 <- df_dez_2020 %>% filter(cid == 'I21') %>% dim()
c61 <- df_dez_2020 %>% filter(cid == 'C61') %>% dim()
j18 <- df_dez_2020 %>% filter(cid == 'J18') %>% dim()


### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'dezembro',
  mes_num = 12,
  m54 = m54[1],
  m543 = m543[1],
  m544 = m544[1],
  i21 = i21[1],
  c61 = c61[1],
  j18 = j18[1],
)




# dados 2019 e 2020 -------------------------------------------------------------------------------------------------------

write.csv(df, 'dados/dados_concedidos_outros_CIDs_2019-2020_igor.csv')

df_outro <- read_csv('dados/dados_concedidos_outros_CIDs_2019-2020_igor.csv')


