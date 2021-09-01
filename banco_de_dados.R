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


#  2019 -------------------------------------------------------------------------------------------------------------------


# janeiro 2019 ------------------------------------------------------------------------------------------------------------
df_jan_2019

# limpeza e analise de dados
df_jan_2019 <- janitor::clean_names(df_jan_2019)

## total linhas
tot_df_jan_2019 <- dim(df_jan_2019)[1]

## filtro para M545
df_jan_2019 <- df_jan_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_jan_2019 <- dim(df_jan_2019)[1]

## proporção
tot_m545_df_jan_2019 / tot_df_jan_2019

## especie 
df_jan_2019 %>% 
  group_by(especie) %>% 
  summarise(n())


## despacho
df_jan_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())


## idade média
df_jan_2019 <- df_jan_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
         )

df_jan_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_jan_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_jan_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_jan_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_jan_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_jan_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- tibble(
  ano = 2019,
  mes = 'janeiro',
  mes_num = 1,
  total_geral = 338947,
  total_m545 = 2811,
  prop = 0.008293332,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 22,
  aposent_invalidez_acidente_trabalho = 29,
  aposentadoria_invalidez_previdenciaria = 202,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 2,
  auxilio_doenca_por_acidente_do_trabalho = 313,
  auxilio_doenca_revidenciario = 2243,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 47,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 58,
  concessao_decorrente_de_acao_judicial = 36,
  concessao_em_fase_recursal = 0,
  concessao_normal = 2670,
  
  idade_media = 48.7,
  idade_dp = 11.3,
  
  sexo_feminino = 1326,
  sexo_masculino = 1485,
  
  # clientela
  urbano = 2487,
  rural = 324,
  
  # vinculo_dependentes
  nao_informado = 2391,
  filho = 420,
  
  # forma_filiacao
  autonomo = 428,
  desempregado = 440,
  domestico = 81,
  empregado = 1447,
  empresario = 0,
  facultativo = 75,
  optante_pela_lei_618474 = 7,
  segurado_especial = 324,
  trabalhador_avulso = 9,
  
  # uf
  acre = 4,
  alagoas = 36,
  amapa = 3,
  amazonas = 27,
  bahia = 112,
  ceara = 45,
  distrito_federal = 45,
  espirito_santo = 69,
  goias = 69,
  maranhao = 44,
  mato_grosso = 49,
  mato_grosso_do_sul = 53,
  minas_gerais = 402,
  para = 95,
  paraiba = 20,
  parana = 204,
  pernambuco = 79,
  piaui = 48,
  rio_de_janeiro = 249,
  rio_grande_do_norte = 53,
  rio_grande_do_sul = 246,
  rondonia = 60,
  roraima = 1,
  santa_catarina = 283,
  sao_paulo = 461,
  sergipe = 34,
  tocantins = 20
)


# fevereiro 2019 ----------------------------------------------------------------------------------------------------------
df_fev_2019

# limpeza e analise de dados
df_fev_2019 <- janitor::clean_names(df_fev_2019)

## total linhas
tot_df_fev_2019 <- dim(df_fev_2019)[1]

## filtro para M545
df_fev_2019 <- df_fev_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_fev_2019 <- dim(df_fev_2019)[1]

## proporção
tot_m545_df_fev_2019 / tot_df_fev_2019

## especie 
df_fev_2019 %>% 
  group_by(especie) %>% 
  summarise(n())


## despacho
df_fev_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())


## idade média
df_fev_2019 <- df_fev_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_fev_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
    )

## sexo
df_fev_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_fev_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_fev_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_fev_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_fev_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'fevereiro',
  mes_num = 2,
  total_geral = 409841,
  total_m545 = 3437,
  prop = 0.008386179,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 18,
  aposent_invalidez_acidente_trabalho = 34,
  aposentadoria_invalidez_previdenciaria = 232,
  auxilio_acidente = 4,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 370,
  auxilio_doenca_revidenciario = 2779,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 58,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 62,
  concessao_decorrente_de_acao_judicial = 42,
  concessao_em_fase_recursal = 2,
  concessao_normal = 3273,
  
  
  idade_media = 48.2,
  idade_dp = 11.4,
  
  sexo_feminino = 1642,
  sexo_masculino = 1795,
  
  # clientela
  urbano = 3139,
  rural = 298,
  
  # vinculo_dependentes
  nao_informado = 2887,
  filho = 550,
  
  # forma_filiacao
  autonomo = 564,
  desempregado = 535,
  domestico = 148,
  empregado = 1790,
  empresario = 0,
  facultativo = 86,
  optante_pela_lei_618474 = 7,
  segurado_especial = 298,
  trabalhador_avulso = 9,
  
  # uf
  acre = 7,
  alagoas = 51,
  amapa = 2,
  amazonas = 42,
  bahia = 111,
  ceara = 66,
  distrito_federal = 55,
  espirito_santo = 86,
  goias = 107,
  maranhao = 53,
  mato_grosso = 56,
  mato_grosso_do_sul = 73,
  minas_gerais = 532,
  para = 117,
  paraiba = 34,
  parana = 244,
  pernambuco = 104,
  piaui = 45,
  rio_de_janeiro = 321,
  rio_grande_do_norte = 70,
  rio_grande_do_sul = 236,
  rondonia = 67,
  roraima = 3,
  santa_catarina = 351,
  sao_paulo = 528,
  sergipe = 51,
  tocantins = 25
)


# marco 2019 --------------------------------------------------------------------------------------------------------------
df_mar_2019

# limpeza e analise de dados
df_mar_2019 <- janitor::clean_names(df_mar_2019)

## total linhas
tot_df_mar_2019 <- dim(df_mar_2019)[1]

## filtro para M545
df_mar_2019 <- df_mar_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_mar_2019 <- dim(df_mar_2019)[1]

## proporção
tot_m545_df_mar_2019 / tot_df_mar_2019

## especie 
df_mar_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_mar_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_mar_2019 <- df_mar_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_mar_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_mar_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_mar_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_mar_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_mar_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_mar_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'marco',
  mes_num = 3,
  total_geral = 364207,
  total_m545 = 3049,
  prop = 0.008371613,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 12,
  aposent_invalidez_acidente_trabalho = 36,
  aposentadoria_invalidez_previdenciaria = 251,
  auxilio_acidente = 4,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 323,
  auxilio_doenca_revidenciario = 2423,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 42,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 80,
  concessao_decorrente_de_acao_judicial = 44,
  concessao_em_fase_recursal = 1,
  concessao_normal = 2882,
  
  
  idade_media = 47.9,
  idade_dp = 11.1,
  
  sexo_feminino = 1430,
  sexo_masculino = 1619,
  
  # clientela
  urbano = 2740,
  rural = 309,
  
  # vinculo_dependentes
  nao_informado = 2556,
  filho = 493,
  
  # forma_filiacao
  autonomo = 472,
  desempregado = 420,
  domestico = 107,
  empregado = 1657,
  empresario = 0,
  facultativo = 70,
  optante_pela_lei_618474 = 5,
  segurado_especial = 307,
  trabalhador_avulso = 11,
  
  # uf
  acre = 4,
  alagoas = 56,
  amapa = 1,
  amazonas = 25,
  bahia = 128,
  ceara = 55,
  distrito_federal = 49,
  espirito_santo = 50,
  goias = 88,
  maranhao = 51,
  mato_grosso = 53,
  mato_grosso_do_sul = 71,
  minas_gerais = 512,
  para = 85,
  paraiba = 23,
  parana = 264,
  pernambuco = 100,
  piaui = 43,
  rio_de_janeiro = 248,
  rio_grande_do_norte = 45,
  rio_grande_do_sul = 231,
  rondonia = 54,
  roraima = 0,
  santa_catarina = 319,
  sao_paulo = 441,
  sergipe = 29,
  tocantins = 24
)


# abril 2019 --------------------------------------------------------------------------------------------------------------
df_abr_2019

# limpeza e analise de dados
df_abr_2019 <- janitor::clean_names(df_abr_2019)

## total linhas
tot_df_abr_2019 <- dim(df_abr_2019)[1]
tot_df_abr_2019

## filtro para M545
df_abr_2019 <- df_abr_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_abr_2019 <- dim(df_abr_2019)[1]
tot_m545_df_abr_2019

## proporção
tot_m545_df_abr_2019 / tot_df_abr_2019

## especie 
df_abr_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_abr_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_abr_2019 <- df_abr_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_abr_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_abr_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_abr_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_abr_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_abr_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_abr_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'abril',
  mes_num = 4,
  total_geral = 428183,
  total_m545 = 3445,
  prop = 0.008045625,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 21,
  aposent_invalidez_acidente_trabalho = 53,
  aposentadoria_invalidez_previdenciaria = 291,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 1,
  auxilio_doenca_por_acidente_do_trabalho = 328,
  auxilio_doenca_revidenciario = 2751,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 54,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 62,
  concessao_decorrente_de_acao_judicial = 44,
  concessao_em_fase_recursal = 0,
  concessao_normal = 3285,
  
  
  idade_media = 48.5,
  idade_dp = 11.2,
  
  sexo_feminino = 1638,
  sexo_masculino = 1807,
  
  # clientela
  urbano = 3105,
  rural = 340,
  
  # vinculo_dependentes
  nao_informado = 2896,
  filho = 549,
  
  # forma_filiacao
  autonomo = 544,
  desempregado = 484,
  domestico = 125,
  empregado = 1838,
  empresario = 0,
  facultativo = 94,
  optante_pela_lei_618474 = 9,
  segurado_especial = 337,
  trabalhador_avulso = 14,
  
  # uf
  acre = 4,
  alagoas = 51,
  amapa = 2,
  amazonas = 57,
  bahia = 122,
  ceara = 75,
  distrito_federal = 72,
  espirito_santo = 72,
  goias = 112,
  maranhao = 47,
  mato_grosso = 61,
  mato_grosso_do_sul = 83,
  minas_gerais = 478,
  para = 90,
  paraiba = 25,
  parana = 330,
  pernambuco = 113,
  piaui = 55,
  rio_de_janeiro = 251,
  rio_grande_do_norte = 36,
  rio_grande_do_sul = 269,
  rondonia = 69,
  roraima = 0,
  santa_catarina = 387,
  sao_paulo = 512,
  sergipe = 49,
  tocantins = 23
)



# maio 2019 ---------------------------------------------------------------------------------------------------------------
df_mai_2019

# limpeza e analise de dados
df_mai_2019 <- janitor::clean_names(df_mai_2019)

## total linhas
tot_df_mai_2019 <- dim(df_mai_2019)[1]
tot_df_mai_2019

## filtro para M545
df_mai_2019 <- df_mai_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_mai_2019 <- dim(df_mai_2019)[1]
tot_m545_df_mai_2019

## proporção
tot_m545_df_mai_2019 / tot_df_mai_2019

## especie 
df_mai_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_mai_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_mai_2019 <- df_mai_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_mai_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_mai_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_mai_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_mai_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_mai_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_mai_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'maio',
  mes_num = 5,
  total_geral = 500917,
  total_m545 = 3546,
  prop = 0.007079017,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 19,
  aposent_invalidez_acidente_trabalho = 31,
  aposentadoria_invalidez_previdenciaria = 200,
  auxilio_acidente = 2,
  auxilio_acidente_previdenciario = 1,
  auxilio_doenca_por_acidente_do_trabalho = 408,
  auxilio_doenca_revidenciario = 2885,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 59,
  Conc_com_base_artigo_35_da_lei_821391 = 1,
  conc_decorrente_revisao_administrativa = 74,
  concessao_decorrente_de_acao_judicial = 59,
  concessao_em_fase_recursal = 0,
  concessao_normal = 3353,
  
  
  idade_media = 47.9,
  idade_dp = 11.1,
  
  sexo_feminino = 1726,
  sexo_masculino = 1820,
  
  # clientela
  urbano = NA,
  rural = NA,
  
  # vinculo_dependentes
  nao_informado = 2969,
  filho = 577,
  
  # forma_filiacao
  autonomo = 529,
  desempregado = 498,
  domestico = 119,
  empregado = 1902,
  empresario = 0,
  facultativo = 89,
  optante_pela_lei_618474 = 5,
  segurado_especial = 395,
  trabalhador_avulso = 9,
  
  # uf
  acre = 9,
  alagoas = 61,
  amapa = 3,
  amazonas = 47,
  bahia = 141,
  ceara = 45,
  distrito_federal = 51,
  espirito_santo = 88,
  goias = 123,
  maranhao = 59,
  mato_grosso = 54,
  mato_grosso_do_sul = 82,
  minas_gerais = 530,
  para = 112,
  paraiba = 32,
  parana = 341,
  pernambuco = 111,
  piaui = 76,
  rio_de_janeiro = 306,
  rio_grande_do_norte = 35,
  rio_grande_do_sul = 265,
  rondonia = 50,
  roraima = 0,
  santa_catarina = 367,
  sao_paulo = 503,
  sergipe = 44,
  tocantins = 11
)


# junho 2019 --------------------------------------------------------------------------------------------------------------
df_jun_2019

# limpeza e analise de dados
df_jun_2019 <- janitor::clean_names(df_jun_2019)

## total linhas
tot_df_jun_2019 <- dim(df_jun_2019)[1]
tot_df_jun_2019

## filtro para M545
df_jun_2019 <- df_jun_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_jun_2019 <- dim(df_jun_2019)[1]
tot_m545_df_jun_2019

## proporção
tot_m545_df_jun_2019 / tot_df_jun_2019

## especie 
df_jun_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_jun_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_jun_2019 <- df_jun_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_jun_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_jun_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_jun_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_jun_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_jun_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_jun_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'junho',
  mes_num = 6,
  total_geral = 402826,
  total_m545 = 2985,
  prop = 0.007410147,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 10,
  aposent_invalidez_acidente_trabalho = 26,
  aposentadoria_invalidez_previdenciaria = 196,
  auxilio_acidente = 2,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 335,
  auxilio_doenca_revidenciario = 2416,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 34,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 74,
  concessao_decorrente_de_acao_judicial = 46,
  concessao_em_fase_recursal = 1,
  concessao_normal = 2830,
  
  
  idade_media = 47.9,
  idade_dp = 11.0,
  
  sexo_feminino = 1405,
  sexo_masculino = 1580,
  
  # clientela
  urbano = NA,
  rural = NA,
  
  # vinculo_dependentes
  nao_informado = 2491,
  filho = 494,
  
  # forma_filiacao
  autonomo = 454,
  desempregado = 408,
  domestico = 94,
  empregado = 1647,
  empresario = 0,
  facultativo = 54,
  optante_pela_lei_618474 = 7,
  segurado_especial = 314,
  trabalhador_avulso = 7,
  
  # uf
  acre = 3,
  alagoas = 39,
  amapa = 4,
  amazonas = 48,
  bahia = 88,
  ceara = 69,
  distrito_federal = 35,
  espirito_santo = 65,
  goias = 97,
  maranhao = 77,
  mato_grosso = 66,
  mato_grosso_do_sul = 68,
  minas_gerais = 419,
  para = 102,
  paraiba = 26,
  parana = 260,
  pernambuco = 65,
  piaui = 51,
  rio_de_janeiro = 259,
  rio_grande_do_norte = 36,
  rio_grande_do_sul = 252,
  rondonia = 50,
  roraima = 1,
  santa_catarina = 281,
  sao_paulo = 473,
  sergipe = 33,
  tocantins = 18
)


# julho 2019 --------------------------------------------------------------------------------------------------------------
df_jul_2019

# limpeza e analise de dados
df_jul_2019 <- janitor::clean_names(df_jul_2019)

## total linhas
tot_df_jul_2019 <- dim(df_jul_2019)[1]
tot_df_jul_2019

## filtro para M545
df_jul_2019 <- df_jul_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_jul_2019 <- dim(df_jul_2019)[1]
tot_m545_df_jul_2019

## proporção
tot_m545_df_jul_2019 / tot_df_jul_2019

## especie 
df_jul_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_jul_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_jul_2019 <- df_jul_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_jul_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_jul_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_jul_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_jul_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_jul_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_jul_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'julho',
  mes_num = 7,
  total_geral = 460998,
  total_m545 = 2945,
  prop = 0.006388314,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 14,
  aposent_invalidez_acidente_trabalho = 25,
  aposentadoria_invalidez_previdenciaria = 107,
  auxilio_acidente = 1,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 340,
  auxilio_doenca_revidenciario = 2458,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 50,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 63,
  concessao_decorrente_de_acao_judicial = 48,
  concessao_em_fase_recursal = 2,
  concessao_normal = 2782,
  
  
  idade_media = 47.5,
  idade_dp = 11.1,
  
  sexo_feminino = 1356,
  sexo_masculino = 1589,
  
  # clientela
  urbano = NA,
  rural = NA,
  
  # vinculo_dependentes
  nao_informado = 2473,
  filho = 472,
  
  # forma_filiacao
  autonomo = 443,
  desempregado = 359,
  domestico = 113,
  empregado = 1617,
  empresario = 0,
  facultativo = 61,
  optante_pela_lei_618474 = 3,
  segurado_especial = 341,
  trabalhador_avulso = 8,
  
  # uf
  acre = 3,
  alagoas = 45,
  amapa = 0,
  amazonas = 46,
  bahia = 115,
  ceara = 68,
  distrito_federal = 41,
  espirito_santo = 60,
  goias = 91,
  maranhao = 63,
  mato_grosso = 48,
  mato_grosso_do_sul = 70,
  minas_gerais = 409,
  para = 114,
  paraiba = 25,
  parana = 281,
  pernambuco = 90,
  piaui = 51,
  rio_de_janeiro = 219,
  rio_grande_do_norte = 22,
  rio_grande_do_sul = 245,
  rondonia = 53,
  roraima = 0,
  santa_catarina = 276,
  sao_paulo = 458,
  sergipe = 35,
  tocantins = 17
)



# agosto 2019 -------------------------------------------------------------------------------------------------------------
df_ago_2019

# limpeza e analise de dados
df_ago_2019 <- janitor::clean_names(df_ago_2019)

## total linhas
tot_df_ago_2019 <- dim(df_ago_2019)[1]
tot_df_ago_2019

## filtro para M545
df_ago_2019 <- df_ago_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_ago_2019 <- dim(df_ago_2019)[1]
tot_m545_df_ago_2019

## proporção
tot_m545_df_ago_2019 / tot_df_ago_2019

## especie 
df_ago_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_ago_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_ago_2019 <- df_ago_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_ago_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_ago_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_ago_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_ago_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_ago_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_ago_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'agosto',
  mes_num = 8,
  total_geral = 510626,
  total_m545 = 3138,
  prop = 0.006145398,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 18,
  aposent_invalidez_acidente_trabalho = 16,
  aposentadoria_invalidez_previdenciaria = 88,
  auxilio_acidente = 1,
  auxilio_acidente_previdenciario = 1,
  auxilio_doenca_por_acidente_do_trabalho = 367,
  auxilio_doenca_revidenciario = 2647,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 49,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 61,
  concessao_decorrente_de_acao_judicial = 33,
  concessao_em_fase_recursal = 1,
  concessao_normal = 2994,
  
  
  idade_media = 47.2,
  idade_dp = 11.2,
  
  sexo_feminino = 1486,
  sexo_masculino = 1652,
  
  # clientela
  urbano = NA,
  rural = NA,
  
  # vinculo_dependentes
  nao_informado = 2633,
  filho = 505,
  
  # forma_filiacao
  autonomo = 471,
  desempregado = 400,
  domestico = 103,
  empregado = 1769,
  empresario = 0,
  facultativo = 77,
  optante_pela_lei_618474 = 1,
  segurado_especial = 309,
  trabalhador_avulso = 8,
  
  # uf
  acre = 4,
  alagoas = 58,
  amapa = 4,
  amazonas = 41,
  bahia = 110,
  ceara = 58,
  distrito_federal = 57,
  espirito_santo = 80,
  goias = 84,
  maranhao = 63,
  mato_grosso = 52,
  mato_grosso_do_sul = 66,
  minas_gerais = 484,
  para = 91,
  paraiba = 26,
  parana = 270,
  pernambuco = 84,
  piaui = 56,
  rio_de_janeiro = 267,
  rio_grande_do_norte = 29,
  rio_grande_do_sul = 239,
  rondonia = 56,
  roraima = 3,
  santa_catarina = 302,
  sao_paulo = 501,
  sergipe = 35,
  tocantins = 18
)


# setembro 2019 -----------------------------------------------------------------------------------------------------------
df_set_2019

# limpeza e analise de dados
df_set_2019 <- janitor::clean_names(df_set_2019)

## total linhas
tot_df_set_2019 <- dim(df_set_2019)[1]
tot_df_set_2019

## filtro para M545
df_set_2019 <- df_set_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_set_2019 <- dim(df_set_2019)[1]
tot_m545_df_set_2019

## proporção
tot_m545_df_set_2019 / tot_df_set_2019

## especie 
df_set_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_set_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_set_2019 <- df_set_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_set_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_set_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_set_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_set_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_set_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_set_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'setembro',
  mes_num = 9,
  total_geral = 454897,
  total_m545 = 3022,
  prop = 0.006643262,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 10,
  aposent_invalidez_acidente_trabalho = 10,
  aposentadoria_invalidez_previdenciaria = 109,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 385,
  auxilio_doenca_revidenciario = 2508,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 39,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 40,
  concessao_decorrente_de_acao_judicial = 33,
  concessao_em_fase_recursal = 0,
  concessao_normal = 2910,
  
  
  idade_media = 47.1,
  idade_dp = 11.1,
  
  sexo_feminino = 1472,
  sexo_masculino = 1550,
  
  # clientela
  urbano = NA,
  rural = NA,
  
  # vinculo_dependentes
  nao_informado = 2563,
  filho = 459,
  
  # forma_filiacao
  autonomo = 446,
  desempregado = 389,
  domestico = 118,
  empregado = 1710,
  empresario = 1,
  facultativo = 63,
  optante_pela_lei_618474 = 2,
  segurado_especial = 286,
  trabalhador_avulso = 7,
  
  # uf
  acre = 3,
  alagoas = 42,
  amapa = 4,
  amazonas = 30,
  bahia = 110,
  ceara = 71,
  distrito_federal = 44,
  espirito_santo = 78,
  goias = 98,
  maranhao = 65,
  mato_grosso = 45,
  mato_grosso_do_sul = 73,
  minas_gerais = 487,
  para = 93,
  paraiba = 17,
  parana = 265,
  pernambuco = 91,
  piaui = 40,
  rio_de_janeiro = 228,
  rio_grande_do_norte = 27,
  rio_grande_do_sul = 225,
  rondonia = 63,
  roraima = 3,
  santa_catarina = 292,
  sao_paulo = 475,
  sergipe = 25,
  tocantins = 28
)


# outubro 2019 ------------------------------------------------------------------------------------------------------------
df_out_2019

# limpeza e analise de dados
df_out_2019 <- janitor::clean_names(df_out_2019)

## total linhas
tot_df_out_2019 <- dim(df_out_2019)[1]
tot_df_out_2019

## filtro para M545
df_out_2019 <- df_out_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_out_2019 <- dim(df_out_2019)[1]
tot_m545_df_out_2019

## proporção
tot_m545_df_out_2019 / tot_df_out_2019

## especie 
df_out_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_out_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_out_2019 <- df_out_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_out_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_out_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_out_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_out_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_out_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_out_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'outubro',
  mes_num = 10,
  total_geral = 521354,
  total_m545 = 3051,
  prop = 0.00585207,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 12,
  aposent_invalidez_acidente_trabalho = 9,
  aposentadoria_invalidez_previdenciaria = 91,
  auxilio_acidente = 1,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 411,
  auxilio_doenca_revidenciario = 2527,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 57,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 80,
  concessao_decorrente_de_acao_judicial = 21,
  concessao_em_fase_recursal = 0,
  concessao_normal = 2893,
  
  
  idade_media = 46.7,
  idade_dp = 11.2,
  
  sexo_feminino = 1483,
  sexo_masculino = 1568,
  
  # clientela
  urbano = NA,
  rural = NA,
  
  # vinculo_dependentes
  nao_informado = 2491,
  filho = 560,
  
  # forma_filiacao
  autonomo = 481,
  desempregado = 381,
  domestico = 120,
  empregado = 1748,
  empresario = 0,
  facultativo = 62,
  optante_pela_lei_618474 = 1,
  segurado_especial = 250,
  trabalhador_avulso = 8,
  
  # uf
  acre = 2,
  alagoas = 48,
  amapa = 2,
  amazonas = 40,
  bahia = 123,
  ceara = 61,
  distrito_federal = 36,
  espirito_santo = 51,
  goias = 77,
  maranhao = 48,
  mato_grosso = 69,
  mato_grosso_do_sul = 59,
  minas_gerais = 463,
  para = 104,
  paraiba = 27,
  parana = 254,
  pernambuco = 95,
  piaui = 41,
  rio_de_janeiro = 242,
  rio_grande_do_norte = 35,
  rio_grande_do_sul = 223,
  rondonia = 67,
  roraima = 2,
  santa_catarina = 299,
  sao_paulo = 525,
  sergipe = 36,
  tocantins = 22
)


# novembro 2019 -----------------------------------------------------------------------------------------------------------
df_nov_2019

# limpeza e analise de dados
df_nov_2019 <- janitor::clean_names(df_nov_2019)

## total linhas
tot_df_nov_2019 <- dim(df_nov_2019)[1]
tot_df_nov_2019

## filtro para M545
df_nov_2019 <- df_nov_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_nov_2019 <- dim(df_nov_2019)[1]
tot_m545_df_nov_2019

## proporção
tot_m545_df_nov_2019 / tot_df_nov_2019

## especie 
df_nov_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_nov_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_nov_2019 <- df_nov_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_nov_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_nov_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_nov_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_nov_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_nov_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_nov_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'novembro',
  mes_num = 11,
  total_geral = 439311,
  total_m545 = 2598,
  prop = 0.005913806,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 10,
  aposent_invalidez_acidente_trabalho = 8,
  aposentadoria_invalidez_previdenciaria = 42,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 370,
  auxilio_doenca_revidenciario = 2168,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 51,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 46,
  concessao_decorrente_de_acao_judicial = 35,
  concessao_em_fase_recursal = 0,
  concessao_normal = 2466,
  
  
  idade_media = 46.4,
  idade_dp = 11.0,
  
  sexo_feminino = 1279,
  sexo_masculino = 1319,
  
  # clientela
  urbano = NA,
  rural = NA,
  
  # vinculo_dependentes
  nao_informado = 2138,
  filho = 460,
  
  # forma_filiacao
  autonomo = 360,
  desempregado = 306,
  domestico = 73,
  empregado = 1576,
  empresario = 0,
  facultativo = 69,
  optante_pela_lei_618474 = 1,
  segurado_especial = 206,
  trabalhador_avulso = 7,
  
  # uf
  acre = 3,
  alagoas = 50,
  amapa = 2,
  amazonas = 34,
  bahia = 95,
  ceara = 59,
  distrito_federal = 46,
  espirito_santo = 62,
  goias = 68,
  maranhao = 54,
  mato_grosso = 44,
  mato_grosso_do_sul = 64,
  minas_gerais = 382,
  para = 96,
  paraiba = 28,
  parana = 231,
  pernambuco = 62,
  piaui = 27,
  rio_de_janeiro = 242,
  rio_grande_do_norte = 30,
  rio_grande_do_sul = 164,
  rondonia = 40,
  roraima = 1,
  santa_catarina = 221,
  sao_paulo = 457,
  sergipe = 18,
  tocantins = 18
)


# dezembro 2019 -----------------------------------------------------------------------------------------------------------
df_dez_2019

# limpeza e analise de dados
df_dez_2019 <- janitor::clean_names(df_dez_2019)

## total linhas
tot_df_dez_2019 <- dim(df_dez_2019)[1]
tot_df_dez_2019

## filtro para M545
df_dez_2019 <- df_dez_2019 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_dez_2019 <- dim(df_dez_2019)[1]
tot_m545_df_dez_2019

## proporção
tot_m545_df_dez_2019 / tot_df_dez_2019

## especie 
df_dez_2019 %>% 
  group_by(especie) %>% 
  summarise(n())

## despacho
df_dez_2019 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_dez_2019 <- df_dez_2019 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_dez_2019 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_dez_2019 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_dez_2019 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_dez_2019 %>% 
  group_by(vinculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_dez_2019 %>% 
  group_by(forma_filiacao) %>% 
  summarise(n())

## unidade federetiva
df_dez_2019 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2019,
  mes = 'dezembro',
  mes_num = 12,
  total_geral = 373716,
  total_m545 = 2173,
  prop = 0.005814576,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 17,
  aposent_invalidez_acidente_trabalho = 7,
  aposentadoria_invalidez_previdenciaria = 41,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 312,
  auxilio_doenca_revidenciario = 1796,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 40,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 52,
  concessao_decorrente_de_acao_judicial = 10,
  concessao_em_fase_recursal = 1,
  concessao_normal = 2070,
  
  
  idade_media = 47.3,
  idade_dp = 10.9,
  
  sexo_feminino = 1028,
  sexo_masculino = 1145,
  
  # clientela
  urbano = NA,
  rural = NA,
  
  # vinculo_dependentes
  nao_informado = 1817,
  filho = 356,
  
  # forma_filiacao
  autonomo = 364,
  desempregado = 259,
  domestico = 87,
  empregado = 1198,
  empresario = 0,
  facultativo = 59,
  optante_pela_lei_618474 = 2,
  segurado_especial = 201,
  trabalhador_avulso = 3,
  
  # uf
  acre = 0,
  alagoas = 37,
  amapa = 0,
  amazonas = 28,
  bahia = 96,
  ceara = 41,
  distrito_federal = 50,
  espirito_santo = 44,
  goias = 48,
  maranhao = 47,
  mato_grosso = 29,
  mato_grosso_do_sul = 51,
  minas_gerais = 364,
  para = 74,
  paraiba = 25,
  parana = 194,
  pernambuco = 68,
  piaui = 25,
  rio_de_janeiro = 180,
  rio_grande_do_norte = 20,
  rio_grande_do_sul = 154,
  rondonia = 44,
  roraima = 2,
  santa_catarina = 170,
  sao_paulo = 345,
  sergipe = 24,
  tocantins = 13
)


# dados 2019 --------------------------------------------------------------------------------------------------------------

#write.csv(df, 'dados/dados_2019_igor.csv')


#  2020 -------------------------------------------------------------------------------------------------------------------
# janeiro 2020 ------------------------------------------------------------------------------------------------------------
df_jan_2020

# limpeza e analise de dados
df_jan_2020 <- janitor::clean_names(df_jan_2020)

## total linhas
tot_df_jan_2020 <- dim(df_jan_2020)[1]
tot_df_jan_2020

## filtro para M545
df_jan_2020 <- df_jan_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_jan_2020 <- dim(df_jan_2020)[1]
tot_m545_df_jan_2020

## proporção
tot_m545_df_jan_2020 / tot_df_jan_2020

## especie 
df_jan_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_jan_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_jan_2020 <- df_jan_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_jan_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_jan_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_jan_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_jan_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_jan_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_jan_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'janeiro',
  mes_num = 1,
  total_geral = 358075,
  total_m545 = 2083,
  prop = 0.005817217,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 14,
  aposent_invalidez_acidente_trabalho = 1,
  aposentadoria_invalidez_previdenciaria = 43,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 291,
  auxilio_doenca_revidenciario = 1734,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 26,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 29,
  concessao_decorrente_de_acao_judicial = 17,
  concessao_em_fase_recursal = 0,
  concessao_normal = 2011,
  
  
  idade_media = 46.5,
  idade_dp = 11.0,
  
  sexo_feminino = 953,
  sexo_masculino = 1130,
  
  # clientela
  urbano = 1924,
  rural = 159,
  
  # vinculo_dependentes
  nao_informado = 1717,
  filho = 366,
  
  # forma_filiacao
  autonomo = 333,
  desempregado = 270,
  domestico = 65,
  empregado = 1206,
  empresario = 0,
  facultativo = 45,
  optante_pela_lei_618474 = 1,
  segurado_especial = 159,
  trabalhador_avulso = 4,
  
  # uf
  acre = 3,
  alagoas = 26,
  amapa = 0,
  amazonas = 35,
  bahia = 70,
  ceara = 37,
  distrito_federal = 29,
  espirito_santo = 34,
  goias = 63,
  maranhao = 62,
  mato_grosso = 25,
  mato_grosso_do_sul = 61,
  minas_gerais = 302,
  para = 88,
  paraiba = 26,
  parana = 144,
  pernambuco = 56,
  piaui = 24,
  rio_de_janeiro = 201,
  rio_grande_do_norte = 16,
  rio_grande_do_sul = 154,
  rondonia = 35,
  roraima = 0,
  santa_catarina = 191,
  sao_paulo = 354,
  sergipe = 34,
  tocantins = 13
)



# devereiro 2020 ----------------------------------------------------------------------------------------------------------
df_fev_2020

# limpeza e analise de dados
df_fev_2020 <- janitor::clean_names(df_fev_2020)

## total linhas
tot_df_fev_2020 <- dim(df_fev_2020)[1]
tot_df_fev_2020

## filtro para M545
df_fev_2020 <- df_fev_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_fev_2020 <- dim(df_fev_2020)[1]
tot_m545_df_fev_2020

## proporção
tot_m545_df_fev_2020 / tot_df_fev_2020

## especie 
df_fev_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_fev_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_fev_2020 <- df_fev_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_fev_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_fev_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_fev_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_fev_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_fev_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_fev_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()


### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'fevereiro',
  mes_num = 2,
  total_geral = 392930,
  total_m545 = 2442,
  prop = 0.006214847,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 14,
  aposent_invalidez_acidente_trabalho = 2,
  aposentadoria_invalidez_previdenciaria = 17,
  auxilio_acidente = 1,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 348,
  auxilio_doenca_revidenciario = 2060,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 48,
  Conc_com_base_artigo_35_da_lei_821391 = 1,
  conc_decorrente_revisao_administrativa = 69,
  concessao_decorrente_de_acao_judicial = 24,
  concessao_em_fase_recursal = 1,
  concessao_normal = 2299,
  
  
  idade_media = 46.2,
  idade_dp = 11.3,
  
  sexo_feminino = 1174,
  sexo_masculino = 1268,
  
  # clientela
  urbano = 2261,
  rural = 181,
  
  # vinculo_dependentes
  nao_informado = 1987,
  filho = 455,
  
  # forma_filiacao
  autonomo = 355,
  desempregado = 301,
  domestico = 91,
  empregado = 1458,
  empresario = 0,
  facultativo = 48,
  optante_pela_lei_618474 = 2,
  segurado_especial = 181,
  trabalhador_avulso = 6,
  
  # uf
  acre = 2,
  alagoas = 21,
  amapa = 2,
  amazonas = 41,
  bahia = 99,
  ceara = 59,
  distrito_federal = 38,
  espirito_santo = 63,
  goias = 72,
  maranhao = 30,
  mato_grosso = 32,
  mato_grosso_do_sul = 55,
  minas_gerais = 373,
  para = 97,
  paraiba = 25,
  parana = 157,
  pernambuco = 66,
  piaui = 33,
  rio_de_janeiro = 225,
  rio_grande_do_norte = 43,
  rio_grande_do_sul = 168,
  rondonia = 45,
  roraima = 1,
  santa_catarina = 204,
  sao_paulo = 458,
  sergipe = 16,
  tocantins = 17
)



# marco 2020 --------------------------------------------------------------------------------------------------------------
df_mar_2020

# limpeza e analise de dados
df_mar_2020 <- janitor::clean_names(df_mar_2020)

## total linhas
tot_df_mar_2020 <- dim(df_mar_2020)[1]
tot_df_mar_2020

## filtro para M545
df_mar_2020 <- df_mar_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_mar_2020 <- dim(df_mar_2020)[1]
tot_m545_df_mar_2020

## proporção
tot_m545_df_mar_2020 / tot_df_mar_2020

## especie 
df_mar_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_mar_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_mar_2020 <- df_mar_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_mar_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_mar_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_mar_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_mar_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_mar_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_mar_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'marco',
  mes_num = 3,
  total_geral = 334191,
  total_m545 = 1950,
  prop = 0.005834987,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 17,
  aposent_invalidez_acidente_trabalho = 3,
  aposentadoria_invalidez_previdenciaria = 24,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 277,
  auxilio_doenca_revidenciario = 1629,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 39,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 37,
  concessao_decorrente_de_acao_judicial = 32,
  concessao_em_fase_recursal = 1,
  concessao_normal = 1841,
  
  
  idade_media = 46.6,
  idade_dp = 10.9,
  
  sexo_feminino = 924,
  sexo_masculino = 1026,
  
  # clientela
  urbano = 1772,
  rural = 178,
  
  # vinculo_dependentes
  nao_informado = 1626,
  filho = 324,
  
  # forma_filiacao
  autonomo = 287,
  desempregado = 264,
  domestico = 82,
  empregado = 1090,
  empresario = 0,
  facultativo = 42,
  optante_pela_lei_618474 = 1,
  segurado_especial = 178,
  trabalhador_avulso = 6,
  
  # uf
  acre = 2,
  alagoas = 22,
  amapa = 1,
  amazonas = 29,
  bahia = 86,
  ceara = 53,
  distrito_federal = 37,
  espirito_santo = 38,
  goias = 45,
  maranhao = 41,
  mato_grosso = 46,
  mato_grosso_do_sul = 44,
  minas_gerais = 277,
  para = 56,
  paraiba = 19,
  parana = 174,
  pernambuco = 51,
  piaui = 17,
  rio_de_janeiro = 153,
  rio_grande_do_norte = 26,
  rio_grande_do_sul = 158,
  rondonia = 44,
  roraima = 0,
  santa_catarina = 190,
  sao_paulo = 313,
  sergipe = 15,
  tocantins = 13
)




# abril 2020 --------------------------------------------------------------------------------------------------------------
df_abr_2020

# limpeza e analise de dados
df_abr_2020 <- janitor::clean_names(df_abr_2020)

## total linhas
tot_df_abr_2020 <- dim(df_abr_2020)[1]
tot_df_abr_2020

## filtro para M545
df_abr_2020 <- df_abr_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_abr_2020 <- dim(df_abr_2020)[1]
tot_m545_df_abr_2020

## proporção
tot_m545_df_abr_2020 / tot_df_abr_2020

## especie 
df_abr_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_abr_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_abr_2020 <- df_abr_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_abr_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_abr_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_abr_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_abr_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_abr_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_abr_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'abril',
  mes_num = 4,
  total_geral = 455575,
  total_m545 = 1084,
  prop = 0.002379411,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 2,
  aposent_invalidez_acidente_trabalho = 0,
  aposentadoria_invalidez_previdenciaria = 16,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 69,
  auxilio_doenca_revidenciario = 997,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 10,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 25,
  concessao_decorrente_de_acao_judicial = 115,
  concessao_em_fase_recursal = 2,
  concessao_normal = 932,
  
  
  idade_media = 46.9,
  idade_dp = 11.2,
  
  sexo_feminino = 561,
  sexo_masculino = 523,
  
  # clientela
  urbano = 1003,
  rural = 81,
  
  # vinculo_dependentes
  nao_informado = 983,
  filho = 101,
  
  # forma_filiacao
  autonomo = 159,
  desempregado = 257,
  domestico = 34,
  empregado = 507,
  empresario = 0,
  facultativo = 40,
  optante_pela_lei_618474 = 3,
  segurado_especial = 81,
  trabalhador_avulso = 3,
  
  # uf
  acre = 0,
  alagoas = 24,
  amapa = 1,
  amazonas = 8,
  bahia = 72,
  ceara = 28,
  distrito_federal = 26,
  espirito_santo = 15,
  goias = 28,
  maranhao = 25,
  mato_grosso = 28,
  mato_grosso_do_sul = 24,
  minas_gerais = 133,
  para = 22,
  paraiba = 4,
  parana = 84,
  pernambuco = 55,
  piaui = 22,
  rio_de_janeiro = 57,
  rio_grande_do_norte = 14,
  rio_grande_do_sul = 90,
  rondonia = 22,
  roraima = 0,
  santa_catarina = 119,
  sao_paulo = 175,
  sergipe = 4,
  tocantins = 4
)



# maio 2020 ---------------------------------------------------------------------------------------------------------------
df_mai_2020

# limpeza e analise de dados
df_mai_2020 <- janitor::clean_names(df_mai_2020)

## total linhas
tot_df_mai_2020 <- dim(df_mai_2020)[1]
tot_df_mai_2020

## filtro para M545
df_mai_2020 <- df_mai_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_mai_2020 <- dim(df_mai_2020)[1]
tot_m545_df_mai_2020

## proporção
tot_m545_df_mai_2020 / tot_df_mai_2020

## especie 
df_mai_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_mai_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_mai_2020 <- df_mai_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_mai_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_mai_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_mai_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_mai_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_mai_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_mai_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'maio',
  mes_num = 5,
  total_geral = 341687,
  total_m545 = 482,
  prop = 0.001410648,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 0,
  aposent_invalidez_acidente_trabalho = 1,
  aposentadoria_invalidez_previdenciaria = 22,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 18,
  auxilio_doenca_revidenciario = 441,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 3,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 12,
  concessao_decorrente_de_acao_judicial = 64,
  concessao_em_fase_recursal = 6,
  concessao_normal = 397,
  
  
  idade_media = 47.5,
  idade_dp = 11.0,
  
  sexo_feminino = 241,
  sexo_masculino = 241,
  
  # clientela
  urbano = 444,
  rural = 38,
  
  # vinculo_dependentes
  nao_informado = 464,
  filho = 18,
  
  # forma_filiacao
  autonomo = 73,
  desempregado = 127,
  domestico = 11,
  empregado = 212,
  empresario = 0,
  facultativo = 20,
  optante_pela_lei_618474 = 1,
  segurado_especial = 38,
  trabalhador_avulso = 0,
  
  # uf
  acre = 0,
  alagoas = 11,
  amapa = 1,
  amazonas = 0,
  bahia = 13,
  ceara = 21,
  distrito_federal = 132,
  espirito_santo = 5,
  goias = 4,
  maranhao = 6,
  mato_grosso = 7,
  mato_grosso_do_sul = 4,
  minas_gerais = 59,
  para = 7,
  paraiba = 4,
  parana = 32,
  pernambuco = 13,
  piaui = 2,
  rio_de_janeiro = 14,
  rio_grande_do_norte = 7,
  rio_grande_do_sul = 40,
  rondonia = 6,
  roraima = 0,
  santa_catarina = 29,
  sao_paulo = 56,
  sergipe = 7,
  tocantins = 2
)




# junho 2020 --------------------------------------------------------------------------------------------------------------
df_jun_2020

# limpeza e analise de dados
df_jun_2020 <- janitor::clean_names(df_jun_2020)

## total linhas
tot_df_jun_2020 <- dim(df_jun_2020)[1]
tot_df_jun_2020

## filtro para M545
df_jun_2020 <- df_jun_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_jun_2020 <- dim(df_jun_2020)[1]
tot_m545_df_jun_2020

## proporção
tot_m545_df_jun_2020 / tot_df_jun_2020

## especie 
df_jun_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_jun_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_jun_2020 <- df_jun_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_jun_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_jun_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_jun_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_jun_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_jun_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_jun_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'junho',
  mes_num = 6,
  total_geral = 434025,
  total_m545 = 236,
  prop = 0.0005437475,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 3,
  aposent_invalidez_acidente_trabalho = 0,
  aposentadoria_invalidez_previdenciaria = 9,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 5,
  auxilio_doenca_revidenciario = 219,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 0,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 13,
  concessao_decorrente_de_acao_judicial = 114,
  concessao_em_fase_recursal = 9,
  concessao_normal = 100,
  
  
  idade_media = 51.4,
  idade_dp = 10.7,
  
  sexo_feminino = 109,
  sexo_masculino = 127,
  
  # clientela
  urbano = 198,
  rural = 38,
  
  # vinculo_dependentes
  nao_informado = 231,
  filho = 5,
  
  # forma_filiacao
  autonomo = 44,
  desempregado = 66,
  domestico = 7,
  empregado = 54,
  empresario = 0,
  facultativo = 22,
  optante_pela_lei_618474 = 5,
  segurado_especial = 38,
  trabalhador_avulso = 0,
  
  # uf
  acre = 0,
  alagoas = 12,
  amapa = 1,
  amazonas = 1,
  bahia = 12,
  ceara = 8,
  distrito_federal = 30,
  espirito_santo = 3,
  goias = 6,
  maranhao = 0,
  mato_grosso = 5,
  mato_grosso_do_sul = 2,
  minas_gerais = 23,
  para = 4,
  paraiba = 0,
  parana = 16,
  pernambuco = 9,
  piaui = 10,
  rio_de_janeiro = 7,
  rio_grande_do_norte = 25,
  rio_grande_do_sul = 14,
  rondonia = 1,
  roraima = 0,
  santa_catarina = 16,
  sao_paulo = 17,
  sergipe = 12,
  tocantins = 2
)


# julho 2020 --------------------------------------------------------------------------------------------------------------
df_jul_2020

# limpeza e analise de dados
df_jul_2020 <- janitor::clean_names(df_jul_2020)

## total linhas
tot_df_jul_2020 <- dim(df_jul_2020)[1]
tot_df_jul_2020

## filtro para M545
df_jul_2020 <- df_jul_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_jul_2020 <- dim(df_jul_2020)[1]
tot_m545_df_jul_2020

## proporção
tot_m545_df_jul_2020 / tot_df_jul_2020

## especie 
df_jul_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_jul_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_jul_2020 <- df_jul_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_jul_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_jul_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_jul_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_jul_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_jul_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_jul_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'julho',
  mes_num = 7,
  total_geral = 448516,
  total_m545 = 208,
  prop = 0.0004637516,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 0,
  aposent_invalidez_acidente_trabalho = 1,
  aposentadoria_invalidez_previdenciaria = 15,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 1,
  auxilio_doenca_revidenciario = 191,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 1,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 3,
  concessao_decorrente_de_acao_judicial = 92,
  concessao_em_fase_recursal = 5,
  concessao_normal = 107,
  
  
  idade_media = 50.1,
  idade_dp = 11.0,
  
  sexo_feminino = 100,
  sexo_masculino = 108,
  
  # clientela
  urbano = 160,
  rural = 48,
  
  # vinculo_dependentes
  nao_informado = 201,
  filho = 7,
  
  # forma_filiacao
  autonomo = 37,
  desempregado = 61,
  domestico = 4,
  empregado = 44,
  empresario = 0,
  facultativo = 7,
  optante_pela_lei_618474 = 6,
  segurado_especial = 48,
  trabalhador_avulso = 1,
  
  # uf
  acre = 1,
  alagoas = 8,
  amapa = 4,
  amazonas = 2,
  bahia = 14,
  ceara = 4,
  distrito_federal = 34,
  espirito_santo = 3,
  goias = 2,
  maranhao = 5,
  mato_grosso = 4,
  mato_grosso_do_sul = 1,
  minas_gerais = 24,
  para = 5,
  paraiba = 2,
  parana = 4,
  pernambuco = 4,
  piaui = 10,
  rio_de_janeiro = 3,
  rio_grande_do_norte = 12,
  rio_grande_do_sul = 20,
  rondonia = 2,
  roraima = 0,
  santa_catarina = 22,
  sao_paulo = 13,
  sergipe = 5,
  tocantins = 0
)



# agosto 2020 -------------------------------------------------------------------------------------------------------------
df_ago_2020

# limpeza e analise de dados
df_ago_2020 <- janitor::clean_names(df_ago_2020)

## total linhas
tot_df_ago_2020 <- dim(df_ago_2020)[1]
tot_df_ago_2020

## filtro para M545
df_ago_2020 <- df_ago_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_ago_2020 <- dim(df_ago_2020)[1]
tot_m545_df_ago_2020

## proporção
tot_m545_df_ago_2020 / tot_df_ago_2020

## especie 
df_ago_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_ago_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_ago_2020 <- df_ago_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_ago_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_ago_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_ago_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_ago_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_ago_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_ago_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'agosto',
  mes_num = 8,
  total_geral = 465980,
  total_m545 = 157,
  prop = 0.0003369243,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 0,
  aposent_invalidez_acidente_trabalho = 2,
  aposentadoria_invalidez_previdenciaria = 21,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 11,
  auxilio_doenca_revidenciario = 123,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 1,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 0,
  concessao_decorrente_de_acao_judicial = 57,
  concessao_em_fase_recursal = 4,
  concessao_normal = 95,
  
  
  idade_media = 52.3,
  idade_dp = 10.8,
  
  sexo_feminino = 80,
  sexo_masculino = 77,
  
  # clientela
  urbano = 123,
  rural = 34,
  
  # vinculo_dependentes
  nao_informado = 150,
  filho = 7,
  
  # forma_filiacao
  autonomo = 39,
  desempregado = 34,
  domestico = 8,
  empregado = 29,
  empresario = 0,
  facultativo = 7,
  optante_pela_lei_618474 = 6,
  segurado_especial = 34,
  trabalhador_avulso = 0,
  
  # uf
  acre = 0,
  alagoas = 5,
  amapa = 0,
  amazonas = 2,
  bahia = 14,
  ceara = 4,
  distrito_federal = 6,
  espirito_santo = 0,
  goias = 4,
  maranhao = 0,
  mato_grosso = 0,
  mato_grosso_do_sul = 3,
  minas_gerais = 30,
  para = 4,
  paraiba = 3,
  parana = 7,
  pernambuco = 6,
  piaui = 5,
  rio_de_janeiro = 14,
  rio_grande_do_norte = 4,
  rio_grande_do_sul = 15,
  rondonia = 3,
  roraima = 0,
  santa_catarina = 15,
  sao_paulo = 10,
  sergipe = 3,
  tocantins = 0
)



# setembro 2020 -----------------------------------------------------------------------------------------------------------
df_set_2020

# limpeza e analise de dados
df_set_2020 <- janitor::clean_names(df_set_2020)

## total linhas
tot_df_set_2020 <- dim(df_set_2020)[1]
tot_df_set_2020

## filtro para M545
df_set_2020 <- df_set_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_set_2020 <- dim(df_set_2020)[1]
tot_m545_df_set_2020

## proporção
tot_m545_df_set_2020 / tot_df_set_2020

## especie 
df_set_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_set_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_set_2020 <- df_set_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_set_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_set_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_set_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_set_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_set_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_set_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'setembro',
  mes_num = 9,
  total_geral = 451122,
  total_m545 = 352,
  prop = 0.0007802767,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 0,
  aposent_invalidez_acidente_trabalho = 2,
  aposentadoria_invalidez_previdenciaria = 17,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 43,
  auxilio_doenca_revidenciario = 290,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 4,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 7,
  concessao_decorrente_de_acao_judicial = 71,
  concessao_em_fase_recursal = 5,
  concessao_normal = 265,
  
  
  idade_media = 48.4,
  idade_dp = 10.2,
  
  sexo_feminino = 160,
  sexo_masculino = 192,
  
  # clientela
  urbano = 331,
  rural = 21,
  
  # vinculo_dependentes
  nao_informado = 289,
  filho = 63,
  
  # forma_filiacao
  autonomo = 54,
  desempregado = 65,
  domestico = 16,
  empregado = 177,
  empresario = 0,
  facultativo = 13,
  optante_pela_lei_618474 = 5,
  segurado_especial = 21,
  trabalhador_avulso = 1,
  
  # uf
  acre = 1,
  alagoas = 4,
  amapa = 0,
  amazonas = 5,
  bahia = 13,
  ceara = 12,
  distrito_federal = 11,
  espirito_santo = 12,
  goias = 7,
  maranhao = 2,
  mato_grosso = 7,
  mato_grosso_do_sul = 20,
  minas_gerais = 68,
  para = 4,
  paraiba = 11,
  parana = 11,
  pernambuco = 8,
  piaui = 5,
  rio_de_janeiro = 10,
  rio_grande_do_norte = 14,
  rio_grande_do_sul = 24,
  rondonia = 2,
  roraima = 0,
  santa_catarina = 46,
  sao_paulo = 51,
  sergipe = 4,
  tocantins = 0
)



# outubro 2020 ------------------------------------------------------------------------------------------------------------
df_out_2020

# limpeza e analise de dados
df_out_2020 <- janitor::clean_names(df_out_2020)

## total linhas
tot_df_out_2020 <- dim(df_out_2020)[1]
tot_df_out_2020

## filtro para M545
df_out_2020 <- df_out_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_out_2020 <- dim(df_out_2020)[1]
tot_m545_df_out_2020

## proporção
tot_m545_df_out_2020 / tot_df_out_2020

## especie 
df_out_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_out_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_out_2020 <- df_out_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_out_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_out_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_out_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_out_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_out_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_out_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'outubro',
  mes_num = 10,
  total_geral = 470778,
  total_m545 = 1254,
  prop = 0.002663676,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 4,
  aposent_invalidez_acidente_trabalho = 1,
  aposentadoria_invalidez_previdenciaria = 9,
  auxilio_acidente = 1,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 207,
  auxilio_doenca_revidenciario = 1032,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 30,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 6,
  concessao_decorrente_de_acao_judicial = 34,
  concessao_em_fase_recursal = 4,
  concessao_normal = 1180,
  
  
  idade_media = 46.2,
  idade_dp = 10.5,
  
  sexo_feminino = 557,
  sexo_masculino = 697,
  
  # clientela
  urbano = 1208,
  rural = 46,
  
  # vinculo_dependentes
  nao_informado = 1003,
  filho = 251,
  
  # forma_filiacao
  autonomo = 153,
  desempregado = 179,
  domestico = 57,
  empregado = 793,
  empresario = 0,
  facultativo = 20,
  optante_pela_lei_618474 = 3,
  segurado_especial = 46,
  trabalhador_avulso = 3,
  
  # uf
  acre = 5,
  alagoas = 14,
  amapa = 4,
  amazonas = 29,
  bahia = 48,
  ceara = 43,
  distrito_federal = 46,
  espirito_santo = 28,
  goias = 26,
  maranhao = 11,
  mato_grosso = 19,
  mato_grosso_do_sul = 29,
  minas_gerais = 153,
  para = 21,
  paraiba = 11,
  parana = 53,
  pernambuco = 20,
  piaui = 20,
  rio_de_janeiro = 37,
  rio_grande_do_norte = 19,
  rio_grande_do_sul = 118,
  rondonia = 42,
  roraima = 1,
  santa_catarina = 176,
  sao_paulo = 272,
  sergipe = 8,
  tocantins = 1
)



# novembro 2020 -----------------------------------------------------------------------------------------------------------
df_nov_2020

# limpeza e analise de dados
df_nov_2020 <- janitor::clean_names(df_nov_2020)

## total linhas
tot_df_nov_2020 <- dim(df_nov_2020)[1]
tot_df_nov_2020

## filtro para M545
df_nov_2020 <- df_nov_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_nov_2020 <- dim(df_nov_2020)[1]
tot_m545_df_nov_2020

## proporção
tot_m545_df_nov_2020 / tot_df_nov_2020

## especie 
df_nov_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_nov_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_nov_2020 <- df_nov_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_nov_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_nov_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_nov_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_nov_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_nov_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_nov_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'novembro',
  mes_num = 11,
  total_geral = 423692,
  total_m545 = 1473,
  prop = 0.003476582,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 16,
  aposent_invalidez_acidente_trabalho = 1,
  aposentadoria_invalidez_previdenciaria = 10,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 238,
  auxilio_doenca_revidenciario = 1208,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 35,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 8,
  concessao_decorrente_de_acao_judicial = 65,
  concessao_em_fase_recursal = 1,
  concessao_normal = 1364,
  
  
  idade_media = 46.5,
  idade_dp = 10.8,
  
  sexo_feminino = 645,
  sexo_masculino = 828,
  
  # clientela
  urbano = 1405,
  rural = 68,
  
  # vinculo_dependentes
  nao_informado = 1176,
  filho = 297,
  
  # forma_filiacao
  autonomo = 175,
  desempregado = 230,
  domestico = 60,
  empregado = 900,
  empresario = 0,
  facultativo = 31,
  optante_pela_lei_618474 = 6,
  segurado_especial = 67,
  trabalhador_avulso = 4,
  
  # uf
  acre = 3,
  alagoas = 27,
  amapa = 3,
  amazonas = 32,
  bahia = 57,
  ceara = 43,
  distrito_federal = 46,
  espirito_santo = 35,
  goias = 37,
  maranhao = 10,
  mato_grosso = 26,
  mato_grosso_do_sul = 40,
  minas_gerais = 231,
  para = 27,
  paraiba = 15,
  parana = 100,
  pernambuco = 42,
  piaui = 12,
  rio_de_janeiro = 44,
  rio_grande_do_norte = 15,
  rio_grande_do_sul = 113,
  rondonia = 30,
  roraima = 1,
  santa_catarina = 173,
  sao_paulo = 286,
  sergipe = 23,
  tocantins = 2
)





# dezembro 2020 -----------------------------------------------------------------------------------------------------------

# df_dez_2020 <- read_csv2('http://dadosabertos.dataprev.gov.br/storage/f/2021-01-28T14%3A55%3A42.046Z/beneficios-concedidos-12-2020.csv', 
#           local = locale(encoding = "latin1")) %>% 
#   janitor::clean_names()

df_dez_2020

# limpeza e analise de dados
df_dez_2020 <- janitor::clean_names(df_dez_2020)

## total linhas
tot_df_dez_2020 <- dim(df_dez_2020)[1]
tot_df_dez_2020

## filtro para M545
df_dez_2020 <- df_dez_2020 %>% filter(cid == 'M545')

## total linhas cid m545
tot_m545_df_dez_2020 <- dim(df_dez_2020)[1]
tot_m545_df_dez_2020

## proporção
tot_m545_df_dez_2020 / tot_df_dez_2020

## especie 
df_dez_2020 %>% 
  group_by(esp_a_c_cie) %>% 
  summarise(n())

## despacho
df_dez_2020 %>% 
  group_by(despacho) %>% 
  summarise(n())

## idade média
df_dez_2020 <- df_dez_2020 %>% 
  mutate(
    ano = str_sub(dt_nascimento, start = 7),
    idade = 2021 - as.integer(ano)
  )

df_dez_2020 %>% 
  summarise(
    media_idade = mean(idade),
    dp_idade = sd(idade)
  )

## sexo
df_dez_2020 %>% 
  group_by(sexo) %>% 
  summarise(n())

## clientela
df_dez_2020 %>% 
  group_by(clientela) %>% 
  summarise(n())

## vinculo dependentes
df_dez_2020 %>% 
  group_by(va_nculo_dependentes) %>% 
  summarise(n())

## forma filiacao
df_dez_2020 %>% 
  group_by(forma_filia_a_a_o) %>% 
  summarise(n())

## unidade federetiva
df_dez_2020 %>% 
  group_by(uf) %>% 
  summarise(n()) %>% View()

### adicionar linha
df <- df %>% add_row(
  ano = 2020,
  mes = 'dezembro',
  mes_num = 12,
  total_geral = 321140,
  total_m545 = 1570,
  prop = 0.004888834,
  
  # especie
  amp_aocial_pessoa_portadora_aeficiencia = 24,
  aposent_invalidez_acidente_trabalho = 0,
  aposentadoria_invalidez_previdenciaria = 11,
  auxilio_acidente = 0,
  auxilio_acidente_previdenciario = 0,
  auxilio_doenca_por_acidente_do_trabalho = 255,
  auxilio_doenca_revidenciario = 1280,
  
  # despacho
  conc_base_artigo_27_inciso_ii_do_rbps = 34,
  Conc_com_base_artigo_35_da_lei_821391 = 0,
  conc_decorrente_revisao_administrativa = 11,
  concessao_decorrente_de_acao_judicial = 39,
  concessao_em_fase_recursal = 4,
  concessao_normal = 1482,
  
  
  idade_media = 46.9,
  idade_dp = 10.6,
  
  sexo_feminino = 713,
  sexo_masculino = 857,
  
  # clientela
  urbano = 1484,
  rural = 86,
  
  # vinculo_dependentes
  nao_informado = 1299,
  filho = 271,
  
  # forma_filiacao
  autonomo = 184,
  desempregado = 260,
  domestico = 65,
  empregado = 937,
  empresario = 0,
  facultativo = 33,
  optante_pela_lei_618474 = 2,
  segurado_especial = 85,
  trabalhador_avulso = 4,
  
  # uf
  acre = 0,
  alagoas = 22,
  amapa = 4,
  amazonas = 19,
  bahia = 63,
  ceara = 43,
  distrito_federal = 53,
  espirito_santo = 47,
  goias = 21,
  maranhao = 9,
  mato_grosso = 16,
  mato_grosso_do_sul = 27,
  minas_gerais = 268,
  para = 44,
  paraiba = 30,
  parana = 89,
  pernambuco = 44,
  piaui = 17,
  rio_de_janeiro = 65,
  rio_grande_do_norte = 14,
  rio_grande_do_sul = 108,
  rondonia = 48,
  roraima = 0,
  santa_catarina = 148,
  sao_paulo = 353,
  sergipe = 17,
  tocantins = 1
)




# dados 2019 e 2020 -------------------------------------------------------------------------------------------------------

write.csv(df, 'dados/dados_concedidos_2019-2020_igor.csv')

df <- read_csv('dados/dados_concedidos_2019-2020_igor.csv')


df
