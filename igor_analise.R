##########################################################################
#                           Análise Igor Caio                            #
##########################################################################

# Bibliotecas
library(tidyverse)
library(reshape2)

# Carregar dataset
df <- read_csv('dados/dados_concedidos_2019-2020_igor.csv')

# Visualizar dataset
glimpse(df) 
df$X1_1 = NULL

# renomear as paradas pra não confundir
df$total = df$total_geral + df$tot_indef
df$total_concedido = df$total_geral
df$total_indeferido = df$tot_indef
df$total_universo = df$total 
df$total_geral = NULL
df$tot_indef = NULL
df$total = NULL



# ajustes
# 1. Total m54.5 [49.491]
sum(df$total_m545)

# 2. Somatórios dos Benefícios [49.495]
df %>% select(amp_aocial_pessoa_portadora_aeficiencia:auxilio_doenca_revidenciario) %>% sum()

# 3. Somatórios concessões [49.491]
df %>% select(conc_base_artigo_27_inciso_ii_do_rbps:concessao_normal) %>% sum()

# 4. auto - trab. avulso [49.493]
df %>% select(autonomo:trabalhador_avulso) %>% sum()

# 5. Somatório estados [49.626]
df %>% select(acre:tocantins) %>% sum()

# 6. Link no portifólio






# Item 1 --------------------------------
df_1_tab <- tibble('mes' = df$mes,
                   'mes_num' = df$mes_num,
                   'ano' = df$ano,
                   'Total' = df$total_universo,
                   'Concedido' = df$total_concedido,
                   'Indeferido'  = df$total_indeferido)

df_1_tab <- df_1_tab %>% melt(id=c('mes', 'ano', 'mes_num'))




df_1_tab %>% filter(ano == 2019) %>% 
  ggplot(aes(x=factor(mes_num), y=value/1000, fill=variable)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  xlab('Meses') + ylab('Contagem de pedidos (/1000)') +
  ggtitle('Pedidos no ano de 2019') +
  scale_fill_brewer(palette="Accent") +
  theme_classic() +
  guides(fill=guide_legend(title="Tipo de pedido"))

df_1_tab %>% filter(ano == 2020) %>% 
  ggplot(aes(x=factor(mes_num), y=value/1000, fill=variable)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  xlab('Meses') + ylab('Contagem de pedidos (/1000)') +
  ggtitle('Pedidos no ano de 2020') +
  scale_fill_brewer(palette="Accent") +
  theme_classic()+
  guides(fill=guide_legend(title="Tipo de pedido"))


df_1_tab %>% 
  ggplot(aes(x=factor(mes_num), y=value/1000, fill=variable)) + 
  geom_bar(stat='identity', position=position_dodge()) +
  xlab('Meses') + ylab('Contagem de pedidos (/1000)') +
  scale_fill_brewer(palette="Accent") +
  theme_classic() + 
  facet_grid('ano') +
  guides(fill=guide_legend(title="Tipo de pedido"))



# Item 2 ----------------------------------------------------

## Media e dp da idade total cid_m545 -----------------------
df %>% 
  summarise(
    media_idade = mean(idade_media),
    desv_idade = mean(idade_dp)
  )

## Media e dp da idade total cid_m545 por ano ---------------
df %>% 
  group_by(ano) %>% 
  summarise(
    media_idade = mean(idade_media),
    desv_idade = mean(idade_dp)
  )






## Sexo Masculino -------------------------------------------
### Freq e prop sexo masculino total cid_m545 ---------------
df %>% 
  summarise(
    freq_sexo_masc = sum(sexo_masculino),
    prop_sexo_masc = freq_sexo_masc / sum(df$sexo_feminino, df$sexo_masculino)
  )


### Freq e prop sexo masculino total cid_m545 em 2019 --------
df %>% 
  filter(ano == 2019) %>% 
  summarise(
    freq_sexo_masc = sum(sexo_masculino),
    prop_sexo_masc = freq_sexo_masc / sum(df %>% filter(ano == 2019) %>% select(sexo_feminino), 
                                          df %>% filter(ano == 2019) %>% select(sexo_masculino))
  )


### Freq e prop sexo masculino total cid_m545 em 2020 --------
df %>% 
  filter(ano == 2020) %>% 
  summarise(
    freq_sexo_masc = sum(sexo_masculino),
    prop_sexo_masc = freq_sexo_masc / sum(df %>% filter(ano == 2020) %>% select(sexo_feminino), 
                                          df %>% filter(ano == 2020) %>% select(sexo_masculino))
  )


## Sexo Feminino -------------------------------------------
### Freq e prop sexo feminino total cid_m545 ---------------
df %>% 
  summarise(
    freq_sexo_masc = sum(sexo_feminino),
    prop_sexo_masc = freq_sexo_masc / sum(df$sexo_feminino, df$sexo_masculino)
  )


### Freq e prop sexo feminino total cid_m545 em 2019 --------
df %>% 
  filter(ano == 2019) %>% 
  summarise(
    freq_sexo_masc = sum(sexo_feminino),
    prop_sexo_masc = freq_sexo_masc / sum(df %>% filter(ano == 2019) %>% select(sexo_feminino), 
                                          df %>% filter(ano == 2019) %>% select(sexo_masculino))
  )


### Freq e prop sexo feminino total cid_m545 em 2020 --------
df %>% 
  filter(ano == 2020) %>% 
  summarise(
    freq_sexo_masc = sum(sexo_feminino),
    prop_sexo_masc = freq_sexo_masc / sum(df %>% filter(ano == 2020) %>% select(sexo_feminino), 
                                          df %>% filter(ano == 2020) %>% select(sexo_masculino))
  )









## Perimetro urbano -------------------------------------------
### Freq e prop perimetro urbano total cid_m545 ---------------
df %>% 
  summarise(
    freq_urbano = sum(urbano, na.rm=T),
    prop_urbano = freq_urbano / sum(df$urbano, df$rural, na.rm=T)
  )


### Freq e prop perimetro urbano total cid_m545 em 2019---------------
df %>% 
  filter(ano == 2019) %>%
  summarise(
    freq_urbano = sum(urbano, na.rm=T),
    prop_urbano = freq_urbano / sum(df %>% filter(ano == 2019) %>% select(urbano), 
                                    df %>% filter(ano == 2019) %>% select(rural), 
                                    na.rm=T)
  )


### Freq e prop perimetro urbano total cid_m545 em 2020---------------
df %>% 
  filter(ano == 2020) %>%
  summarise(
    freq_urbano = sum(urbano, na.rm=T),
    prop_urbano = freq_urbano / sum(df %>% filter(ano == 2020) %>% select(urbano), 
                                    df %>% filter(ano == 2020) %>% select(rural), 
                                    na.rm=T)
  )







## Perimetro rural -------------------------------------------
### Freq e prop perimetro rural total cid_m545 ---------------
df %>% 
  summarise(
    freq_rural = sum(rural, na.rm=T),
    prop_rural = freq_rural / sum(df$urbano, df$rural, na.rm=T)
  )

### Freq e prop perimetro rural total cid_m545 em 2019---------------
df %>% 
  filter(ano == 2019) %>%
  summarise(
    freq_rural = sum(rural, na.rm=T),
    prop_rural = freq_rural / sum(df %>% filter(ano == 2019) %>% select(urbano), 
                                    df %>% filter(ano == 2019) %>% select(rural), 
                                    na.rm=T)
  )


### Freq e prop perimetro rural total cid_m545 em 2020---------------
df %>% 
  filter(ano == 2020) %>%
  summarise(
    freq_rural = sum(rural, na.rm=T),
    prop_rural = freq_rural / sum(df %>% filter(ano == 2020) %>% select(urbano), 
                                    df %>% filter(ano == 2020) %>% select(rural), 
                                    na.rm=T)
  )







## Vinculo dependentes filho -------------------------------------------
### Freq e prop filho total cid_m545 ---------------
df %>% 
  summarise(
    freq_filho = sum(filho),
    prop_filho = freq_filho / sum(df$filho, df$nao_informado)
  )


### Freq e prop filho total cid_m545 em 2019---------------
df %>% 
  filter(ano == 2019) %>%
  summarise(
    freq_filho = sum(filho),
    prop_filho = freq_filho / sum(df %>% filter(ano == 2019) %>% select(filho), 
                                  df %>% filter(ano == 2019) %>% select(nao_informado))
  )


### Freq e prop perimetro urbano total cid_m545 em 2020---------------
df %>% 
  filter(ano == 2020) %>%
  summarise(
    freq_filho = sum(filho),
    prop_filho = freq_filho / sum(df %>% filter(ano == 2020) %>% select(filho), 
                                  df %>% filter(ano == 2020) %>% select(nao_informado))
  )

## Vinculo dependentes nao_informado -------------------------------------------
### Freq e prop filho total cid_m545 ---------------
df %>% 
  summarise(
    freq_nao_informado = sum(nao_informado),
    prop_nao_informado = freq_nao_informado / sum(df$filho, df$nao_informado)
  )


### Freq e prop filho total cid_m545 em 2019---------------
df %>% 
  filter(ano == 2019) %>%
  summarise(
    freq_nao_informado = sum(nao_informado),
    prop_nao_informado = freq_nao_informado / sum(df %>% filter(ano == 2019) %>% select(filho), 
                                                  df %>% filter(ano == 2019) %>% select(nao_informado))
  )


### Freq e prop perimetro urbano total cid_m545 em 2020---------------
df %>% 
  filter(ano == 2020) %>%
  summarise(
    freq_nao_informado = sum(nao_informado),
    prop_nao_informado = freq_nao_informado / sum(df %>% filter(ano == 2020) %>% select(filho), 
                                                  df %>% filter(ano == 2020) %>% select(nao_informado))
  )



# Item 3 -------------------------------------------------------------
#devtools::install_github("rpradosiqueira/brazilmaps")
library(brazilmaps)

## Total de concedidos por estado

df_estados <- df %>% select(acre:tocantins)

soma_estados <- df_estados %>% summarise(
  "ACRE" = sum(acre),
  "ALAGOAS" = sum(alagoas),
  "AMAPÁ" = sum(amapa),
  "AMAZONAS" = sum(amazonas),
  "BAHIA" = sum(bahia),
  "CEARÁ" = sum(ceara),
  "DISTRITO FEDERAL" = sum(distrito_federal),
  "ESPÍRITO SANTO" = sum(espirito_santo),
  "GOIÁS" = sum(goias),
  "MARANHÃO" = sum(maranhao),
  "MATO GROSSO" = sum(mato_grosso),
  "MATO GROSSO DO SUL" = sum(mato_grosso_do_sul),
  "MINAS GERAIS" = sum(minas_gerais),
  "PARÁ" = sum(para),
  "PARAÍBA" = sum(paraiba),
  "PARANÁ" = sum(parana),
  "PERNAMBUCO" = sum(pernambuco),
  "PIAUÍ" = sum(piaui),
  "RIO DE JANEIRO" = sum(rio_de_janeiro),
  "RIO GRANDE DO NORTE" = sum(rio_grande_do_norte),
  "RIO GRANDE DO SUL" = sum(rio_grande_do_sul),
  "RONDÔNIA" = sum(rondonia),
  "RORAIMA" = sum(roraima),
  "SANTA CATARINA" = sum(santa_catarina),
  "SÃO PAULO" = sum(sao_paulo),
  "SERGIPE" = sum(sergipe),
  "TOCANTINS" = sum(tocantins)
)

soma_estados <- soma_estados %>% melt()
soma_estados <- soma_estados %>% tibble()

write_csv2(x = soma_estados, file = 'tabela_estados_total.csv')



df_mapa_total <- get_brmap("State") %>% 
  inner_join(soma_estados, c("nome" = "variable"))

df_mapa_total %>% 
  ggplot() +
  geom_sf(aes(fill = value)) +
  theme_void() +
  guides(fill=guide_legend(title="Contagem\nTotal"))




## Total de concedidos por estado em 2019
df_estados_19 <- df %>% filter(ano == 2019) %>% select(acre:tocantins)
soma_estados_19 <- df_estados_19 %>% summarise(
  "ACRE" = sum(acre),
  "ALAGOAS" = sum(alagoas),
  "AMAPÁ" = sum(amapa),
  "AMAZONAS" = sum(amazonas),
  "BAHIA" = sum(bahia),
  "CEARÁ" = sum(ceara),
  "DISTRITO FEDERAL" = sum(distrito_federal),
  "ESPÍRITO SANTO" = sum(espirito_santo),
  "GOIÁS" = sum(goias),
  "MARANHÃO" = sum(maranhao),
  "MATO GROSSO" = sum(mato_grosso),
  "MATO GROSSO DO SUL" = sum(mato_grosso_do_sul),
  "MINAS GERAIS" = sum(minas_gerais),
  "PARÁ" = sum(para),
  "PARAÍBA" = sum(paraiba),
  "PARANÁ" = sum(parana),
  "PERNAMBUCO" = sum(pernambuco),
  "PIAUÍ" = sum(piaui),
  "RIO DE JANEIRO" = sum(rio_de_janeiro),
  "RIO GRANDE DO NORTE" = sum(rio_grande_do_norte),
  "RIO GRANDE DO SUL" = sum(rio_grande_do_sul),
  "RONDÔNIA" = sum(rondonia),
  "RORAIMA" = sum(roraima),
  "SANTA CATARINA" = sum(santa_catarina),
  "SÃO PAULO" = sum(sao_paulo),
  "SERGIPE" = sum(sergipe),
  "TOCANTINS" = sum(tocantins)
)

soma_estados_19 <- soma_estados_19 %>% melt()
soma_estados_19 <- soma_estados_19 %>% tibble()


write_csv2(x = soma_estados_19, file = 'tabela_estados_2019.csv')



df_mapa_total_19 <- get_brmap("State") %>% 
  inner_join(soma_estados_19, c("nome" = "variable"))

df_mapa_total_19 %>% 
  ggplot() +
  geom_sf(aes(fill = value)) +
  theme_void() +
  guides(fill=guide_legend(title="Contagem\n2019"))


## Total de concedidos por estado em 2020
df_estados_20 <- df %>% filter(ano == 2020) %>% select(acre:tocantins)
soma_estados_20 <- df_estados_20 %>% summarise(
  "ACRE" = sum(acre),
  "ALAGOAS" = sum(alagoas),
  "AMAPÁ" = sum(amapa),
  "AMAZONAS" = sum(amazonas),
  "BAHIA" = sum(bahia),
  "CEARÁ" = sum(ceara),
  "DISTRITO FEDERAL" = sum(distrito_federal),
  "ESPÍRITO SANTO" = sum(espirito_santo),
  "GOIÁS" = sum(goias),
  "MARANHÃO" = sum(maranhao),
  "MATO GROSSO" = sum(mato_grosso),
  "MATO GROSSO DO SUL" = sum(mato_grosso_do_sul),
  "MINAS GERAIS" = sum(minas_gerais),
  "PARÁ" = sum(para),
  "PARAÍBA" = sum(paraiba),
  "PARANÁ" = sum(parana),
  "PERNAMBUCO" = sum(pernambuco),
  "PIAUÍ" = sum(piaui),
  "RIO DE JANEIRO" = sum(rio_de_janeiro),
  "RIO GRANDE DO NORTE" = sum(rio_grande_do_norte),
  "RIO GRANDE DO SUL" = sum(rio_grande_do_sul),
  "RONDÔNIA" = sum(rondonia),
  "RORAIMA" = sum(roraima),
  "SANTA CATARINA" = sum(santa_catarina),
  "SÃO PAULO" = sum(sao_paulo),
  "SERGIPE" = sum(sergipe),
  "TOCANTINS" = sum(tocantins)
)

soma_estados_20 <- soma_estados_20 %>% melt()
soma_estados_20 <- soma_estados_20 %>% tibble()

write_csv2(x = soma_estados_20, file = 'tabela_estados_2020.csv')


df_mapa_total_20 <- get_brmap("State") %>% 
  inner_join(soma_estados_20, c("nome" = "variable"))

df_mapa_total_20 %>% 
  ggplot() +
  geom_sf(aes(fill = value)) +
  theme_void() +
  guides(fill=guide_legend(title="Contagem\n2020"))



# Item 4 ----------------------------------------------------

## Soma categotias total ------------------------------------
df_categ_total <- df %>% select(autonomo:trabalhador_avulso)
df_categ_total <- df_categ_total %>% summarise(
  "autonomo" = sum(autonomo),
  "desempregado" = sum(desempregado),
  "domestico" = sum(domestico),
  "empregado" = sum(empregado),
  "empresario" = sum(empresario),
  "facultativo" = sum(facultativo),
  "optante_pela_lei_618474" = sum(optante_pela_lei_618474),
  "segurado_especial" = sum(segurado_especial),
  "trabalhador_avulso"  = sum(trabalhador_avulso)
) %>% melt() %>% tibble()

soma_total <- sum(df_categ_total$value)
df_categ_total$prop <- df_categ_total$value / soma_total *100
df_categ_total


## Soma categotias 2019 ------------------------------------
df_categ_2019 <- df %>% filter(ano == 2019) %>% select(autonomo:trabalhador_avulso)
df_categ_2019 <- df_categ_2019 %>% summarise(
  "autonomo" = sum(autonomo),
  "desempregado" = sum(desempregado),
  "domestico" = sum(domestico),
  "empregado" = sum(empregado),
  "empresario" = sum(empresario),
  "facultativo" = sum(facultativo),
  "optante_pela_lei_618474" = sum(optante_pela_lei_618474),
  "segurado_especial" = sum(segurado_especial),
  "trabalhador_avulso"  = sum(trabalhador_avulso)
) %>% melt() %>% tibble()

soma_2019 <- sum(df_categ_2019$value)
df_categ_2019$prop <- df_categ_2019$value / soma_2019 *100
df_categ_2019


## Soma categotias 2020 ------------------------------------
df_categ_2020 <- df %>% filter(ano == 2020) %>% select(autonomo:trabalhador_avulso)
df_categ_2020 <- df_categ_2020 %>% summarise(
  "autonomo" = sum(autonomo),
  "desempregado" = sum(desempregado),
  "domestico" = sum(domestico),
  "empregado" = sum(empregado),
  "empresario" = sum(empresario),
  "facultativo" = sum(facultativo),
  "optante_pela_lei_618474" = sum(optante_pela_lei_618474),
  "segurado_especial" = sum(segurado_especial),
  "trabalhador_avulso"  = sum(trabalhador_avulso)
) %>% melt() %>% tibble()

soma_2020 <- sum(df_categ_2020$value)
df_categ_2020$prop <- df_categ_2020$value / soma_2020 *100
df_categ_2020






# item 5 -------------------------------------------------------------

df_prop <- df %>% select(total_m545, total_concedido, mes_num, ano) %>% melt(c('mes_num', 'ano'))
df_prop

## Propporcao total -------------------------------------------------------
df_prop %>%
  ggplot(aes(x=factor(mes_num), y=value, fill=variable)) + 
  geom_bar(stat='identity', position='fill') +
  xlab('Meses') + ylab('Proporção de pedidos') +
  ggtitle('Pedidos total') +
  theme_classic() +
  scale_fill_discrete(name = "Tipo de pedido", labels = c("CID M54", "Concedido")) 


## Propporcao em 2019 -------------------------------------------------------
df_prop %>% filter(ano == 2019) %>% 
  ggplot(aes(x=factor(mes_num), y=value, fill=variable)) + 
  geom_bar(stat='identity', position='fill') +
  xlab('Meses') + ylab('Proporção de pedidos') +
  ggtitle('Pedidos no ano de 2019') +
  theme_classic() +
  scale_fill_discrete(name = "Tipo de pedido", labels = c("CID M54", "Concedido")) 
 

## Propporcao em 2020 -------------------------------------------------------
df_prop %>% filter(ano == 2020) %>% 
  ggplot(aes(x=factor(mes_num), y=value, fill=variable)) + 
  geom_bar(stat='identity', position='fill') +
  xlab('Meses') + ylab('Proporção de pedidos') +
  ggtitle('Pedidos no ano de 2020') +
  theme_classic() +
  scale_fill_discrete(name = "Tipo de pedido", labels = c("CID M54", "Concedido")) 


## Propporcao em 2019 e 2020 -------------------------------------------------------
df_prop %>%
  ggplot(aes(x=factor(mes_num), y=value, fill=variable)) + 
  geom_bar(stat='identity', position='fill') +
  xlab('Meses') + ylab('Proporção de pedidos') +
  ggtitle('Pedidos total') +
  theme_classic() +
  scale_fill_discrete(name = "Tipo de pedido", labels = c("CID M54", "Concedido")) +
  facet_grid('ano')



# Item 6 --------------------------------------------------------------------------

df <- df %>% left_join(df_outro, by = 'X1')

'M54' # Dorsalgia
'M543' # Ciatica
'M544' # Lumbago com ciatica
'I21' # infarto agudo do miocardio'
'C61' # Neoplasi malignina da protata
'J18' # Pneumonia

df_loko <- df %>% select(X1, total_m545, m54, m544, m543, i21, c61, j18) %>% melt('X1')



df_loko %>%
  ggplot(aes(x=factor(X1), y=value)) + 
  geom_bar(stat='identity') +
  facet_grid('variable')




# item 7 ----------------------------------------------------------------------------

df_benef <- df %>% select(ano = ano.x, amp_aocial_pessoa_portadora_aeficiencia:auxilio_doenca_revidenciario)


df_benef %>% names()


## Benef total -------------------------------------------
### amp_aocial_pessoa_portadora_aeficiencia ---------------
df_benef %>% 
  summarise(
    freq = sum(amp_aocial_pessoa_portadora_aeficiencia),
    prop = freq / sum(df$amp_aocial_pessoa_portadora_aeficiencia, 
                      df$aposent_invalidez_acidente_trabalho,
                      df$aposentadoria_invalidez_previdenciaria,
                      df$auxilio_acidente,
                      df$auxilio_acidente_previdenciario,
                      df$auxilio_doenca_por_acidente_do_trabalho,
                      df$auxilio_doenca_revidenciario) *100
  )

### aposent_invalidez_acidente_trabalho ---------------
df_benef %>% 
  summarise(
    freq = sum(aposent_invalidez_acidente_trabalho),
    prop = freq / sum(df$amp_aocial_pessoa_portadora_aeficiencia, 
                      df$aposent_invalidez_acidente_trabalho,
                      df$aposentadoria_invalidez_previdenciaria,
                      df$auxilio_acidente,
                      df$auxilio_acidente_previdenciario,
                      df$auxilio_doenca_por_acidente_do_trabalho,
                      df$auxilio_doenca_revidenciario) *100
  )

### aposentadoria_invalidez_previdenciaria ---------------
df_benef %>% 
  summarise(
    freq = sum(aposentadoria_invalidez_previdenciaria),
    prop = freq / sum(df$amp_aocial_pessoa_portadora_aeficiencia, 
                      df$aposent_invalidez_acidente_trabalho,
                      df$aposentadoria_invalidez_previdenciaria,
                      df$auxilio_acidente,
                      df$auxilio_acidente_previdenciario,
                      df$auxilio_doenca_por_acidente_do_trabalho,
                      df$auxilio_doenca_revidenciario) *100
  )



### auxilio_acidente ---------------
df_benef %>% 
  summarise(
    freq = sum(auxilio_acidente),
    prop = freq / sum(df$amp_aocial_pessoa_portadora_aeficiencia, 
                      df$aposent_invalidez_acidente_trabalho,
                      df$aposentadoria_invalidez_previdenciaria,
                      df$auxilio_acidente,
                      df$auxilio_acidente_previdenciario,
                      df$auxilio_doenca_por_acidente_do_trabalho,
                      df$auxilio_doenca_revidenciario) *100
  )


### auxilio_acidente_previdenciario ---------------
df_benef %>% 
  summarise(
    freq = sum(auxilio_acidente_previdenciario),
    prop = freq / sum(df$amp_aocial_pessoa_portadora_aeficiencia, 
                      df$aposent_invalidez_acidente_trabalho,
                      df$aposentadoria_invalidez_previdenciaria,
                      df$auxilio_acidente,
                      df$auxilio_acidente_previdenciario,
                      df$auxilio_doenca_por_acidente_do_trabalho,
                      df$auxilio_doenca_revidenciario) *100
  )


### auxilio_doenca_por_acidente_do_trabalho ---------------
df_benef %>% 
  summarise(
    freq = sum(auxilio_doenca_por_acidente_do_trabalho),
    prop = freq / sum(df$amp_aocial_pessoa_portadora_aeficiencia, 
                      df$aposent_invalidez_acidente_trabalho,
                      df$aposentadoria_invalidez_previdenciaria,
                      df$auxilio_acidente,
                      df$auxilio_acidente_previdenciario,
                      df$auxilio_doenca_por_acidente_do_trabalho,
                      df$auxilio_doenca_revidenciario) *100
  )


### auxilio_doenca_revidenciario ---------------
df_benef %>% 
  summarise(
    freq = sum(auxilio_doenca_revidenciario),
    prop = freq / sum(df$amp_aocial_pessoa_portadora_aeficiencia, 
                      df$aposent_invalidez_acidente_trabalho,
                      df$aposentadoria_invalidez_previdenciaria,
                      df$auxilio_acidente,
                      df$auxilio_acidente_previdenciario,
                      df$auxilio_doenca_por_acidente_do_trabalho,
                      df$auxilio_doenca_revidenciario) *100
  )








## Benef 2019-------------------------------------------
### amp_aocial_pessoa_portadora_aeficiencia ---------------
df_benef %>% 
  filter(ano == 2019) %>% 
  summarise(
    freq = sum(amp_aocial_pessoa_portadora_aeficiencia),
    prop = freq / sum(df_benef %>% filter(ano == 2019) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2019) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )

### aposent_invalidez_acidente_trabalho ---------------
df_benef %>% 
  filter(ano == 2019) %>% 
  summarise(
    freq = sum(aposent_invalidez_acidente_trabalho),
    prop = freq / sum(df_benef %>% filter(ano == 2019) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2019) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )

### aposentadoria_invalidez_previdenciaria ---------------
df_benef %>% 
  filter(ano == 2019) %>% 
  summarise(
    freq = sum(aposentadoria_invalidez_previdenciaria),
    prop = freq / sum(df_benef %>% filter(ano == 2019) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2019) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )



### auxilio_acidente ---------------
df_benef %>% 
  filter(ano == 2019) %>% 
  summarise(
    freq = sum(auxilio_acidente),
    prop = freq / sum(df_benef %>% filter(ano == 2019) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2019) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )


### auxilio_acidente_previdenciario ---------------
df_benef %>% 
  filter(ano == 2019) %>% 
  summarise(
    freq = sum(auxilio_acidente_previdenciario),
    prop = freq / sum(df_benef %>% filter(ano == 2019) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2019) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )


### auxilio_doenca_por_acidente_do_trabalho ---------------
df_benef %>% 
  filter(ano == 2019) %>% 
  summarise(
    freq = sum(auxilio_doenca_por_acidente_do_trabalho),
    prop = freq / sum(df_benef %>% filter(ano == 2019) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2019) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )


### auxilio_doenca_revidenciario ---------------
df_benef %>% 
  filter(ano == 2019) %>% 
  summarise(
    freq = sum(auxilio_doenca_revidenciario),
    prop = freq / sum(df_benef %>% filter(ano == 2019) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2019) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2019) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )








## Benef 2020-------------------------------------------
### amp_aocial_pessoa_portadora_aeficiencia ---------------
df_benef %>% 
  filter(ano == 2020) %>% 
  summarise(
    freq = sum(amp_aocial_pessoa_portadora_aeficiencia),
    prop = freq / sum(df_benef %>% filter(ano == 2020) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2020) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )

### aposent_invalidez_acidente_trabalho ---------------
df_benef %>% 
  filter(ano == 2020) %>% 
  summarise(
    freq = sum(aposent_invalidez_acidente_trabalho),
    prop = freq / sum(df_benef %>% filter(ano == 2020) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2020) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )

### aposentadoria_invalidez_previdenciaria ---------------
df_benef %>% 
  filter(ano == 2020) %>% 
  summarise(
    freq = sum(aposentadoria_invalidez_previdenciaria),
    prop = freq / sum(df_benef %>% filter(ano == 2020) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2020) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )



### auxilio_acidente ---------------
df_benef %>% 
  filter(ano == 2020) %>% 
  summarise(
    freq = sum(auxilio_acidente),
    prop = freq / sum(df_benef %>% filter(ano == 2020) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2020) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )


### auxilio_acidente_previdenciario ---------------
df_benef %>% 
  filter(ano == 2020) %>% 
  summarise(
    freq = sum(auxilio_acidente_previdenciario),
    prop = freq / sum(df_benef %>% filter(ano == 2020) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2020) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )


### auxilio_doenca_por_acidente_do_trabalho ---------------
df_benef %>% 
  filter(ano == 2020) %>% 
  summarise(
    freq = sum(auxilio_doenca_por_acidente_do_trabalho),
    prop = freq / sum(df_benef %>% filter(ano == 2020) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2020) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )


### auxilio_doenca_revidenciario ---------------
df_benef %>% 
  filter(ano == 2020) %>% 
  summarise(
    freq = sum(auxilio_doenca_revidenciario),
    prop = freq / sum(df_benef %>% filter(ano == 2020) %>% select(amp_aocial_pessoa_portadora_aeficiencia), 
                      df_benef %>% filter(ano == 2020) %>% select(aposent_invalidez_acidente_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(aposentadoria_invalidez_previdenciaria),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_acidente_previdenciario),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_por_acidente_do_trabalho),
                      df_benef %>% filter(ano == 2020) %>% select(auxilio_doenca_revidenciario)
    ) *100
  )



















