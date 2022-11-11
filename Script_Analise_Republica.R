# Analise de dados de Remuneração de Servidores Públicos por raça e genero entre 1999 e 2020
# Mudando diretorio
setwd('/Users/chris/Desktop/Análise República')
# Importando bibliotecas
library(dplyr)
library(tidyr)
library(janitor)
library(readr)
library(geobr)
library(stringr)
library(ggplot2)


# Inicio da analise
# Importando a base
df = read.csv('5233-liquidosexoraca.csv', sep = ';') %>% clean_names() %>%  # Deixado nome de colunas minusculo
  select(!1) # Retirando a primeira colunas


# Observando os tipos dos dados

df$ano %>% class() # Int

df$sexo_raca %>% class() # Chr

df$liquido %>% class() # Chr

# Trocando a , por ponto para poder transformar o valor de caracter para float

df$liquido = str_replace_all(df$liquido,",", ".")


# Alterando o tipo de dados do valor liquido

df$liquido = as.double(df$liquido)

# Veriicando a classe novamente

df$liquido %>% class()

# Verificando o valor médio de remuneração por Raça e Genero

df$sexo_raca %>% unique()

# Salario Médio Geral entre 1999 e 2020
media_geral = round(mean(df$liquido),2)

print(paste('O valor Médio de Remuneração entre 1999 e 2020 é de R$', media_geral))

# Agrupando valores por Sexo raça
sexo_raca = df %>% group_by(sexo_raca) %>% summarise(media = round(mean(liquido),2))

# Plotando grafico de média Salarial para cada grupo Sexo_raca

ggplot(data = sexo_raca, aes(sexo_raca, media))+
  geom_col(fill = '#9bb78f', col = 'black')+
  geom_text(data = sexo_raca, aes(label = media),
            position = position_stack(vjust = 1.05))+
  labs(title = 'Remuneração líquida média mensal em R$ no Executivo civil federal ativo, por sexo e raça entre os anos de 1999 e 2020 - (Média da Série Histórica)',
       subtitle = 'Fonte - Atlas do Estado Brasileiro',
       caption = 'Produzido por Christian Basilio Oliveira',
       x = 'Sexo e Raça',
       y = 'Média Salarial em (R$)')+
  theme(title = element_text(size = 10))+
  theme_light()


# Grafico de Série temporal ----------------------------------------------------

ggplot(df)+
  geom_line(aes(x = ano, y = liquido, col = sexo_raca), size = .8)+
  geom_point(aes(x = ano, y = liquido, col = sexo_raca))+
  labs(title = 'Remuneração Líquida Mensal em R$ no Executivo Federal Ativo, por Sexo e Raça nos entre os anos 1999 e 2020 em Série Temporal',
       subtitle = 'Fonte - Atlas do Estado Brasileiro',
       caption = 'Produzido por Christian Basilio Oliveira',
       x = 'Anos',
       y = 'Média Salarial em (R$)',
       fill = 'Sexo e Raça',
       col = 'Sexo e Raça')+
  theme_light()+
  expand_limits(y=c(0, 10000), x = c(1999, 2020))+
  theme(legend.position = 'top', legend.direction = 'horizontal', legend.title = element_text(size = 8),
        title = element_text(size = 12))

# Há poucas Transformações salariais durante a serie historica, mas sempre com a manutenção do panorama racial, havendo um maior distanciamento salarial entre os anos de 2007 e 2013, logo após uma diminuição pequena nos outros anos seguintes

# Analisando salárial de Homens e Mulheres
mulheres = df %>% filter(sexo_raca == c('Mulher Branca', 'Mulher Negra'))
homens = df %>% filter(sexo_raca == c('Homem Branco', 'Homem Negro'))

# Media Salarial de Mulheres
media_mulheres = mulheres$liquido %>% mean()
print(paste('O valor Médio de Remuneração de mulheres entre 1999 e 2020 é de R$', round(media_mulheres,2)))

# Média Salarial de Homens
media_homens = homens$liquido %>% mean()
print(paste('O valor Médio de Remuneração de mulheres entre 1999 e 2020 é de R$', round(media_homens,2)))

# Diferença entre os médias salariais de homens e Mulheres no Periodo de 1999 e 2020

dif_salarial_homem_mulher = round((media_homens / media_mulheres - 1)*100,2)

print(paste('No período entre 1999 e 2020 homens receberam', dif_salarial_homem_mulher,'% a Mais que mulheres'))


# Analisando a média salarial anual
#####################________________________________ Ver
media_anos = df %>% group_by(ano) %>% summarise(media_salarial = round(mean(liquido),2))


### _________________________________________________________________________________#


# Grafico de Tendência salarial------------------------------------------------

ggplot(df)+
  geom_smooth(aes(x = ano, y = liquido, col = sexo_raca), size = .8, stat = 'smooth', method = lm)+
  labs(title = 'Remuneração Líquida Mensal em R$ no Executivo Federal Ativo, por Sexo e Raça nos entre os anos 1999 e 2020 em Série Temporal',
       subtitle = 'Fonte - Atlas do Estado Brasileiro',
       caption = 'Produzido por Christian Basilio Oliveira',
       x = 'Anos',
       y = 'Média Salarial em (R$)',
       fill = 'Sexo e Raça',
       col = 'Sexo e Raça')+
  theme(legend.position = 'top', legend.direction = 'horizontal', legend.title = element_text(size = 8),
        title = element_text(size = 12))