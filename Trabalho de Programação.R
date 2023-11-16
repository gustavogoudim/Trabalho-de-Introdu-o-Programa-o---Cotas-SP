# SCRIPT FINAL #

#GUSTAVO GOUDIM AREDES PIMENTEL#
#ACADÊMICO DE RELAÇÕES INTERNACIONAIS#
#TRABALHO DE INTRODUÇÃO À PROGRAMAÇÃO#


install.packages("plotly")
install.packages("htmlwidgets")
install.packages("RColorBrewer")
install.packages("gridExtra")
install.packages("ggthemes")
install.packages("hrbrthemes")



library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer) #paleta de cores 
library(plotly) #gráficos interativos
library(htmlwidgets) #salvar gráficos interativos
library(gridExtra) #gráficos múltiplos
library(readxl)
library(ggthemes) #temas
library(hrbrthemes) #temas 
library(zip)




if(!file.exists("./data")){dir.create("./data")} #criar uma nova pasta para o diretório do trabalho
setwd("./data")


#####DISTRIBUIÇÃO DE COTAS#####

# data frame que representa a hierarquia das categorias
cotas <- data.frame(
  Categoria = c(
    "Lei de Cotas (100)","Ampla Concorrência (50)", "Escola Pública (50)", "Baixa Renda (25)",
    "PPI (13) ", "Deficientes (2) ", "Demais vagas (10) ",
    "Independente da Renda (25)",
    "PPI (13)", "Deficientes (2)", "Demais vagas (10)"
  ),
  Parent = c(
    "","Lei de Cotas (100)", "Lei de Cotas (100)", "Escola Pública (50)",
    "Baixa Renda (25)", "Baixa Renda (25)", "Baixa Renda (25)",
    "Escola Pública (50)",
    "Independente da Renda (25)", "Independente da Renda (25)", "Independente da Renda (25)"
  ), Valor=c(0,50,0,0,13,2,10,0,13,2,10) #representação das vagas por cotas, segundo o MEC
)

# Crie um treemap hierárquico aninhado
plot.cotas <- plot_ly(cotas, type = "treemap", ids = ~Categoria, labels = ~Categoria, parents = ~Parent, values = ~Valor)


# Personalize o treemap
plot.cotas <- plot.cotas %>% layout(margin = list(l = 0, r = 0, b = 0, t = 0))


# Definição de cores e plotando o gráfico 
palette <- c("white", "#FEB24C", 
             "#9ECAE1", 
             "#4292C6", 
             "#2171B5", "#08306B","#08519C",
             "#4292C6", 
             "#2171B5","#08306B","#08519C")


plot.cotas <- plot.cotas %>% add_trace(
  marker = list(colors = palette),
  colors = ~Valor,
  name = "",
  hoverinfo = "none")


plot.cotas

# Salvar

saveWidget(plot.cotas, file="Distribuição_Lei_de_Cotas.html")

#####2010#####

# Baixando, descompactando e abrindo a base de dados 

download.file(url = "https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2010.zip",
              destfile = paste(basename("https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2010.zip"),
                               sep="/"),
              mode = "wb")

unzip(zipfile = "./microdados_censo_da_educacao_superior_2010.zip")

cursos2010 <- read.csv2("./Microdados do Censo da Educa��o Superior 2010/dados/MICRODADOS_CADASTRO_CURSOS_2010.CSV",
                        sep = ";", fileEncoding = "latin1")

#Selecionando os dados desejados 

cursos2010 <- cursos2010 %>%
  select(1,3,5,6,7,8,12,13,
         14,27,28,46,
         47,48,70,71,72,73,74,75)

colnames(cursos2010) <- c("Ano", "Região", "UF", "IBGE UF","Município", "IBGE Município", "Categoria adminsitrativa", "Rede de ensino",
                          "Cod Instituição","Presencial ou Distância", "Nível acad do curso", "Qtd Ingressantes",
                          "Femininas", "Masculinas", "Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada") #essa última linha representa a quantidade de ingressantes das respectivas raças e sexos


#Graduações em institutos, universidades e faculdades federais paulistas

d1.2010 <- cursos2010[cursos2010$UF == 'SP' & #em SP
                        cursos2010$`Rede de ensino` == 1 & #rede pública
                        cursos2010$`Nível acad do curso` == 1 & #graduação
                        cursos2010$`Categoria adminsitrativa` == 1,] #federal
rownames(d1.2010) <- NULL

#Gráfico de representações por raça

raça2010 <- c("Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada")

porcentagem_raça2010 <- c((sum(d1.2010$Branco)/sum(d1.2010$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2010$Preta)/sum(d1.2010$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2010$Parda)/sum(d1.2010$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2010$Amarela)/sum(d1.2010$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2010$Indígena)/sum(d1.2010$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2010$`Não declarada`)/sum(d1.2010$`Qtd Ingressantes`)) * 100)

#data frame com as variaveis desejadas 
df.raça2010 <- data.frame(raça2010,porcentagem_raça2010)


ordem_desejada <- c("Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada")

# Crie uma nova variável de fator com a ordem desejada
df.raça2010$raça2010 <- factor(df.raça2010$raça2010, levels = ordem_desejada)


minha_paleta <- brewer.pal(n = 6, name = "Set3")

#plotar o gráfico de barras

grafico_2010 <- ggplot(df.raça2010, aes(x = raça2010, y = porcentagem_raça2010, fill = raça2010)) +
  geom_bar(stat = "identity") +
  theme_void()+
  labs(x = "Raça",
       y = "Porcentagem",
       title = "REPRESENTAÇÃO DOS INGRESSANTES DE GRADUAÇÃO NAS IES FEDERAIS PAULISTAS POR RAÇA",
       subtitle = "No ano de 2010",
       caption = "Fonte: INEP, Microdados do Censo da Educação Superior 2010") +
  theme (axis.text.x = element_text( hjust = 0.5),
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.margin = margin(0, 0, 0, 0), #posição da fonte 
         legend.position = "none") + #remover legenda
  scale_fill_manual(values = minha_paleta) + #definindo a paleta de cores
  geom_text(aes(label = paste0(round(porcentagem_raça2010, 2), "%"), vjust = -0.5), size = 5, col = "black") #adicionando os valores % 

grafico_2010

#salvando o gráfico
ggsave("2010.jpg", dpi = 300, width = 15, height = 6, units = "in")


#####2012#####

# Baixando, descompactando e abrindo a base de dados 

download.file(url = "https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2012.zip",
              destfile = paste(basename("https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2012.zip"),
                               sep="/"),
              mode = "wb")

unzip(zipfile = "./microdados_censo_da_educacao_superior_2012.zip")

cursos2012 <- read.csv2("./Microdados do Censo da Educa��o Superior 2012/dados/MICRODADOS_CADASTRO_CURSOS_2012.CSV",
                        sep = ";", fileEncoding = "latin1")

#Selecionando os dados desejados 

cursos2012 <- cursos2012 %>%
  select(1,3,5,6,7,8,12,13,
         14,27,28,46,
         47,48,70,71,72,73,74,75)

colnames(cursos2012) <- c("Ano", "Região", "UF", "IBGE UF","Município", "IBGE Município", "Categoria adminsitrativa", "Rede de ensino",
                          "Cod Instituição","Presencial ou Distância", "Nível acad do curso", "Qtd Ingressantes",
                          "Femininas", "Masculinas", "Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada") #essa última linha representa a quantidade de ingressantes das respectivas raças e sexos


#Graduações em institutos, universidades e faculdades federais paulistas

d1.2012 <- cursos2012[cursos2012$UF == 'SP' & #em SP
                        cursos2012$`Rede de ensino` == 1 & #rede pública
                        cursos2012$`Nível acad do curso` == 1 & #graduação
                        cursos2012$`Categoria adminsitrativa` == 1,] #federal
rownames(d1.2012) <- NULL

#Gráfico de representações por raça

raça2012 <- c("Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada")

porcentagem_raça2012 <- c((sum(d1.2012$Branco)/sum(d1.2012$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2012$Preta)/sum(d1.2012$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2012$Parda)/sum(d1.2012$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2012$Amarela)/sum(d1.2012$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2012$Indígena)/sum(d1.2012$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2012$`Não declarada`)/sum(d1.2012$`Qtd Ingressantes`)) * 100)

#data frame com as variaveis desejadas 
df.raça2012 <- data.frame(raça2012,porcentagem_raça2012)


# Crie uma nova variável de fator com a ordem desejada
df.raça2012$raça2012 <- factor(df.raça2012$raça2012, levels = ordem_desejada)



#plotar o gráfico de barras

grafico_2012 <- ggplot(df.raça2012, aes(x = raça2012, y = porcentagem_raça2012, fill = raça2012)) +
  geom_bar(stat = "identity") +
  theme_void()+
  labs(x = "Raça",
       y = "Porcentagem",
       title = "REPRESENTAÇÃO DOS INGRESSANTES DE GRADUAÇÃO NAS IES FEDERAIS PAULISTAS POR RAÇA",
       subtitle = "No ano de 2012",
       caption = "Fonte: INEP, Microdados do Censo da Educação Superior 2012") +
  theme (axis.text.x = element_text( hjust = 0.5),
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.margin = margin(0, 0, 0, 0), #posição da fonte 
         legend.position = "none") + #remover legenda
  scale_fill_manual(values = minha_paleta) + #definindo a paleta de cores
  geom_text(aes(label = paste0(round(porcentagem_raça2012, 2), "%"), vjust = -0.5), size = 5, col = "black") #adicionando os valores % 

grafico_2012

#salvando o gráfico
ggsave("2012.jpg", dpi = 300, width = 15, height = 6, units = "in")


#####2014#####


# Baixando, descompactando e abrindo a base de dados 

download.file(url = "https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2014.zip",
              destfile = paste(basename("https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2014.zip"),
                               sep="/"),
              mode = "wb")

unzip(zipfile = "./microdados_censo_da_educacao_superior_2014.zip")

cursos2014 <- read.csv2("./Microdados do Censo da Educa��o Superior 2014/dados/MICRODADOS_CADASTRO_CURSOS_2014.CSV",
                        sep = ";", fileEncoding = "latin1")

#Selecionando os dados desejados 
cursos2014 <- cursos2014 %>%
  select(1,3,5,6,7,8,12,13,
         14,27,28,46,
         47,48,70,71,72,73,74,75)

colnames(cursos2014) <- c("Ano", "Região", "UF", "IBGE UF","Município", "IBGE Município", "Categoria adminsitrativa", "Rede de ensino",
                          "Cod Instituição","Presencial ou Distância", "Nível acad do curso", "Qtd Ingressantes",
                          "Femininas", "Masculinas", "Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada") #essa última linha representa a quantidade de ingressantes das respectivas raças e sexos


#Graduações em institutos, universidades e faculdades federais paulistas

d1.2014 <- cursos2014[cursos2014$UF == 'SP' & #em SP
                        cursos2014$`Rede de ensino` == 1 & #rede pública
                        cursos2014$`Nível acad do curso` == 1 & #graduação
                        cursos2014$`Categoria adminsitrativa` == 1,] #federal
rownames(d1.2014) <- NULL

#Gráfico de representações por raça

raça2014 <- c("Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada")

porcentagem_raça2014 <- c((sum(d1.2014$Branco)/sum(d1.2014$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2014$Preta)/sum(d1.2014$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2014$Parda)/sum(d1.2014$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2014$Amarela)/sum(d1.2014$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2014$Indígena)/sum(d1.2014$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2014$`Não declarada`)/sum(d1.2014$`Qtd Ingressantes`)) * 100)

#data frame com as variaveis desejadas 
df.raça2014 <- data.frame(raça2014,porcentagem_raça2014)


# Crie uma nova variável de fator com a ordem desejada
df.raça2014$raça2014 <- factor(df.raça2014$raça2014, levels = ordem_desejada)




#plotar o gráfico de barras

grafico_2014 <- ggplot(df.raça2014, aes(x = raça2014, y = porcentagem_raça2014, fill = raça2014)) +
  geom_bar(stat = "identity") +
  theme_void()+
  labs(x = "Raça",
       y = "Porcentagem",
       title = "REPRESENTAÇÃO DOS INGRESSANTES DE GRADUAÇÃO NAS IES FEDERAIS PAULISTAS POR RAÇA",
       subtitle = "No ano de 2014",
       caption = "Fonte: INEP, Microdados do Censo da Educação Superior 2014") +
  theme (axis.text.x = element_text( hjust = 0.5),
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.margin = margin(0, 0, 0, 0), #posição da fonte 
         legend.position = "none") + #remover legenda
  scale_fill_manual(values = minha_paleta) + #definindo a paleta de cores
  geom_text(aes(label = paste0(round(porcentagem_raça2014, 2), "%"), vjust = -0.5), size = 5, col = "black") #adicionando os valores % 

grafico_2014

#salvando o gráfico
ggsave("2014.jpg", dpi = 300, width = 15, height = 6, units = "in")


#####2016#####

# Baixando, descompactando e abrindo a base de dados 

download.file(url = "https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2016.zip",
              destfile = paste(basename("https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2016.zip"),
                               sep="/"),
              mode = "wb")

unzip(zipfile = "./microdados_censo_da_educacao_superior_2016.zip")

cursos2016 <- read.csv2("./Microdados do Censo da Educa��o Superior 2016/dados/MICRODADOS_CADASTRO_CURSOS_2016.CSV",
                        sep = ";", fileEncoding = "latin1")

#Selecionando os dados desejados 

cursos2016 <- cursos2016 %>%
  select(1,3,5,6,7,8,12,13,
         14,27,28,46,
         47,48,70,71,72,73,74,75)

colnames(cursos2016) <- c("Ano", "Região", "UF", "IBGE UF","Município", "IBGE Município", "Categoria adminsitrativa", "Rede de ensino",
                          "Cod Instituição","Presencial ou Distância", "Nível acad do curso", "Qtd Ingressantes",
                          "Femininas", "Masculinas", "Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada") #essa última linha representa a quantidade de ingressantes das respectivas raças e sexos


#Graduações em institutos, universidades e faculdades federais paulistas

d1.2016 <- cursos2016[cursos2016$UF == 'SP' & #em SP
                        cursos2016$`Rede de ensino` == 1 & #rede pública
                        cursos2016$`Nível acad do curso` == 1 & #graduação
                        cursos2016$`Categoria adminsitrativa` == 1,] #federal
rownames(d1.2016) <- NULL

#Gráfico de representações por raça

raça2016 <- c("Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada")

porcentagem_raça2016 <- c((sum(d1.2016$Branco)/sum(d1.2016$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2016$Preta)/sum(d1.2016$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2016$Parda)/sum(d1.2016$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2016$Amarela)/sum(d1.2016$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2016$Indígena)/sum(d1.2016$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2016$`Não declarada`)/sum(d1.2016$`Qtd Ingressantes`)) * 100)

#data frame com as variaveis desejadas 
df.raça2016 <- data.frame(raça2016,porcentagem_raça2016)


# Crie uma nova variável de fator com a ordem desejada
df.raça2016$raça2016 <- factor(df.raça2016$raça2016, levels = ordem_desejada)



#plotar o gráfico de barras

grafico_2016 <- ggplot(df.raça2016, aes(x = raça2016, y = porcentagem_raça2016, fill = raça2016)) +
  geom_bar(stat = "identity") +
  theme_void()+
  labs(x = "Raça",
       y = "Porcentagem",
       title = "REPRESENTAÇÃO DOS INGRESSANTES DE GRADUAÇÃO NAS IES FEDERAIS PAULISTAS POR RAÇA",
       subtitle = "No ano de 2016",
       caption = "Fonte: INEP, Microdados do Censo da Educação Superior 2016") +
  theme (axis.text.x = element_text( hjust = 0.5),
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.margin = margin(0, 0, 0, 0), #posição da fonte 
         legend.position = "none") + #remover legenda
  scale_fill_manual(values = minha_paleta) + #definindo a paleta de cores
  geom_text(aes(label = paste0(round(porcentagem_raça2016, 2), "%"), vjust = -0.5), size = 5, col = "black") #adicionando os valores % 

grafico_2016

#salvando o gráfico
ggsave("2016.jpg", dpi = 300, width = 15, height = 6, units = "in")


#####2018#####

# Baixando, descompactando e abrindo a base de dados 

download.file(url = "https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2018.zip",
              destfile = paste(basename("https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_2018.zip"),
                               sep="/"),
              mode = "wb")

unzip(zipfile = "./microdados_censo_da_educacao_superior_2018.zip")

cursos2018 <- read.csv2("./Microdados do Censo da Educa��o Superior 2018/dados/MICRODADOS_CADASTRO_CURSOS_2018.CSV",
                        sep = ";", fileEncoding = "latin1")

#Selecionando os dados desejados 


cursos2018 <- cursos2018 %>%
  select(1,3,5,6,7,8,12,13,
         14,27,28,46,
         47,48,70,71,72,73,74,75)

colnames(cursos2018) <- c("Ano", "Região", "UF", "IBGE UF","Município", "IBGE Município", "Categoria adminsitrativa", "Rede de ensino",
                          "Cod Instituição","Presencial ou Distância", "Nível acad do curso", "Qtd Ingressantes",
                          "Femininas", "Masculinas", "Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada") #essa última linha representa a quantidade de ingressantes das respectivas raças e sexos


#Graduações em institutos, universidades e faculdades federais paulistas

d1.2018 <- cursos2018[cursos2018$UF == 'SP' & #em SP
                        cursos2018$`Rede de ensino` == 1 & #rede pública
                        cursos2018$`Nível acad do curso` == 1 & #graduação
                        cursos2018$`Categoria adminsitrativa` == 1,] #federal
rownames(d1.2018) <- NULL

#Gráfico de representações por raça

raça2018 <- c("Branco", "Preta", "Parda", "Amarela", "Indígena", "Não declarada")

porcentagem_raça2018 <- c((sum(d1.2018$Branco)/sum(d1.2018$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2018$Preta)/sum(d1.2018$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2018$Parda)/sum(d1.2018$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2018$Amarela)/sum(d1.2018$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2018$Indígena)/sum(d1.2018$`Qtd Ingressantes`)) * 100,
                          (sum(d1.2018$`Não declarada`)/sum(d1.2018$`Qtd Ingressantes`)) * 100)

#data frame com as variaveis desejadas 
df.raça2018 <- data.frame(raça2018,porcentagem_raça2018)


# Crie uma nova variável de fator com a ordem desejada
df.raça2018$raça2018 <- factor(df.raça2018$raça2018, levels = ordem_desejada)


#plotar o gráfico de barras

grafico_2018 <- ggplot(df.raça2018, aes(x = raça2018, y = porcentagem_raça2018, fill = raça2018)) +
  geom_bar(stat = "identity") +
  theme_void()+
  labs(x = "Raça",
       y = "Porcentagem",
       title = "REPRESENTAÇÃO DOS INGRESSANTES DE GRADUAÇÃO NAS IES FEDERAIS PAULISTAS POR RAÇA",
       subtitle = "No ano de 2018",
       caption = "Fonte: INEP, Microdados do Censo da Educação Superior 2018") +
  theme (axis.text.x = element_text( hjust = 0.5),
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.margin = margin(0, 0, 0, 0), #posição da fonte 
         legend.position = "none") + #remover legenda
  scale_fill_manual(values = minha_paleta) + #definindo a paleta de cores
  geom_text(aes(label = paste0(round(porcentagem_raça2018, 2), "%"), vjust = -0.5), size = 5, col = "black") #adicionando os valores %

grafico_2018

#salvando o gráfico

ggsave("2018.jpg", dpi = 300, width = 15, height = 6, units = "in")




#####variação entre os anos- gráfico de barras composto#####

#alterando os gráficos para ficar mais legível um ao lado do outro. 

grafico_2010 <- 
  grafico_2010 + 
  labs(x = "Raça",
       y = "Porcentagem",
       title = "VARIAÇÃO DA REPRESENTAÇÃO DOS INGRESSANTES DE GRADUAÇÃO NAS IES FEDERAIS PAULISTAS POR RAÇA (2010-2018)",
       subtitle = "2010",
       caption = "") +
  theme(plot.title = element_text(hjust = -4.5))

grafico_2012 <- 
  grafico_2012 + 
  labs(x = "Raça",
       y = "Porcentagem",
       title = "",
       subtitle = "2012",
       caption = "")


grafico_2014 <- 
  grafico_2014 + 
  labs(x = "Raça",
       y = "Porcentagem",
       title = "",
       subtitle = "2014",
       caption = "")

grafico_2016 <- 
  grafico_2016 + 
  labs(x = "Raça",
       y = "Porcentagem",
       title = "",
       subtitle = "2016",
       caption = "")


grafico_2018 <- 
  grafico_2018 + 
  labs(x = "Raça",
       y = "Porcentagem",
       title = "",
       subtitle = "2018")

# plotando
grafico_composto <- grid.arrange(grafico_2010, grafico_2012, grafico_2014, grafico_2016, grafico_2018, ncol = 5)

#Colocar um separador entre os gráficos anuais

#Personalizar o tema para adicionar a separação
separador_tema <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.margin = margin(0, 0, 0, 0)
)

#Aplicar o tema aos gráficos
grafico_2010 <- grafico_2010 + separador_tema
grafico_2012 <- grafico_2012 + separador_tema
grafico_2014 <- grafico_2014 + separador_tema
grafico_2016 <- grafico_2016 + separador_tema
grafico_2018 <- grafico_2018 + separador_tema

#Organizar os gráficos
grafico_composto <- grid.arrange(
  grafico_2010, grafico_2012, grafico_2014, grafico_2016, grafico_2018,
  ncol = 5
)


ggsave("Gráfico Composto.jpg", grafico_composto, width = 40, height = 7, units = "in")


#####Distribuição de raça no estado de SP#####

# Baixando, descompactando e abrindo a base de dados 

download.file(url = "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/xls/Unidades_da_Federacao/sao_paulo.zip",
              destfile = paste(basename("https://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/xls/Unidades_da_Federacao/sao_paulo.zip"),
                               sep="/"),
              mode = "wb")

unzip(zipfile = "./sao_paulo.zip")

populacao_saopaulo_2010<- read_excel("./Tabela 3.20.3.1.xls",
                                     range = "A6:H7")


colnames(populacao_saopaulo_2010) <- c("","Total","Branco", "Preta", "Amarela","Parda", "Indígena", "Não declarada")

populacao_saopaulo_2010 <- populacao_saopaulo_2010[,-1]

rownames(populacao_saopaulo_2010) <- NULL

#criando data frame para gerar o gráfico 

raca_sp_2010 = c("Branco", "Preta", "Parda","Amarela", "Indígena", "Não declarada")

percentual_raca_sp_2010 <- c((populacao_saopaulo_2010$Branco/populacao_saopaulo_2010$Total) * 100,
                             (populacao_saopaulo_2010$Preta/populacao_saopaulo_2010$Total) * 100,
                             (populacao_saopaulo_2010$Parda/populacao_saopaulo_2010$Total) * 100,
                             (populacao_saopaulo_2010$Amarela/populacao_saopaulo_2010$Total) * 100,
                             (populacao_saopaulo_2010$Indígena/populacao_saopaulo_2010$Total) * 100,
                             (populacao_saopaulo_2010$`Não declarada`/populacao_saopaulo_2010$Total) * 100)

df.raça.sp.2010 <- data.frame(raca_sp_2010,percentual_raca_sp_2010)


df.raça.sp.2010$raca_sp_2010 <- factor(df.raça.sp.2010$raca_sp_2010, levels = ordem_desejada)


# Gráfico de pirulito

ggplot(df.raça.sp.2010, aes(x= raca_sp_2010, y= percentual_raca_sp_2010))+
  geom_segment(aes(x=raca_sp_2010, xend=raca_sp_2010, y = 0, yend= percentual_raca_sp_2010),
               linewidth=2, color=minha_paleta)+
  geom_point(size=3.5, color=minha_paleta)+
  geom_text(aes(label=c("63,9%","5,5%","29,1%","1,3%", "0,1%", "< 0,01%")),
            hjust = -0.3, size = 4.5, color=minha_paleta)+
  coord_flip()+
  theme_gdocs()+
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color=minha_paleta),
    plot.title = element_text(color="white",hjust = 0.5),
    plot.subtitle = element_text(color="white",hjust = 0.5),
    plot.caption = element_text(color="white",hjust = 1),
    plot.background = element_rect(fill = "gray18"))+
  xlab("")+ 
  ylab("%")+
  labs(title = "REPRESENTAÇÃO RACIAL DA POPULAÇÃO DO ESTADO DE SÃO PAULO",
       subtitle= "EM 2010",
       caption = "Fonte: Censo de 2010, IBGE")


ggsave("Representação racial em SP.jpg", dpi = 300, width = 16, height = 6, units = "in")




#####Número de ingressantes cotistas ao longo dos anos#####

# datas frames das quantidades de ingressantes 

# 2010
ingressantes2010 <- c(sum(d1.2010$Branco),
                      sum(d1.2010$Preta),
                      sum(d1.2010$Parda),
                      sum(d1.2010$Amarela),
                      sum(d1.2010$Indígena),
                      sum(d1.2010$`Não declarada`))

#selecionando as variaveis desejadas 
df.ingressantes2010 <- data.frame(raça2010,ingressantes2010)


# 2012
ingressantes2012 <- c(sum(d1.2012$Branco),
                      sum(d1.2012$Preta),
                      sum(d1.2012$Parda),
                      sum(d1.2012$Amarela),
                      sum(d1.2012$Indígena),
                      sum(d1.2012$`Não declarada`))

#data frame com as variaveis desejadas 
df.ingressantes2012 <- data.frame(raça2012,ingressantes2012)


# 2014
ingressantes2014 <- c(sum(d1.2014$Branco),
                      sum(d1.2014$Preta),
                      sum(d1.2014$Parda),
                      sum(d1.2014$Amarela),
                      sum(d1.2014$Indígena),
                      sum(d1.2014$`Não declarada`))

#data frame com as variaveis desejadas 
df.ingressantes2014 <- data.frame(raça2014,ingressantes2014)


# 2016
ingressantes2016 <- c(sum(d1.2016$Branco),
                      sum(d1.2016$Preta),
                      sum(d1.2016$Parda),
                      sum(d1.2016$Amarela),
                      sum(d1.2016$Indígena),
                      sum(d1.2016$`Não declarada`))

#data frame com as variaveis desejadas 
df.ingressantes2016 <- data.frame(raça2016,ingressantes2016)


# 2018
ingressantes2018 <- c(sum(d1.2018$Branco),
                      sum(d1.2018$Preta),
                      sum(d1.2018$Parda),
                      sum(d1.2018$Amarela),
                      sum(d1.2018$Indígena),
                      sum(d1.2018$`Não declarada`))

#data frame com as variaveis desejadas 
df.ingressantes2018 <- data.frame(raça2018,ingressantes2018)



# Data frame com as raças e ingressantes anuais
ingressantes <- c(df.ingressantes2010$ingressantes2010,
                  df.ingressantes2012$ingressantes2012,
                  df.ingressantes2014$ingressantes2014,
                  df.ingressantes2016$ingressantes2016,
                  df.ingressantes2018$ingressantes2018)


df.ing <- data.frame(
  Ano = as.numeric(rep(seq(2010,2018,2),each=6)),
  Ingressantes= ingressantes,
  Raça = factor(raça2018, levels = ordem_desejada)
)


minha_paleta1 <- c("Branco" = "#8DD3C7", "Preta" = "#FFFFB3", "Parda" = "#BEBADA",
                   "Amarela" = "#FB8072", "Indígena" = "#80B1D3", "Não declarada" = "#FDB462")


# Plotando gráfico

p1 <- ggplot(df.ing, aes(x=Ano, y=Ingressantes, fill=Raça, color= Raça, text=Raça)) + 
  geom_line(linewidth=1.5)+
  theme_gdocs()+
  scale_fill_brewer(breaks= c("Branco","Preta","Parda","Amarela","Indígena","Não declarada"))+
  scale_color_manual(values = minha_paleta1)+
  labs(
    title = "QUANTIDADE ANUAL DE INGRESSANTES DE GRADUAÇÃO NAS IES FEDERAIS PAULISTAS POR RAÇA",
    subtitle = "(2010-2018)",
    caption = "Fonte: INEP, Microdados do Censo da Educação Superior"
  )+
  theme(axis.text.x = element_text( hjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(0, 0, 0, 0))

p1

#Gráfico interativo
p1 <- ggplotly(p1, tooltip = "y")

p1
#salvar gráfico interativo 
saveWidget(p1, file="Ingressantes_interativo.html")




# Gráfico pequeno múltiplo


p2 <- ggplot(df.ing, aes(x=Ano, group=Raça,fill=Raça, y= Ingressantes)) +
  geom_line(aes(color=Raça), linewidth=1.5) +
  theme_gdocs() +
  scale_color_manual(values = minha_paleta1)+
  scale_fill_manual(values = minha_paleta1)+
  facet_wrap(~Raça) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank(),
    axis.text.x = element_text( hjust = 0.5),
    plot.title = element_text(hjust = 0.5))+
  labs( title = "QUANTIDADE ANUAL DE INGRESSANTES DE GRADUAÇÃO NAS IES FEDERAIS PAULISTAS SEPARADOS POR RAÇA")

p2

#Gráfico interativo
p2 <- ggplotly(p2, tooltip = "y")

p2

#salvar gráfico interativo
saveWidget(p2, file="Ingressantes_interativo_multiplos.html")

