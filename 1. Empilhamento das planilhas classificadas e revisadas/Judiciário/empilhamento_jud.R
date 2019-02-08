##empilhar jud

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\Para empilhar - estagiários\\Judiciário (não é final)")

library(dplyr)
library(rJava)
library(xlsxjars)
library(xlsx)
library(readxl)

teste_igualdade_nomes_var_df <- function(base1, base2) {
  
  x <- names(base1)
  y <- names(base2)
  n <- length(x)
  k <- length(y)
  
  teste_nome_igual_x <- numeric()
  teste_nome_igual_y <- numeric()
  
  for ( i in 1:n) {
    teste_nome_igual_x[i] <- x[i] %in% y
  }
  
  for ( i in 1:k) {
    teste_nome_igual_y[i] <- y[i] %in% x
  }
  resp_x <- paste(x[!as.logical(teste_nome_igual_x)], collapse = ", ")
  resp_y <- paste(y[!as.logical(teste_nome_igual_y)], collapse = ", ")
  
  print(paste("as variáveis em x que não estão em y, são:", resp_x,
              ". E as variáveris de y que não estão em x, são:", resp_y,
              sep=" "))
  
}

anajud <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Judiciário (não é final)/ana_revisão_judiciário.xlsx")
josejud <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Judiciário (não é final)/jose_revisão_judiciário.xlsx")
lizjud <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Judiciário (não é final)/lizandra_revisão_judiciário.xlsx")
lizjudNA <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Judiciário (não é final)/lizandra_judNA_revisao.xlsx")
lucjud <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Judiciário (não é final)/lucas_revisão_judiciário.xlsx")

teste_igualdade_nomes_var_df(anajud, lucjud)


##correcao anajud

anajud <- anajud %>%
  mutate(contem_dados_pessoais = as.character(contem_dados_pessoais))

##correcao josejud

josejud <- josejud %>%
  mutate(duplicado = NA,
         complementação = NA,
         'base de dados' = as.character('base de dados'),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))

##correcao lizjud

lizjud <- lizjud %>%
  mutate(duplicado = NA,
         complementação = NA,
         anexo_com_extensao_resposta = ifelse(is.na(anexo_com_extensao_resposta), anexo,
                                              anexo_com_extensao_resposta)) %>%
  select(-c(anexo)) %>%
  mutate('base de dados' = as.character('base de dados'),
         contem_dados_pessoais = as.character(contem_dados_pessoais))


##correcao lizjudNA

lizjudNA <- lizjudNA %>%
  rename('novo assunto' = novo_assunto,
         'novo outros' = novo_outros,
         'base de dados' = base_de_dados) %>%
  mutate(duplicado = NA,
         complementação = NA)

lucjud <- lucjud %>%
  mutate(duplicado = NA,
         complementação = NA,
         contem_dados_pessoais = as.character(contem_dados_pessoais))

##empilhar

jud <- anajud %>%
  bind_rows(lizjud, lizjudNA, lucjud, josejud) %>%
  rename(novo_assunto = 'novo assunto')


glimpse(josejud)


###Encontrar colunas NA


Cols_AllMissing <- function(jud){ # helper function
  as.vector(which(colSums(is.na(jud)) == nrow(jud)))
}

valid <- jud %>%
  group_by(novo_assunto) %>%
  summarise(cont=n())


jud2 <- jud %>%
  select(-Cols_AllMissing(.)) %>%
  mutate(assunto_final_beta = ifelse(is.na(novo_assunto), assunto, novo_assunto),
         assunto_final_beta = gsub("6-base de dados", "6-bases de dados", assunto_final_beta),
         assunto_final_beta = gsub("4-admissão", "4-admissões", assunto_final_beta),
         assunto_final_beta = gsub("2-administração/servidores", "_verba servidores",
                                   assunto_final_beta),
         assunto_final_beta = gsub("oficio/normativo", "7-outros", assunto_final_beta),
         nao_e_pedido_de_informacao = gsub("1.0", "1", nao_e_pedido_de_informacao),
         nao_e_pedido_de_informacao = gsub("não", NA, nao_e_pedido_de_informacao))

valid <- jud2 %>%
  group_by(assunto_final_beta) %>%
  summarise(cont = n())

valid2 <- jud2 %>%
  filter(nao_e_pedido_de_informacao == "não")


write.xlsx(as.data.frame(jud2), file="jud2.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)