setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\Para empilhar - estagiários\\Executivo estadual (não é final!)")


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

anaest1 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo estadual (não é final!)/ana_base_executivo_estadual_mg_ma.xlsx")
anaest2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo estadual (não é final!)/Ana_base_executivo_estadual1.xlsx")
josest1 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo estadual (não é final!)/jose_base_executivo_estadual_mg_ma.xlsx")
joseest2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo estadual (não é final!)/José_base_executivo_estadual1.xlsx")
lizest1 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo estadual (não é final!)/liz_base_executivo_estadual_mg_ma.xlsx")
lizest2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo estadual (não é final!)/Lizandra_base_executivo_estadual1.xlsx")
lucasest1 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo estadual (não é final!)/lucas_base_executivo_estadual_mg_ma.xlsx")
lucasest2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo estadual (não é final!)/Lucas_base_executivo_estadual1.xlsx")

teste_igualdade_nomes_var_df(anaest1, lizest1)


##correcao anaest1

anaest1 <- anaest1 %>%
  select(-58) %>%
  mutate(responsavel,
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         data_do_pedido = as.character(data_do_pedido),
         data_da_resposta = as.character(data_da_resposta),
         data_recurso_1 = as.character(data_recurso_1),
         data_resposta_recurso_1 = as.character(data_resposta_recurso_1),
         data_resposta_recurso_2 = as.character(data_resposta_recurso_2),
         data_recurso_2 = as.character(data_recurso_2),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))

##correcao anaest2

anaest2 <- anaest2 %>%
  mutate(id_maranhao = NA,
         obs_id_ma = NA,
         id_recurso_ma1 = NA,
         id_recurso_ma2 = NA,
         id_recurso_ma3 = NA,
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         data_do_pedido = as.character(data_do_pedido),
         data_da_resposta = as.character(data_da_resposta),
         data_recurso_1 = as.character(data_recurso_1),
         data_resposta_recurso_1 = as.character(data_resposta_recurso_1),
         data_resposta_recurso_2 = as.character(data_resposta_recurso_2),
         data_recurso_2 = as.character(data_recurso_2),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))

##correcao hugoest1

hugoest1 <- hugoest1 %>%
  mutate(id_maranhao = NA,
         obs_id_ma = NA,
         id_recurso_ma1 = NA,
         id_recurso_ma2 = NA,
         id_recurso_ma3 = NA,
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         data_do_pedido = as.character(data_do_pedido),
         data_da_resposta = as.character(data_da_resposta),
         data_recurso_1 = as.character(data_recurso_1),
         data_resposta_recurso_1 = as.character(data_resposta_recurso_1),
         data_resposta_recurso_2 = as.character(data_resposta_recurso_2),
         data_recurso_2 = as.character(data_recurso_2),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))

##correcao josest1

josest1 <- josest1 %>%
  mutate(id_maranhao = NA,
         obs_id_ma = NA,
         id_recurso_ma1 = NA,
         id_recurso_ma2 = NA,
         id_recurso_ma3 = NA,
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         data_do_pedido = as.character(data_do_pedido),
         data_da_resposta = as.character(data_da_resposta),
         data_recurso_1 = as.character(data_recurso_1),
         data_resposta_recurso_1 = as.character(data_resposta_recurso_1),
         data_resposta_recurso_2 = as.character(data_resposta_recurso_2),
         data_recurso_2 = as.character(data_recurso_2),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))
  

##correcao joseest2

joseest2 <- joseest2 %>%
  select(-12) %>%
  mutate(id_maranhao = NA,
         obs_id_ma = NA,
         id_recurso_ma1 = NA,
         id_recurso_ma2 = NA,
         id_recurso_ma3 = NA,
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         data_do_pedido = as.character(data_do_pedido),
         data_da_resposta = as.character(data_da_resposta),
         data_recurso_1 = as.character(data_recurso_1),
         data_resposta_recurso_1 = as.character(data_resposta_recurso_1),
         data_resposta_recurso_2 = as.character(data_resposta_recurso_2),
         data_recurso_2 = as.character(data_recurso_2),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))
 

##correcao lizest1

lizest1 <- lizest1 %>%
  select(-56) %>%
  mutate(contem_dados_pessoais = as.character(contem_dados_pessoais),
         data_do_pedido = as.character(data_do_pedido),
         data_da_resposta = as.character(data_da_resposta),
         data_recurso_1 = as.character(data_recurso_1),
         data_resposta_recurso_1 = as.character(data_resposta_recurso_1),
         data_resposta_recurso_2 = as.character(data_resposta_recurso_2),
         data_recurso_2 = as.character(data_recurso_2),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))


##correcao lizest2

lizest2 <- lizest2 %>%
  mutate(id_maranhao = NA,
         obs_id_ma = NA,
         id_recurso_ma1 = NA,
         id_recurso_ma2 = NA,
         id_recurso_ma3 = NA,
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         data_do_pedido = as.character(data_do_pedido),
         data_da_resposta = as.character(data_da_resposta),
         data_recurso_1 = as.character(data_recurso_1),
         data_resposta_recurso_1 = as.character(data_resposta_recurso_1),
         data_resposta_recurso_2 = as.character(data_resposta_recurso_2),
         data_recurso_2 = as.character(data_recurso_2),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao)) %>%
  rename(atendimento = `Não Atendido`)

##correcao lucasest1

lucasest1 <- lucasest1 %>%
  select(-58) %>%
  mutate(responsavel = NA,
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         data_do_pedido = as.character(data_do_pedido),
         data_da_resposta = as.character(data_da_resposta),
         data_recurso_1 = as.character(data_recurso_1),
         data_resposta_recurso_1 = as.character(data_resposta_recurso_1),
         data_resposta_recurso_2 = as.character(data_resposta_recurso_2),
         data_recurso_2 = as.character(data_recurso_2),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))

##correcao lucasest2

lucasest2 <- lucasest2 %>%
  select(-12) %>%
  mutate(id_maranhao = NA,
         obs_id_ma = NA,
         id_recurso_ma1 = NA,
         id_recurso_ma2 = NA,
         id_recurso_ma3 = NA,
         contem_dados_pessoais = as.character(contem_dados_pessoais),
         data_do_pedido = as.character(data_do_pedido),
         data_da_resposta = as.character(data_da_resposta),
         data_recurso_1 = as.character(data_recurso_1),
         data_resposta_recurso_1 = as.character(data_resposta_recurso_1),
         data_resposta_recurso_2 = as.character(data_resposta_recurso_2),
         data_recurso_2 = as.character(data_recurso_2),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao))
  

##empilhando

execest <- anaest1 %>%
  bind_rows(anaest2, josest1, joseest2, lizest1, lizest2, lucasest1, lucasest2)


write.xlsx(as.data.frame(execest), file="execest.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
