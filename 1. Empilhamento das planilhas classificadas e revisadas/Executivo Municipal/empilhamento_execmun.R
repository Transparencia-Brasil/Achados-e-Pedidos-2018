##empilhar pref

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\Para empilhar - estagiários\\Executivo municipal")

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

anapref <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo municipal/ana_base_pref.xlsx")
hugopref <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo municipal/hugo_prefs.xlsx")
josepref <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo municipal/jose_base_pref.xlsx")
lizpref <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo municipal/lizandra_base_pref.xlsx")
lucaspref <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Executivo municipal/lucas_base_pref.xlsx")


teste_igualdade_nomes_var_df(anapref, lucaspref)

##correcao anapref

anapref <- anapref %>%
  mutate(data_do_pedido = as.character(data_do_pedido),
         data_recurso_1 = as.character(data_recurso_1),
         data_recurso_2 = as.character(data_recurso_2),
         resposta_duplicada = as.character(resposta_duplicada),
         data_da_resposta = as.character(data_da_resposta),
         e_complementacao_de_pedido = as.character(e_complementacao_de_pedido),
         resposta_duplicada = as.character(resposta_duplicada))

##correcao hugopref

hugopref <- hugopref %>%
  mutate(revisão = NA,
         responsavel = NA,
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao),
         data_do_pedido = as.character(data_do_pedido),
         data_recurso_1 = as.character(data_recurso_1),
         data_recurso_2 = as.character(data_recurso_2),
         resposta_duplicada = as.character(resposta_duplicada),
         data_da_resposta = as.character(data_da_resposta),
         e_complementacao_de_pedido = as.character(e_complementacao_de_pedido),
         resposta_duplicada = as.character(resposta_duplicada))

##correcao lizpref

lizpref <- lizpref %>%
  mutate(revisão = NA,
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao),
         data_do_pedido = as.character(data_do_pedido),
         data_recurso_1 = as.character(data_recurso_1),
         data_recurso_2 = as.character(data_recurso_2),
         resposta_duplicada = as.character(resposta_duplicada),
         data_da_resposta = as.character(data_da_resposta),
         e_complementacao_de_pedido = as.character(e_complementacao_de_pedido),
         resposta_duplicada = as.character(resposta_duplicada))

##correcao lucaspref

lucaspref <- lucaspref %>%
  mutate(revisão = NA,
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao),
         data_do_pedido = as.character(data_do_pedido),
         data_recurso_1 = as.character(data_recurso_1),
         data_recurso_2 = as.character(data_recurso_2),
         resposta_duplicada = as.character(resposta_duplicada),
         data_da_resposta = as.character(data_da_resposta),
         e_complementacao_de_pedido = as.character(e_complementacao_de_pedido))

##correcao josepref

josepref <- josepref %>%
  mutate(e_complementacao_de_pedido = as.character(e_complementacao_de_pedido),
         resposta_duplicada = as.character(resposta_duplicada),
         data_do_pedido = as.character(data_do_pedido),
         nao_e_pedido_de_informacao = as.character(nao_e_pedido_de_informacao),
         data_recurso_1 = as.character(data_recurso_1),
         data_recurso_2 = as.character(data_recurso_2),
         resposta_duplicada = as.character(resposta_duplicada),
         data_da_resposta = as.character(data_da_resposta),
         e_complementacao_de_pedido = as.character(e_complementacao_de_pedido))

#empilhar

exec <- anapref %>%
  bind_rows(lizpref, hugopref, josepref, lucaspref) %>%
  clean_names()


valid <- exec %>%
  group_by(assunto) %>%
  summarise(cont=n())



write.xlsx(as.data.frame(exec), file="exec.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
