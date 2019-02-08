##empilhar leg

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\Para empilhar - estagiários\\Legislativo")

library(dplyr)
library(rJava)
library(xlsxjars)
library(xlsx)

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

analeg1 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Legislativo/ana_revisão_legislativo1.xlsx")
analeg2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Legislativo/ana_revisão_legislativo2.xlsx")
joseleg1 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Legislativo/jose_revisão_legislativo1.xlsx")
joseleg2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Legislativo/jose_revisão_legislativo2.xlsx")
lizleg1 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Legislativo/lizandra_revisão_legislativo1.xlsx")
lizleg2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Legislativo/lizandra_revisão_legislativo2.xlsx")
lizlegNA <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Legislativo/lizandra_legNA_revisao.xlsx")
lucleg1 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Legislativo/lucas_revisão_legislativo.xlsx")
lucleg2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/Legislativo/lucas_revisão_legislativo2.xlsx")


teste_igualdade_nomes_var_df(lucleg1, lucleg2)

#correção analeg2

analeg2 <- analeg2 %>%
  rename(pedido = 'pedido+)y2') %>%
  mutate(outros = NA)

#NA novo assunto




#correção joseleg1

joseleg1 <- joseleg1 %>%
  mutate('base de dados' = NA)

#correcao joseleg2

joseleg2 <- joseleg2 %>%
  mutate(outros = NA)

#correcao lizleg1

lizleg1 <- lizleg1 %>%
  mutate('base de dados' = NA)

#correcao lizleg2

lizleg2 <- lizleg2 %>%
  mutate(outros = NA)

#correcao lizlegNA

lizlegNA <- lizlegNA %>%
  rename('novo outros' = novo_outros,
         'base de dados' = base_de_dados)

#correcao lucleg1

lucleg1 <- lucleg1 %>%
  rename(novo_assunto = 'novo assunto') %>%
  mutate('base de dados' = NA) %>%
  rename(visto_jessica = 'Visto Jessica') %>%
  select(-c(visto_jessica))

#correcao lucleg2

lucleg2 <- lucleg2 %>%
  mutate(outros = NA,
         'base de dados' = NA)
  
leg <- analeg1 %>%
  bind_rows(analeg2, joseleg1, joseleg2, lizleg1, lizleg2, lizlegNA, lucleg1,
            lucleg2)

###Encontrar colunas NA


Cols_AllMissing <- function(leg){ # helper function
  as.vector(which(colSums(is.na(leg)) == nrow(leg)))
}


leg2 <- leg %>%
  select(-Cols_AllMissing(.)) %>%
  mutate(assunto_final_beta = ifelse(is.na(novo_assunto), assunto, novo_assunto))

valid <- leg2 %>%
  filter(is.na(nao_e_pedido_de_informacao)) %>%
  group_by(assunto_final_beta) %>%
  summarise(cont = n())
  

teste_igualdade_nomes_var_df(leg, leg2)

names(leg2)

unique(leg2$anexo_com_extensao_resposta_recurso_1)

write.xlsx(as.data.frame(leg2), file="leg2.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)



set.seed(15)
x <- sample_n(leg2, 10)
