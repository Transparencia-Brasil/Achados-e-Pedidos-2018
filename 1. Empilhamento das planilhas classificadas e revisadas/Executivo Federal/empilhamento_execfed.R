setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\Para empilhar - estagiários\\CGU")

library(dplyr)
library(rJava)
library(xlsxjars)
library(xlsx)
library(readxl)

####IMPORTAÇÃO
###############
ana_cgu <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/CGU/ana_cgu.xlsx", 
                      col_types = c("text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text"))

jose_cgu <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/CGU/jose_cgu_final.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text", 
                                     "text", "text", "text", "text"))

lizandra_cgu <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/CGU/lizandra_cgu.xlsx", 
                           col_types = c("text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text", "text", 
                                         "text", "text", "text"))

lizandra_cgu2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/CGU/lizandra_cgu2.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", 
                                          "text", "text", "text", "text", 
                                          "text", "text", "text", "text", 
                                          "text", "text", "text", "text", 
                                          "text", "text", "text", "text", 
                                          "text", "text", "text", "text", 
                                          "text", "text", "text", "text", 
                                          "text", "text", "text", 
                                          "text", "text", "text", 
                                          "text", "text", "text", 
                                          "text", "text", "text", 
                                          "text", "text", "text", 
                                          "text"))

lucas_cgu <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/CGU/lucas_cgu.xlsx", 
                        col_types = c("text", "text", "text", 
                                      "text", "text", "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text", "text", "text", 
                                      "text", "text"))

lucas_cgu2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/Para empilhar - estagiários/CGU/lucas_cgu2.xlsx", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "text", "text", 
                                       "text", "text", "text", "text", 
                                       "text", "text", "text", "text", 
                                       "text", "text", "text", "text", 
                                       "text", "text", "text", "text", 
                                       "text", "text", "text", 
                                       "text", "text", "text", 
                                       "text", "text", "text", 
                                       "text", "text", "text", 
                                       "text", "text", "text", 
                                       "text", "text", "text", 
                                       "text", "text", "text", 
                                       "text", "text", "text"))







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




teste_igualdade_nomes_var_df(ana_cgu, lucas_cgu2)

##consertando Jose

jose_cgu <- jose_cgu %>%
  rename(assunto = X__1)

##consertando Lizandra

lizandra_cgu2 <- lizandra_cgu2 %>%
  mutate(pasta_do_anexo_recurso_3 = NA,
         pasta_do_anexo_recurso_4 = NA,
         responsavel = "Lizandra") %>%
  select(-total)


##consertando Lucas

lucas_cgu <- lucas_cgu %>%
  mutate(responsavel = "Lucas")

#consertando Lucas2

lucas_cgu2 <- lucas_cgu2 %>%
  mutate(pasta_do_anexo_recurso_3 = NA,
         pasta_do_anexo_recurso_4 = NA,
         responsavel = "Lucas") %>%
  select(-total)
  

cgu_emp <- ana_cgu %>%
  bind_rows(jose_cgu, lizandra_cgu, lizandra_cgu2, lucas_cgu, lucas_cgu2)


write.xlsx(as.data.frame(cgu_emp), file="cgu_emp.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)


