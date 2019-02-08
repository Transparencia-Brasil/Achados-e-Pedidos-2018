##executivo federal template

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\versões finais - template")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(xlsx)

cgu_original <- read_excel("C:/Users/Renato/Desktop/Colab classificados/versões finais - template/cgu_final_14082018.xlsx", 
                  col_types = c("text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text", "text", 
                                "text", "text", "text", "text", "text", 
                                "text", "text", "text", "text", 
                                "text", "text", "text", "text", 
                                "text", "text", "text", "text", 
                                "text", "text", "text", "text", 
                                "text", "text", "text", "text", 
                                "text", "text", "text", "text", 
                                "text", "text", "text", "text", 
                                "text", "text", "text", "text"))

cgu <- cgu_original %>%
  clean_names() %>%
  gather(interacao, conteudo, c(pedido, resposta, recurso_1,
                                resposta_recurso_1, recurso_2, resposta_recurso_2,
                                recurso_3, resposta_recurso_3, recurso_4, resposta_recurso_4)) %>%
  filter(is.na(nao_e_pedido_de_informacao),
         !is.na(conteudo)) %>%
  mutate(teste = grepl("^\\&lt.*", conteudo)) %>%
  filter(teste == "FALSE") %>%
  mutate(data = ifelse(interacao == "pedido", data_do_pedido,
                       ifelse(interacao == "resposta", data_da_resposta,
                              ifelse(interacao == "recurso_1", data_recurso_1,
                                     ifelse(interacao == "resposta_recurso_1", data_resposta_recurso_1,
                                            ifelse(interacao == "recurso_2", data_recurso_2,
                                                   ifelse(interacao == "resposta_recurso_2", data_resposta_recurso_2,
                                                          ifelse(interacao == "recurso_3", data_recurso_3,
                                                                 ifelse(interacao == "resposta_recurso_3", data_resposta_recurso_3,
                                                                        ifelse(interacao == "recurso_4", data_recurso_4,
                                                                               ifelse(interacao == "resposta_recurso_4", data_resposta_recurso_4, NA))))))))))) %>%
  mutate(PastaAnexos = pasta_do_anexo_resposta) %>%
  mutate(interacao = gsub("pedido", "Pedido", interacao),
         interacao = gsub("\\bresposta\\b", "Resposta do Pedido", interacao),
         interacao = gsub("\\brecurso_1\\b", "Recurso - 1º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_1\\b", "Resposta do Recurso - 1º Instância", interacao),
         interacao = gsub("\\brecurso_2\\b", "Recurso - 2º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_2\\b", "Resposta do Recurso - 2º Instância", interacao),
         interacao = gsub("\\brecurso_3\\b", "Recurso - 3º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_3\\b", "Resposta do Recurso - 3º Instância", interacao)) %>%
  mutate(UF = NA,
         municipio = NA,
         prorrogado = NA,
         id = NA,
         titulo = orgao,
         situacao = "Finalizado",
         NomeAnexos = NA) %>%
  select(53, 4, 54, 46, 45, 48, 52, 3, 9, 55, 50, 51, 1, 2, 49, 56, 7, 5, 6)


"id"; "protocolo"; "titulo"; "conteudo"; "interação"; "data"; "prorrogado"; "órgão"; "status"; 
"situação"; "uf"; "município"; "esfera"; "nível"; "Pasta Anexos"; "Nome Anexos" 


write.xlsx(as.data.frame(cgu), file="exefed_templ.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

save(cgu, file = "exefed_templ.RData")

