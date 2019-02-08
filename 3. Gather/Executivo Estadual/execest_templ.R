##executivo estadual template

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\versões finais - template")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(xlsx)

execest_original <- read_excel("C:/Users/Renato/Desktop/Colab classificados/versões finais - template/exec_estadual_final_01072018.xlsx", 
                      col_types = c("text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text", 
                                    "text", "text", "text", 
                                    "text", "text", "text", 
                                    "text", "text", "text", 
                                    "text", "text", "text", "text", 
                                    "text", "text", "text"))

execest <- execest_original %>%
  clean_names() %>%
  gather(interacao, conteudo, c(pedido, resposta, recurso_1,
                                resposta_recurso_1, recurso_2, resposta_recurso_2,
                                recurso_3, resposta_recurso_3, recurso_4, resposta_recurso_4)) %>%
  filter(is.na(nao_e_pedido_de_informacao),
         !is.na(conteudo)) %>%
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
  mutate(PastaAnexos1 = ifelse(interacao == "pedido", pasta_do_anexo_pedido,
                               ifelse(interacao == "resposta", pasta_do_anexo_resposta,
                                      ifelse(interacao == "recurso_1", pasta_do_anexo_recurso_1,
                                             ifelse(interacao == "resposta_recurso_1", pasta_do_anexo_resposta_recurso_1,
                                                    ifelse(interacao == "recurso_2", pasta_do_anexo_recurso_2,
                                                           ifelse(interacao == "resposta_recurso_2", pasta_do_anexo_resposta_recurso_2,
                                                                  ifelse(interacao == "recurso_3", pasta_do_anexo_recurso_3,
                                                                         ifelse(interacao == "resposta_recurso_3", pasta_do_anexo_resposta_recurso_3,
                                                                                ifelse(interacao == "recurso_4", pasta_do_anexo_recurso_4,
                                                                                       ifelse(interacao == "resposta_recurso_4", pasta_do_anexo_resposta_recurso_4, NA))))))))))) %>%
  mutate(NomeAnexos = ifelse(interacao == "pedido", anexo_com_extensao_pedido,
                             ifelse(interacao == "resposta", anexo_com_extensao_resposta,
                                    ifelse(interacao == "recurso_1", anexo_com_extensao_recurso_1,
                                           ifelse(interacao == "resposta_recurso_1", anexo_com_extensao_resposta_recurso_1,
                                                  ifelse(interacao == "recurso_2", anexo_com_extensao_recurso_2,
                                                         ifelse(interacao == "resposta_recurso_2", anexo_com_extensao_resposta_recurso_2,
                                                                ifelse(interacao == "recurso_3", anexo_com_extensao_recurso_3,
                                                                       ifelse(interacao == "resposta_recurso_3", anexo_com_extensao_resposta_recurso_3,
                                                                              ifelse(interacao == "recurso_4", anexo_com_extensao_recurso_4,
                                                                                     ifelse(interacao == "resposta_recurso_4", anexo_com_extensao_resposta_recurso_4, NA))))))))))) %>%
  mutate(PastaAnexos2 = case_when(!is.na(NomeAnexos) & is.na(PastaAnexos1) ~ pasta_do_anexo_resposta)) %>%
  mutate(PastaAnexos = ifelse(is.na(PastaAnexos1), PastaAnexos2, PastaAnexos1)) %>%
  mutate(interacao = gsub("pedido", "Pedido", interacao),
         interacao = gsub("\\bresposta\\b", "Resposta do Pedido", interacao),
         interacao = gsub("\\brecurso_1\\b", "Recurso - 1º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_1\\b", "Resposta do Recurso - 1º Instância", interacao),
         interacao = gsub("\\brecurso_2\\b", "Recurso - 2º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_2\\b", "Resposta do Recurso - 2º Instância", interacao),
         interacao = gsub("\\brecurso_3\\b", "Recurso - 3º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_3\\b", "Resposta do Recurso - 3º Instância", interacao)) %>%
  mutate(UF = ifelse(orgao == "Governo do Estado de Minas Gerais", "MG",
                     ifelse(orgao == "Governo de Estado do Maranhão", "MA",
                            ifelse(orgao == "Governo do Estado do Rio Grande do Norte", "RN",
                                   ifelse(orgao == "Governo do Estado de Alagoas", "AL",
                                          ifelse(orgao == "Governo do Estado do Rio Grande do Sul", "RS", NA))))),
         municipio = NA,
         prorrogado = NA,
         id = NA,
         titulo = orgao,
         situacao = "Finalizado") %>%
  select(58, 1, 59, 49, 48, 50, 57, 5, 11, 60, 55, 56, 4, 3, 54, 52, 6)
  
  
"id"; "protocolo"; "titulo"; "conteudo"; "interação"; "data"; "prorrogado"; "órgão"; "status"; 
"situação"; "uf"; "município"; "esfera"; "nível"; "Pasta Anexos"; "Nome Anexos"  

write.xlsx(as.data.frame(execest), file="execest_templ.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

save(execest, file = "execest_templ.RData")
