setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\versões finais - template")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(xlsx)

mp_original <- read_excel("C:/Users/Renato/Desktop/Colab classificados/versões finais - template/mp_final_18062018.xlsx", 
                 col_types = c("text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text"))


mp <- mp_original %>%
  clean_names() %>%
  gather(interacao, conteudo, c(pedido, resposta, recurso_1,
                                resposta_recurso_1, recurso_2, resposta_recurso_2)) %>%
  filter(is.na(nao_e_pedido_de_informacao),
         !is.na(conteudo)) %>%
  mutate(interacao = gsub("pedido", "Pedido", interacao),
         interacao = gsub("\\bresposta\\b", "Resposta do Pedido", interacao),
         esfera = gsub("estadual", "Estadual", esfera),
         poder = gsub("ministerio publico", "Ministério Público", poder),
         orgao = gsub("ministerio publico estadual do rio de janeiro", "Ministério Público Estadual do Rio de Janeiro", orgao),
         orgao = gsub("ministerio publico estadual do piaui", "Ministério Público Estadual do Piauí", orgao),
         data = ifelse(interacao == "Pedido", data_do_pedido, NA),
         PastaAnexos = ifelse(interacao == "Pedido", pedido_pasta_do_anexo_pedido,
                              ifelse(interacao == "Resposta do Pedido", pasta_do_anexo_resposta, NA)),
         NomeAnexos = ifelse(interacao == "Pedido", anexo_com_extensao_pedido,
                             ifelse(interacao == "Resposta do Pedido", anexo_com_extensao_resposta, NA)),
         PastaAnexos = gsub("ministerio publico estadual do piaui/MPPI/2013", "MPPI/2013", PastaAnexos),
         PastaAnexos = gsub("MPPI/2013", "MPPI/2013/", PastaAnexos),
         PastaAnexos = gsub("MPPI/2014", "MPPI/2014/", PastaAnexos),
         PastaAnexos = gsub("MPPI/2015", "MPPI/2015/", PastaAnexos),
         PastaAnexos = gsub("MPPI/2016", "MPPI/2016/", PastaAnexos),
         PastaAnexos = gsub("MPPI/2017", "MPPI/2017/", PastaAnexos),
         NomeAnexos = gsub(",", ";", NomeAnexos),
         NomeAnexos = gsub("/", ";", NomeAnexos),
         UF = ifelse(orgao == "Ministério Público Estadual do Rio de Janeiro", "RJ",
                     ifelse(orgao == "Ministério Público Estadual do Piauí", "PI", NA)),
         municipio = NA,
         prorrogado = NA,
         titulo = orgao,
         id = NA,
         situacao = "Finalizado") %>%
  select(38, 4, 37, 30, 29, 31, 36, 3, 8, 39, 34, 35, 2, 1, 32, 33, 7)


"id"; "protocolo"; "titulo"; "conteudo"; "interação"; "data"; "prorrogado"; "órgão"; "status"; 
"situação"; "uf"; "município"; "esfera"; "nível"; "Pasta Anexos"; "Nome Anexos"


##substituir , por ; na coluna "NomeAnexos" Fazer isso nos anteriores!
##checar se datas estão OK - JESSICA

write.xlsx(as.data.frame(mp), file="mps_templ.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

save(mp, file = "mp_templ.RData")

  
