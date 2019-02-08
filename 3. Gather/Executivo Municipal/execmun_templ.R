###Municipal no template

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\versões finais - template")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(xlsx)


exec_municipal_final_250618 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/versões finais - template/exec_municipal_final_250618.xlsx", 
                                          col_types = c("text", "text", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", 
                                                        "text", "text", "text", "text", 
                                                        "text", "text", "text", 
                                                        "text", "text", "text", "text", 
                                                        "text", "text", "text"))


execmun <- exec_municipal_final_250618 %>%
  clean_names() %>%
  filter(is.na(nao_e_pedido_de_informacao)) %>%
  gather(interacao, conteudo, c(pedido, resposta, recurso_1, resposta_recurso_1,
                                recurso_2, resposta_recurso_2, recurso_3, resposta_recurso_3,
                                recurso_4, resposta_recurso_4)) %>%
  filter(!is.na(conteudo)) %>%
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
  mutate(NomeAnexos = gsub(",", ";", NomeAnexos),
         PastaAnexos2 = case_when(!is.na(NomeAnexos) & is.na(PastaAnexos1) ~ pasta_do_anexo_resposta)) %>%
  mutate(PastaAnexos = ifelse(is.na(PastaAnexos1), PastaAnexos2, PastaAnexos1)) %>%
  mutate(PastaAnexos = gsub(">", "/", PastaAnexos),
         PastaAnexos = gsub(" /", "/", PastaAnexos),
         PastaAnexos = gsub("/ ", "/", PastaAnexos),
         PastaAnexos = gsub("PMSP", "Prefeitura de São Paulo", PastaAnexos),
         PastaAnexos = gsub("pmsp", "Prefeitura de São Paulo", PastaAnexos),
         PastaAnexos = gsub("prefeitura de são paulo", "Prefeitura de São Paulo", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de SãoPaulo", "Prefeitura de São Paulo", PastaAnexos),
         PastaAnexos = gsub("Executivo/prefeitura de são paulo/\"Executivo/", "", PastaAnexos),
         PastaAnexos = gsub("Anexos dos órgãos/Executivo/", "", PastaAnexos),
         PastaAnexos = gsub("Executivo/prefeitura de são paulo/Executivo/", "", PastaAnexos),
         PastaAnexos = gsub("Anexos dos órgãos/Executivo/", "", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de São Paulo/\"", "", PastaAnexos),
         PastaAnexos = gsub("\n", "", PastaAnexos),
         PastaAnexos = gsub("\r", "", PastaAnexos),
         PastaAnexos = gsub("\"", "", PastaAnexos),
         PastaAnexos = gsub("Executivo/", "", PastaAnexos),
         PastaAnexos = gsub("parte4 (13581-16481)", "Prefeitura de São Paulo/parte4 (13581-16481)", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de São Paulo/Prefeitura de São Paulo/parte1 (4-894)", "Prefeitura de São Paulo/parte1 (4-894)", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de São Paulo/Prefeitura de São Paulo/parte4 (13581-16481)", "Prefeitura de São Paulo/parte4 (13581-16481)", PastaAnexos),
         PastaAnexos = gsub("pmrb", "Prefeitura de Rio Branco", PastaAnexos),
         PastaAnexos = gsub("pmjp", "Prefeitura de João Pessoa", PastaAnexos),
         PastaAnexos = gsub("pm Recife", "Prefeitura de Recife", PastaAnexos),
         PastaAnexos = gsub("pmRE", "Prefeitura de Recife", PastaAnexos),
         PastaAnexos = gsub("pmv", "Prefeitura de Vitória", PastaAnexos),
         PastaAnexos = gsub("pmVT", "Prefeitura de Vitória", PastaAnexos),
         PastaAnexos = gsub("prefeitura", "Prefeitura", PastaAnexos),
         PastaAnexos = gsub("dde", "de", PastaAnexos),
         PastaAnexos = gsub("Prefeitura do Rio Branco", "Prefeitura de Rio Branco", PastaAnexos),
         PastaAnexos = gsub("pedidodetransparencia", "pedidostransparencia", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de Rio Branco/anexos 2014", "Prefeitura de Rio Branco/ANO 2014", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de Rio Branco/Ano 2015", "Prefeitura de Rio Branco/ANO 2015", PastaAnexos),
         PastaAnexos = gsub("anexos 2012", "Anexos 2012", PastaAnexos),
         PastaAnexos = gsub("Anexo de 2012", "Anexos 2012", PastaAnexos),
         PastaAnexos = gsub("Anexo de 2013", "Anexos 2013", PastaAnexos),
         PastaAnexos = gsub("Anexo de 2014", "Anexos 2014", PastaAnexos),
         PastaAnexos = gsub("Anexo de 2016", "Anexos 2016", PastaAnexos),
         PastaAnexos = gsub("Anexo de 2017", "Anexos 2017", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de joão pessoa", "Prefeitura de João Pessoa", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de São Paulo/parte 3(8330-13571)", "Prefeitura de São Paulo/parte3 (8330-13571)", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de São Paulo/parte 5 (16484-19684)", "Prefeitura de São Paulo/parte5 (16484-19684)", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de São Paulo/Prefeitura de São Paulo/parte1 (4-894)", "Prefeitura de São Paulo/parte1 (4-894)", PastaAnexos),
         PastaAnexos = gsub("Prefeitura de São Paulo/Prefeitura de São Paulo/parte4 (13581-16481)", "Prefeitura de São Paulo/parte4 (13581-16481)", PastaAnexos)) %>%
  mutate(orgao = gsub("controladoria-geral do municipio de sao paulo", "Prefeitura Municipal de São Paulo", orgao),
         orgao = gsub("prefeitura municipal do recife", "Prefeitura Municipal de Recife", orgao),
         orgao = gsub("prefeitura municipal de joao pessoa", "Prefeitura Municipal de João Pessoa", orgao),
         orgao = gsub("prefeitura municipal de vitoria", "Prefeitura Municipal de Vitória", orgao),
         orgao = gsub("prefeitura municipal de Salvador", "Prefeitura Municipal de Salvador", orgao),
         orgao = gsub("prefeitura municipal de rio branco", "Prefeitura Municipal de Rio Branco", orgao),
         orgao = gsub("prefeitura municipal de porto velho", "Prefeitura Municipal de Porto Velho", orgao),
         poder = gsub("executivo", "Executivo", poder),
         esfera = gsub("municipal", "Municipal", esfera),
         UF = ifelse(orgao == "Prefeitura Municipal de Recife", "PE",
                     ifelse(orgao == "Prefeitura Municipal de João Pessoa", "PB",
                            ifelse(orgao == "Prefeitura Municipal de Vitória", "ES",
                                   ifelse(orgao == "Prefeitura Municipal de Salvador", "BA",
                                          ifelse(orgao == "Prefeitura Municipal de Rio Branco", "AC",
                                                 ifelse(orgao == "Prefeitura Municipal de Porto Velho", "RO",
                                                        ifelse(orgao == "Prefeitura Municipal de São Paulo", "SP", NA))))))),
         situacao = "Finalizado",
         id = NA,
         prorrogado = NA,
         titulo = orgao,
         municipio = ifelse(orgao == "Prefeitura Municipal de Recife", "Recife",
                            ifelse(orgao == "Prefeitura Municipal de João Pessoa", "João Pessoa",
                                   ifelse(orgao == "Prefeitura Municipal de Vitória", "Vitória",
                                          ifelse(orgao == "Prefeitura Municipal de Salvador", "Salvador",
                                                 ifelse(orgao == "Prefeitura Municipal de Rio Branco", "Rio Branco",
                                                        ifelse(orgao == "Prefeitura Municipal de Porto Velho", "Porto Velho",
                                                               ifelse(orgao == "Prefeitura Municipal de São Paulo", "São Paulo", NA)))))))) %>%
  mutate(interacao = gsub("pedido", "Pedido", interacao),
         interacao = gsub("\\bresposta\\b", "Resposta do Pedido", interacao),
         interacao = gsub("\\brecurso_1\\b", "Recurso - 1º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_1\\b", "Resposta do Recurso - 1º Instância", interacao),
         interacao = gsub("\\brecurso_2\\b", "Recurso - 2º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_2\\b", "Resposta do Recurso - 2º Instância", interacao),
         interacao = gsub("\\brecurso_3\\b", "Recurso - 3º Instância", interacao),
         interacao = gsub("\\bresposta_recurso_3\\b", "Resposta do Recurso - 3º Instância", interacao)) %>%
  select(53, 4, 55, 46, 45, 47, 54, 3, 7, 43, 52, 56, 1, 2, 51, 49, 5)

  
"id"; "protocolo"; "titulo"; "conteudo"; "interação"; "data"; "prorrogado"; "órgão"; "status"; 
"situação"; "uf"; "município"; "esfera"; "nível"; "Pasta Anexos"; "Nome Anexos"


write.xlsx(as.data.frame(execmun), file="emunic_templ.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

save(execmun, file = "emunic.RData")
                              