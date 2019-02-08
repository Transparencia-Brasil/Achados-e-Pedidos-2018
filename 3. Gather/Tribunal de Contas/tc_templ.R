setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\versões finais - template")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(xlsx)

tcs <- read_excel("C:/Users/Renato/Desktop/Colab classificados/versões finais - template/tcs_050618.xlsx")

tcsf <- tcs %>%
  clean_names() %>%
  filter(is.na(nao_e_pedido_de_informacao)) %>%
  gather(interacao, conteudo, c(pedido, resposta, recurso_1,
                                resposta_recurso_1)) %>%
  filter(!is.na(conteudo)) %>%
  mutate(interacao = gsub("pedido", "Pedido", interacao),
         interacao = gsub("\\bresposta\\b", "Resposta do Pedido", interacao),
         interacao = gsub("\\brecurso_1\\b", "Recurso - 1º Instância", interacao),
         interacao = gsub("resposta_recurso_1", "Resposta do Recurso - 1º Instância", interacao),
         esfera = gsub("federal", "Federal", esfera),
         esfera = gsub("municipal", "Municipal", esfera),
         esfera = gsub("estadual", "Estadual", esfera),
         poder = gsub("tribunal de contas", "Tribunal de Contas", poder),
         orgao = gsub("tribunal de contas municipal do Rio de Janeiro", 
                      "Tribunal de Contas Municipal do Rio de Janeiro", orgao),
         orgao = gsub("tribunal de contas do municipio de sao paulo", 
                      "Tribunal de Contas do Município de São Paulo", orgao),
         orgao = gsub("tribunal de contas da uniao", 
                      "Tribunal de Contas da União", orgao),
         orgao = gsub("tribunal de contas estadual de roraima", 
                      "Tribunal de Contas Estadual de Roraima", orgao),
         orgao = gsub("tribunal de contas dos municipios do ceara", 
                      "Tribunal de Contas dos Municípios do Ceará", orgao),
         orgao = gsub("tribunal de contas estadual de goias", 
                      "Tribunal de Contas Estadual de Goiás", orgao),
         atendimento = gsub("Não classificado", "Não Classificado", atendimento),
         UF = ifelse(orgao == "Tribunal de Contas Municipal do Rio de Janeiro", "RJ",
                     ifelse(orgao == "Tribunal de Contas do Município de São Paulo", "SP",
                            ifelse(orgao == "Tribunal de Contas Estadual de Roraima","RR",
                                   ifelse(orgao=="Tribunal de Contas dos Municípios do Ceará","CE",
                                          ifelse(orgao=="Tribunal de Contas Estadual de Goiás", "GO", NA))))),
         municipio = ifelse(orgao=="Tribunal de Contas Municipal do Rio de Janeiro", "Rio de Janeiro",
                            ifelse(orgao=="Tribunal de Contas do Município de São Paulo", "São Paulo", NA)),
         PastaAnexos = ifelse(interacao == "Pedido", pedido_pasta_do_anexo_pedido,
                              ifelse(interacao == "Resposta do Pedido", pasta_do_anexo_resposta, NA)),
         NomeAnexos = ifelse(interacao == "Pedido", anexo_com_extensao_pedido,
                              ifelse(interacao == "Resposta do Pedido", anexo_com_extensao_resposta, NA)),
         PastaAnexos2 = case_when(orgao == "Tribunal de Contas do Município de São Paulo" & is.na(PastaAnexos) & 
                                !is.na(NomeAnexos) ~ "TCM-SP",
                                orgao == "Tribunal de Contas Estadual de Roraima" & is.na(PastaAnexos) & 
                                  !is.na(NomeAnexos) ~ "TCE-RR"),
         PastaAnexosF = ifelse(is.na(PastaAnexos), PastaAnexos2, PastaAnexos),
         PastaAnexosF = gsub("tcmsp / Anexo 20160099 R.xlsx", "TCM-SP", PastaAnexosF),
         PastaAnexosF = gsub("Anexo dos orgãos/ Tribunal de contas/TCM-SP", "TCM-SP", PastaAnexosF),
         PastaAnexosF = gsub("Anexos dos Orgãos/Tribunal de contas/TCM-SP", "TCM-SP", PastaAnexosF),
         PastaAnexosF = gsub("TCE-RR", "TCE-RR/", PastaAnexosF),
         PastaAnexosF = gsub("TCM-SP", "TCM-SP/", PastaAnexosF),
         NomeAnexos = ifelse(PastaAnexosF == "Anexo 20150194 R.pdf", "Anexo 20150194 R.pdf", NomeAnexos),
         PastaAnexosF = gsub("Anexo 20150194 R.pdf", "TCM-SP/", PastaAnexosF),
         id = NA,
         titulo = orgao,
         data = NA,
         situacao = "Finalizado",
         prorrogado = NA) %>%
  select(34, 4, 35, 27, 26, 36, 38, 3, 17, 37, 28, 29, 2, 1, 33, 31, 8)

"id"; "protocolo"; "titulo"; "conteudo"; "interação"; "data"; "prorrogado"; "órgão"; "status"; 
"situação"; "uf"; "município"; "esfera"; "nível"; "Pasta Anexos"; "Nome Anexos"


####PENDÊNCIAS!!!
##sem data. Pedir para Jessica arrumar
##inserir .PDF após os nomes dos anexos TCE-RR


write.xlsx(as.data.frame(tcsf), file="tcs_templ.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)
         

save(tcsf, file = "tcs_templ.RData")
