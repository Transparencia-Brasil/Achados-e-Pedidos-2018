setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\versões finais - template")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(xlsx)

###LEGISLATIVO
###########

##Passando planilha para o template


leg_final_18062018 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/versões finais - template/leg_final_18062018.xlsx", 
                                 col_types = c("text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text"))
leg <- leg_final_18062018 %>%
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
         esfera = gsub("distrital", "Distrital", esfera),
         esfera = gsub("municipal", "Municipal", esfera),
         esfera = gsub("estadual", "Estadual", esfera),
         poder = gsub("legislativo", "Legislativo", poder),
         orgao = gsub("camara dos deputados", "Câmara dos Deputados", orgao),
         orgao = gsub("camara legislativa do distrito federal", "Câmara Legislativa do Distrito Federal", orgao),
         orgao = gsub("camara municipal de curitiba", "Câmara Municipal de Curitiba", orgao),
         orgao = gsub("camara municipal de fortaleza", "Câmara Municipal de Fortaleza", orgao),
         orgao = gsub("camara municipal de salvador", "Câmara Municipal de Salvador", orgao),
         orgao = gsub("assembleia legislativa de pernambuco", "Assembleia Legislativa de Pernambuco", orgao),
         atendimento = gsub("Não atendido", "Não Atendido", atendimento),
         atendimento = gsub("atendido", "Atendido", atendimento),
         UF = ifelse(orgao == "Câmara Legislativa do Distrito Federal", "DF",
                     ifelse(orgao == "Câmara Municipal de Curitiba", "PR",
                            ifelse(orgao == "Câmara Municipal de Fortaleza", "PE",
                                   ifelse(orgao == "Câmara Municipal de Salvador", "BA",
                                          ifelse(orgao == "Assembleia Legislativa de Pernambuco", "PE", NA))))),
         municipio = ifelse(orgao == "Câmara Municipal de Curitiba", "Curitiba",
                            ifelse(orgao == "Câmara Municipal de Fortaleza", "Fortaleza",
                                   ifelse(orgao == "Câmara Municipal de Salvador", "Salvador", NA))),
         PastaAnexos = ifelse(interacao == "Pedido", pasta_do_anexo_pedido,
                              ifelse(interacao == "Resposta do Pedido", pasta_do_anexo_resposta,
                                     ifelse(interacao == "Recurso - 1º Instância", pasta_do_anexo_recurso_1,
                                            ifelse(interacao == "Resposta do Recurso - 1º Instância", pasta_do_anexo_resposta_recurso_1, NA)))),
         PastaAnexos = gsub("curitiba", "Curitiba", PastaAnexos),
         PastaAnexos = gsub("camara", "Câmara", PastaAnexos),
         PastaAnexos = gsub("cmc", "Câmara de Curitiba", PastaAnexos),
         PastaAnexos = gsub("salvador>Câmara", "Câmara", PastaAnexos),
         PastaAnexos = gsub(">", "/", PastaAnexos),
         PastaAnexos = gsub("\\banexo\\b", "Anexos", PastaAnexos),
         PastaAnexos = gsub("Assembleia Legislativa do DF", "Assembleia Legislativa do DF/", PastaAnexos),
         PastaAnexos = gsub("Assembleia Legislativa DF", "Assembleia Legislativa do DF/", PastaAnexos),
         PastaAnexos = gsub("Câmara de Salvador", "Câmara de Salvador/", PastaAnexos),
         PastaAnexos = gsub("anexos", "Anexos", PastaAnexos),
         PastaAnexos = gsub("Câmara Curitiba", "Câmara de Curitiba", PastaAnexos),
         PastaAnexos = gsub("Câmara Municipal de Curitiba", "Câmara de Curitiba", PastaAnexos),
         PastaAnexos = gsub("Cãmara de Curitiba/ Anexos sic 2016", "Câmara de Curitiba/Anexos sic 2016", PastaAnexos),
         PastaAnexos = gsub("Câmara de Curitiba /Anexos sic 2014", "Câmara de Curitiba/Anexos sic 2014", PastaAnexos),
         PastaAnexos = gsub("Camara de Curitiba/Anexos sic 2013/900.00106.2013.zip", "Câmara de Curitiba/Anexos sic 2013", PastaAnexos),
         PastaAnexos = gsub("\\<Anexos sic 2014\\>", "Câmara de Curitiba/Anexos sic 2014", PastaAnexos),
         PastaAnexos = gsub("Câmara de Curitiba/Câmara de Curitiba/Anexos sic 2014", "Câmara de Curitiba/Anexos sic 2014", PastaAnexos),
         NomeAnexos = case_when(interacao == "Pedido" ~ anexo_com_extensao_pedido,
                                interacao == "Resposta do Pedido" ~ anexo_com_extensao_resposta,
                                interacao == "\\bRecurso - 1º Instância\\b" ~ anexo_com_extensao_recurso_1,
                                interacao == "Resposta do Recurso - 1º Instância" ~ anexo_com_extensao_resposta_recurso_1),
         id = NA,
         titulo = orgao,
         data = NA,
         prorrogado = NA,
         situacao = "Finalizado",
         NomeAnexos2 = case_when(!is.na(PastaAnexos) & is.na(NomeAnexos) ~ anexo_com_extensao_recurso_1),
         NomeAnexosF = ifelse(is.na(NomeAnexos), NomeAnexos2, NomeAnexos)) %>%
  select(39, 5, 40, 42, 34, 33, 41, 42, 4, 14, 43, 35, 36, 3, 2, 37, 45, 10)
         

###deixei pedidos marcados com 1 em "contem dados pessoais" pois não continham dados pessoais.
###filtrei apenas pedidos de informação - incluir para análise Renata
###deixei apenas as linhas 'conteudo' diferentes de NA
  

write.xlsx(as.data.frame(leg), file="leg_templ.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

save(leg, file = "leg_templ.RData")



####PENDÊNCIAS! Arrumar data!


##Alternativa: nomeanexos

NomeAnexos = ifelse(interacao == "Pedido", anexo_com_extensao_pedido,
                    ifelse(interacao == "Resposta do Pedido", anexo_com_extensao_resposta,
                           ifelse(interacao == "Recurso - 1º Instância", anexo_com_extensao_recurso_1,
                                  ifelse(interacao == "Resposta do Recurso - 1º Instância", anexo_com_extensao_resposta_recurso_1, NA)))),
