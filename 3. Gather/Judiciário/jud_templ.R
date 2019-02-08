##Passando planilhas para o template

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\versões finais - template")

library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(xlsx)

###JUDICIÁRIO
############

##TEMPLATE

jud <- read_excel("C:/Users/Renato/Desktop/Colab classificados/versões finais - template/jud_final_11062018.xlsx")

jud2 <- jud %>%
  clean_names() %>%
  filter(is.na(nao_e_pedido_de_informacao)) %>%
  gather(interacao, conteudo, c(pedido, resposta, resposta_recurso_1, resposta_recurso_2)) %>%
  mutate(data = ifelse(interacao == "pedido", data_do_pedido, 
                       ifelse(interacao == "resposta", data_da_resposta,
                              ifelse(interacao == "resposta_recurso_1", data_resposta_recurso_1,
                                     data_resposta_recurso_2))),
         interacao = gsub("pedido", "Pedido", interacao),
         interacao = gsub("\\bresposta\\b", "Resposta do Pedido", interacao),
         interacao = gsub("resposta_recurso_1", "Resposta do Recurso - 1º Instância", interacao),
         interacao = gsub("resposta_recurso_2", "Resposta do Recurso - 2º Instância", interacao),
         orgao = gsub("conselho nacional de justica", "Conselho Nacional de Justiça", orgao),
         orgao = gsub("superior tribunal de justica", "Superior Tribunal de Justiça", orgao),
         orgao = gsub("tribunal de justica de pernambuco", "Tribunal de Justiça de Pernambuco", orgao),
         orgao = gsub("tribunal regional federal da terceira regiao", "Tribunal Regional Federal da 3ª Região", orgao),
         orgao = gsub("tribunal regional federal da segunda regiao", "Tribunal Regional Federal da 2ª Região", orgao),
         orgao = gsub("tribunal superior do trabalho", "Tribunal Superior do Trabalho", orgao),
         orgao = gsub("tribunal de justica de sao paulo", "Tribunal de Justiça de São Paulo", orgao),
         poder = gsub("judiciario", "Judiciário", poder),
         esfera = gsub("federal", "Federal", esfera),
         esfera = gsub("estadual", "Estadual", esfera),
         atendimento = ifelse(is.na(atendimento), "Não Classificado", atendimento),
         atendimento = gsub("Não classificado", "Não Classificado", atendimento),
         atendimento = gsub("não classificado", "Não Classificado", atendimento),
         atendimento = gsub("Não atendido", "Não Atendido", atendimento),
         id = NA,
         titulo = orgao,
         prorrogado = NA,
         situação = "Finalizado",
         uf = ifelse(orgao == "Tribunal de Justiça de Pernambuco", "PE", 
                     ifelse(orgao == "Tribunal de Justiça de São Paulo", "SP", NA)),
         município = NA,
         PastaAnexos = ifelse(interacao == "Pedido", pasta_do_anexo_pedido,
                              ifelse(interacao == "Resposta do Pedido", pasta_do_anexo_resposta,
                                    NA)),
         NomeAnexos = ifelse(interacao == "Pedido", anexo_com_extensao_pedido,
                             ifelse(interacao == "Resposta do Pedido", anexo_com_extensao_resposta,
                                    NA)),
         PastaAnexos =  gsub("[º-]", "", PastaAnexos),
         PastaAnexos = gsub(" ", "ñ", PastaAnexos),
         PastaAnexos = str_extract(PastaAnexos, "[:alnum:]*/[:alnum:]*$"),
         PastaAnexos = gsub("ñ", " ", PastaAnexos),
         PastaAnexos = gsub("stj", "STJ", PastaAnexos),
         PastaAnexos = gsub("Judiciário", "STJ", PastaAnexos)) %>%
  select(-novo_outros, -base_de_dados, -nao_e_pedido_de_informacao, -visto,
         -duplicado, -complementacao, -contem_dados_pessoais, -revisado, -visto1,
         -pasta_do_anexo_pedido, -data_do_pedido, -data_da_resposta,
         -pasta_do_anexo_resposta, -data_resposta_recurso_1, 
         -data_resposta_recurso_2, -anexo_com_extensao_pedido,
         -anexo_com_extensao_resposta) %>%
  rename(assunto = assunto_final_beta,
         status = atendimento,
         nivel = esfera) %>%
  select(10,4,11,8,7,9,12,3,6,13,14,15,2,1,16,17,5) %>%
  filter(!is.na(conteudo)) %>%
  mutate(PastaAnexos = ifelse(orgao == "Tribunal Regional Federal da 3ª Região"
                              & !is.na(NomeAnexos), "TRF3/", PastaAnexos))



####PENDÊNCIAS! Arrumar data!




##Arrumar datas de alguns recursos (JESSICA - em andamento! Única coisa que falta)
### Mudar nome das subpastas TJPE  OK!
### removidos os que não são pedidos de informação  OK!
### deixei na planilha os que estavam marcados como 1 na coluna contém dados pessoais,
#pois não continham dados pessoais - ao menos da maioria.   

###falta arrumar nome dos anexos estranhos e pastas. OK
#Ver se isso não foi problema ao empilhar.  OK!

write.xlsx(as.data.frame(jud2), file="jud_templ.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

save(jud2, file = "jud_templ.RData")
