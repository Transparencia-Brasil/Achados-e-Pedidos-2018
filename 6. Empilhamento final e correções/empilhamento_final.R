library(dplyr)
library(readxl)
library(readr)
library(googlesheets)
library(janitor)
library(xlsx)
library(stringr)
library(stringi)
library(tidyverse)

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)")

###juntando macrotemas e macroassuntos

gs_ls()

#macrotemas 

sheet1 <- gs_title("macrotemas_colab")
sheet2 <- gs_title("macroassuntos_colab")
macrotemas <- gs_read(sheet1) %>%
  mutate(Esfera = stringr::str_to_title(Esfera),
         Esfera = ifelse(Esfera == "Distrital", "Estadual", Esfera))

macroassunto <- gs_read(sheet2)

temassunto <- macrotemas %>%
  left_join(macroassunto) %>%
  mutate(macroassunto = ifelse(grepl("dúvidas do processo", Assunto), "Processos",
                               ifelse(grepl("SANEAMENTO - Solicitação", Assunto), "Saúde, saneamento e assistência social",
                                      ifelse(grepl("SAÚDE - Solicitação de serviços", Assunto), "Saúde, saneamento e assistência social",
                                             ifelse(grepl("EDUCAÇÃO -  Execução", Assunto), "Educação",
                                                    ifelse(grepl("HABITAÇÃO -  Orçamento", Assunto), "Orçamento", macroassunto))))))

##carregando poderes

##legislativo

load("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)\\leg_fixed.RData")

leg_final <- leg_fixed %>%
  mutate(esfera = ifelse(esfera == "Distrital", "Estadual", esfera)) %>%
  left_join(temassunto, by = c("poder" = "Poder", "esfera" = "Esfera",
                               "assunto_final_beta" = "Assunto")) %>%
  rename(assunto = assunto_final_beta) %>%
  filter(!is.na(assunto)) %>%
  clean_names() %>%
  mutate_all(as.character) %>%
  rename(nome_anexos = nome_anexos_f)

valid <- leg_final %>%
  group_by(macrotema, macroassunto, assunto) %>%
  summarise(cont = n())

##judiciario

load("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)\\jud_fix.Rdata")

jud_final <- jud3 %>%
  left_join(temassunto, by = c("poder" = "Poder", "nivel" = "Esfera",
                               "assunto" = "Assunto")) %>%
  filter(!is.na(assunto),
         !is.na(macrotema)) %>%
  clean_names() %>%
  mutate_all(as.character) %>%
  rename(esfera = nivel,
         atendimento = status) %>%
  mutate(macroassunto = ifelse(grepl("_dúvidas do processo", assunto), "Processos", macroassunto))

valid <- jud_final %>%
  group_by(macrotema, macroassunto, assunto) %>%
  summarise(cont = n())

##tcs

###planilha com categorias certas
tcsf_fix2 <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/tcs_templ.xlsx")

###planilha com datas certas
load("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)\\tcsf_fix.Rdata")

###arrumando datas TCs

# selecionando colunas para o join e criando id de controle

tc_datas <- tcsf_fix %>%
  select(protocolo, interacao, orgao, data) %>%
  mutate(n = 1:n())

# eliminando duplicatas para evitar multiplicação das duplicatas na etapa seguinte

tc_datas <- tc_datas[!duplicated(tc_datas[c(1:4)]),]

#dando join

tc_categ <- tcsf_fix2 %>%
  left_join(tc_datas, by = c("protocolo", "orgao", "interacao")) %>%
  mutate(data.x = data.y) %>%
  rename(data = data.x) %>%
  select(-c(data.y, n))

tc_final <- tc_categ %>%
  left_join(temassunto, by = c("poder" = "Poder", "esfera" = "Esfera",
                               "assunto" = "Assunto")) %>%
  filter(!is.na(assunto)) %>%
  clean_names() %>%
  mutate_all(as.character) %>%
  rename(pasta_anexos = pasta_anexos_f,
         nome_anexos = nome_anexos_f) %>%
  mutate(poder = gsub("Tribunal de Contas", "Tribunais de Contas", poder),
         esfera = ifelse(orgao == "Tribunal de Contas dos Municípios do Ceará",
                         "Estadual", esfera),
         macrotema = ifelse(assunto == "2. certidões", 
                            "serviços e impostos", macrotema)) %>%
  distinct(protocolo, conteudo, .keep_all = T)

valid <- tc_final %>%
  group_by(orgao) %>%
  summarise(cont = n())

valid2 <- tc_final %>%
  group_by(orgao) %>%
  summarise(cont = n())


##mp

load("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)\\mp_fix.RData")

mp_final <- mp_fix  %>%
  mutate(assunto_final_beta = gsub("Processos", "processos", assunto_final_beta)) %>%
  left_join(temassunto, by = c("poder" = "Poder", "esfera" = "Esfera",
                               "assunto_final_beta" = "Assunto")) %>%
  rename(assunto = assunto_final_beta) %>%
  filter(!is.na(assunto)) %>%
  clean_names() %>%
  mutate_all(as.character) %>%
  mutate(macroassunto = ifelse(grepl("_verba servidores", assunto), "Servidores e verbas", macroassunto),
         macroassunto = ifelse(grepl("\\bProcessos\\b", macroassunto), "Processos e fiscalização externa", macroassunto))

valid <- mp_final %>%
  group_by(macrotema, macroassunto, assunto) %>%
  summarise(cont = n())


##execum

load("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)\\execum_fixed.RData")

excum_final <- ind  %>%
  left_join(temassunto, by = c("poder" = "Poder", "esfera" = "Esfera",
                               "assunto" = "Assunto")) %>%
  filter(!is.na(assunto)) %>%
  clean_names() %>%
  mutate_all(as.character) %>%
  mutate(macroassunto = ifelse(grepl("8.4 EDUCAÇÃO", assunto), "Educação", macroassunto),
         macroassunto = ifelse(grepl("9.3 HABITAÇÃO", assunto), "Habitação, desapropriações e reforma agrária", macroassunto))

###encontrando datas com problemas
###########################
# datap <- exec_aep %>%
#   select(protocolo, interacao, data) %>%
#   filter(interacao == "Pedido")
# 
# datar <- exec_aep %>%
#   select(protocolo, interacao, data) %>%
#   filter(interacao == "Resposta do Pedido")
# 
# diff <- datap %>%
#   left_join(datar, by = c("protocolo")) %>%
#   mutate(data.y = as.Date(data.y),
#          data.x = as.Date(data.x)) %>%
#   mutate(diff = difftime(data.y, data.x, units = c("days")))
###########################

###protocolos com datas problemáticas

prot <- c("75000077201681", "75000052201769","75000079201751", "E-SIC 131310000062015-28", 
          "13131000003/2016-75", "75000030201707", "75000065201657")


##corrigindo datas problemáticas

excum_final <- excum_final %>%
  mutate(data = ifelse(interacao == "Resposta do Pedido" & protocolo == "75000077201681", "2016-11-08", data),
         data = ifelse(interacao == "Resposta do Pedido" & protocolo == "75000052201769", "2017-05-17", data),
         data = ifelse(interacao == "Resposta do Pedido" & protocolo == "E-SIC 131310000062015-28", "2015-01-20", data),
         data = ifelse(interacao == "Resposta do Pedido" & protocolo == "13131000003/2016-75", "2016-03-02", data),
         data = ifelse(interacao == "Resposta do Pedido" & protocolo == "75000030201707", "2017-07-14", data),
         data = ifelse(interacao == "Resposta do Pedido" & protocolo == "75000065201657", "2016-10-22", data),
         data = ifelse(interacao == "Resposta do Pedido" & protocolo == "75000079201751", "2017-06-26", data))

##execest

load("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)\\execest_fixed.Rdata")

execest_final <- execest_fixed  %>%
  left_join(temassunto, by = c("poder" = "Poder", "esfera" = "Esfera",
                               "assunto" = "Assunto")) %>%
  filter(!is.na(assunto)) %>%
  clean_names() %>%
  mutate_all(as.character)

##exefed

load("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)\\exefed_templ.RData")

##Necessidade de fazer as seguintes correções (não consegui):
##Se órgão == INSS | SUSEP, o assunto "3.3 INSS, previdência e seguros" foi transformado em "4.2 Execução",
#o macroassunto em "INSS, previdência e seguros" e o macrotema em "atividade-fim".

##ATUALIZAR LISTA DE MACROASSUNTOS PARA O ANEXO DO RELATÓRIO,
#DESTRINCHANDO O VARIADOS - EXECUÇÃO.


###agrupamentos órgãos CGU, com ajuste no encoding (Manoel) para evitar alguns problemas no gsub

assuntos <- c("4.2 Execução","4.99 Outros")

educ <- c("CAPES – Coordenação de Aperfeiçoamento de Pessoal de Nível Superior", "universidades",
       "CNPQ – Conselho Nacional de Desenvolvimento Científico e Tecnológico", "escolas técnicas e CPII",
       "FNDE – Fundo Nacional de Desenvolvimento da Educação", "MEC – Ministério da Educação") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

saude <- c("hospitais", "MS – Ministério da Saúde") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")


cult <- c("ME – Ministério do Esporte", "MinC – Ministério da Cultura") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

seguranca <- c("DEPEN – Departamento Penitenciário Nacional", "DPF – Departamento de Polícia Federal",
               "DPRF – Departamento de Polícia Rodoviária Federal") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

meio_ambiente <- c("MMA – Ministério do Meio Ambiente") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")


mob <- c("DNIT/MT – Departamento Nacional de Infraestrutura de Transportes",
         "MTPA – Ministério dos Transportes, Portos e Aviação Civil") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

defesa <- c("CMAR – Comando da Marinha", "COMAER – Comando da Aeronáutica",
            "MD – Ministério da Defesa") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

bancos <- c("bancos") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

economia <- c("MF – Ministério da Fazenda", "MP – Ministério do Planejamento, Desenvolvimento e Gestão") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

agencias <- c("agencia") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

inss <- c("INSS – Instituto Nacional do Seguro Social", "SUSEP – Superintendência de Seguros Privados",
          "PREVIC – Superintendência Nacional de Previdência Complementar") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

estatais <- "estatal" %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

institutos <- "institutos" %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")

assuntos_inss <- c("3.3 INSS, previdência e seguros", "4.2 Execução") %>%
  iconv(., from="windows-1252",  to= "ASCII//TRANSLIT")
# 
# ####ministerios variados (expandir)
# ###########
                                                        
# [2] "AGU – Advocacia-Geral da União"                                      
# [3] "AN – Arquivo Nacional"                                               
# [6] "CC-PR – Casa Civil da Presidência da República"                      
# [7] "CGU – Ministério da Transparência e Controladoria-Geral da União"    
# [15] "DPU – Defensoria Pública da União"                                   
# [19] "FUNAI – Fundação Nacional do Índio"                                  
# [20] "Fundações menores"                                                   
# [23] "MAPA – Ministério da Agricultura, Pecuária e Abastecimento"          
# [24] "MCIDADES – Ministério das Cidades"                                   
# [25] "MCTIC – Ministério da Ciência, Tecnologia, Inovações e Comunicações" 
# [27] "MDIC - Ministério da Indústria, Comércio Exterior e Serviços"        
# [28] "MDS – Ministério do Desenvolvimento Social"                          
# [32] "MI – Ministério da Integração Nacional"                              
# [34] "Ministérios menores"                                                 
# [35] "MJ – Ministério da Justiça"                                          
# [37] "MME – Ministério de Minas e Energia"                                 
# [39] "MRE – Ministério das Relações Exteriores"                            
# [41] "MT – Ministério do Trabalho"                                         
# [43] "outros"                                                              
# [44] "Secretarias menores"                                                 
# [45] "SERPRO – Serviço Federal de Processamento de Dados"                  
# [46] "SGPR – Secretaria-Geral da Presidência da República"                 
# [47] "superintendencias"                                                   
# ########

exefed_final <- cgu  %>%
  left_join(temassunto, by = c("poder" = "Poder", "esfera" = "Esfera",
                               "assunto" = "Assunto")) %>%
  filter(!is.na(assunto)) %>%
  clean_names() %>%
  mutate_all(as.character) %>%
  mutate(tipo_destino1 = iconv(tipo_destino, from="UTF-8",  to= "ASCII//TRANSLIT"),
         destino1 = iconv(destino, from="UTF-8",  to= "ASCII//TRANSLIT"),
         macroassunto2 = ifelse(tipo_destino1 %in% educ, "Educação", 
                                ifelse(tipo_destino1 %in% saude, "Saúde, saneamento e assistência social",
                                       ifelse(tipo_destino1 %in% cult, "Esporte, cultura e lazer",
                                              ifelse(tipo_destino1 %in% seguranca, "Segurança pública",
                                                     ifelse(tipo_destino1 %in% meio_ambiente, "Meio ambiente",
                                                            ifelse(tipo_destino1 %in% mob, "Trânsito e mobilidade",
                                                                   ifelse(tipo_destino1 %in% defesa, "Defesa",
                                                                          ifelse(tipo_destino1 %in% bancos, "Bancos",
                                                                                 ifelse(tipo_destino1 %in% agencias, "Agências",
                                                                                        ifelse(tipo_destino1 %in% economia, "Economia",
                                                                                               ifelse(tipo_destino1 %in% estatais, "Estatais",
                                                                                                      ifelse(destino1 == "institutos" & tipo_destino1 %in% institutos, "Institutos federais", 
                                                                                                             ifelse(tipo_destino1 == "institutos" & destino1 %in% inss, "INSS, previdência e seguros", NA))))))))))))),
         macroassunto3 = ifelse(assunto %in% c("4.2 Execução","4.99 Outros") & !is.na(macroassunto2), macroassunto2, NA),
         macroassunto = ifelse(!is.na(macroassunto3), macroassunto3, macroassunto)) %>%
  select(-c(macroassunto2, macroassunto3, tipo_destino1, destino1))

exefed_final <- exefed_final %>%
  mutate(assunto = ifelse(destino %in% inss & assunto %in% "3.5 Emissão e regularização de documentos pessoais",
                          "4.2 Execução", assunto))

##ajustando nomes
#################
    # 
# teste_igualdade_nomes_var_df <- function(base1, base2) {
#   
#   x <- names(base1)
#   y <- names(base2)
#   n <- length(x)
#   k <- length(y)
#   
#   teste_nome_igual_x <- numeric()
#   teste_nome_igual_y <- numeric()
#   
#   for ( i in 1:n) {
#     teste_nome_igual_x[i] <- x[i] %in% y
#   }
#   
#   for ( i in 1:k) {
#     teste_nome_igual_y[i] <- y[i] %in% x
#   }
#   resp_x <- paste(x[!as.logical(teste_nome_igual_x)], collapse = ", ")
#   resp_y <- paste(y[!as.logical(teste_nome_igual_y)], collapse = ", ")
#   
#   print(paste("as variáveis em x que não estão em y, são:", resp_x,
#               ". E as variáveris de y que não estão em x, são:", resp_y,
#               sep=" "))
#   
# }
# teste_igualdade_nomes_var_df(leg_final, tc_final)
# 
##################
  
##empilhando

emp <- leg_final %>%
  bind_rows(jud_final, tc_final, mp_final, excum_final, execest_final, exefed_final) %>%
  select(-c(x6, fix, pedidos)) %>%
  filter(!is.na(atendimento)) %>%
  filter(!is.na(assunto)) %>%
  filter(!is.na(macroassunto)) %>%
  filter(!is.na(macrotema)) %>%
  rename(assunto = macroassunto,
         subassunto = assunto) %>%
  mutate(conteudo = ifelse(orgao == "Governo do Estado de Minas Gerais",
                            ifelse(is.na(nome_anexos), conteudo, paste(conteudo, nome_anexos, sep = ", \t")), conteudo),
         nome_anexos2 = gsub("; ", ";", nome_anexos)) %>%
  mutate(nome_anexos3 = gsub(";", "|", nome_anexos2)) %>%
  mutate(nome_anexos = gsub(" ", "_", nome_anexos3)) %>%
  select(-c(nome_anexos2, nome_anexos3)) %>%
  mutate(dataf = "%Y-%m-%d")

valid<- emp %>%
  filter(!is.na(nome_anexos)) %>%
  select(nome_anexos)

### arrumando atendimentos e classificacoes incoerentes

prot_NC <- c("75/2017","720/2016","1007201612537975","1	000820201667",
          "07112016142929696","03042017153530747", "60502001859201763",
          "60502001412201794", "01122015132645529", "1    000087201508",
          "1    000488201631", "1    000659201621", "1014/2016",
          "23480011085201758", "23480022794201769", "23480022694201732",
          "23480022153201712", "23480021915201755", "1390001093201762",
          "16853002427201706", "16853006443201760", "18600001863201757")

prot_PA <- c("60502001412201794", "040417KE1205E", "23480020480201721",
             "23480012572201738", "23480002031201700", "18600002811201706",
             "23480015394201705")

prot_A <- c("1    000012201519", "126/2017", "23480023039201700", "23480019542201752",
            "23480017138201744", "23480009927201710", "23480002448201764",
            "23480001687201705", "23480006330201713", "16853002309201790")

emp <- emp %>%
  mutate(atendimento = ifelse(protocolo %in% prot_NC, "Não Classificado",
                              ifelse(protocolo %in% prot_PA, "Parcialmente Atendido",
                                     ifelse(protocolo %in% prot_A, "Atendido", atendimento))))


########AJUSTES PARA ADEQUAR PLANILHA PARA SER INSERIDA NO ACHADOS E PEDIDOS

###ajustes Manoel: 1) substituindo espaço por underline no nome dos arquivos reais; 2) retirando barra
## do final do caminho dos anexos; 3) criando datas fictícias.

##################
# 1) substituir espaços por underline nos nomes dos arquivos reais. Essa substituição NAO deve ser feita
# no nome das pastas. É para fazer só no nome dos arquivos! (FEITO!)
# 
# setwd("C:\\Users\\Renato\\Desktop\\FTP - Copia\\Anexos dos órgãos")
# lista <- list.files(recursive = T)
# lista_nova <- gsub(" (?!.*/.*)", "_",lista, perl = TRUE)
# lista_caminhos <- list.dirs(full.names = F)
# lista_caminhos_arquivos <- list.files(recursive = T)
# lista_split <- str_split(lista_caminhos_arquivos, "/")
# 
# sapply(lista_split, tail, 1)
# 
# file.rename(to = lista_nova, from = lista)

# 2) retirando barra do final dos caminhos inseridos na coluna pasta_anexos.

emp <- emp %>%
  mutate(pasta_anexos = gsub("/$", "", pasta_anexos),
         nome_anexos2 = nome_anexos,
         nome_anexos2 = ifelse(grepl("SIM_", nome_anexos2), gsub(".*P", "", nome_anexos2), nome_anexos2),
         nome_anexos2 = ifelse(grepl("SIM_", nome_anexos), paste0("p", nome_anexos2), nome_anexos2),
         nome_anexos2 = ifelse(grepl("SIM_", nome_anexos), paste0(nome_anexos2, ".pdf"), nome_anexos2),
         nome_anexos2 = ifelse(grepl("_{5}", nome_anexos2), paste0(nome_anexos2, ".pdf"), nome_anexos2),
         nome_anexos2 = gsub("_{5,}", ".pdf | ", nome_anexos2),
         nome_anexos2 = gsub("|$", "", nome_anexos2),
         extensao = ifelse(grepl("\\.", nome_anexos2) &
                             !grepl("http://", nome_anexos2), gsub(".*\\.","", nome_anexos2), NA),
         nome_anexos2 = ifelse(is.na(extensao) &
                                 !grepl("http://", nome_anexos2), paste0(nome_anexos2, ".pdf"), nome_anexos2)) %>%
  mutate(nome_anexos2 = ifelse(nome_anexos2 == "NA.pdf", NA, nome_anexos2)) %>%
  select(-nome_anexos) %>%
  rename(nome_anexos = nome_anexos2)

valid <- emp %>%
  filter(!is.na(pasta_anexos)) %>%
  select(pasta_anexos, nome_anexos)

##trocando alguns nomes

emp <- emp %>%
  mutate(assunto = ifelse(assunto == "Variados - Execução", "Políticas públicas diversas",
                          assunto),
         assunto = ifelse(assunto == "Dados, pesquisas sobre processos e bases CNJ", "Bases de dados e levantamentos",
                          assunto),
         assunto = ifelse(assunto == "Processos indeterminados", 
                          "Processos administrativos indeterminados", assunto),
         assunto = ifelse(assunto == "LAI como RH", 
                          "Consultas de RH", assunto),
         assunto = ifelse(assunto == "Orçamento", 
                          "Orçamento, receitas e despesas", assunto),
         macrotema = ifelse(macrotema == "controle social", "Controle social", 
                            ifelse(macrotema == "informação básica", "Informação básica",
                                   ifelse(macrotema == "atividade-fim", "Atividade-fim",
                                          ifelse(macrotema == "outros", "Outros",
                                                 ifelse(macrotema == "admissão", "Admissão",
                                                        ifelse(macrotema == "serviços e impostos", "Serviços e impostos",
                                                               ifelse(macrotema == "processos", "Processos", macrotema))))))))





emp <- emp %>%
  mutate(nome_anexos = gsub("Pedido_de_informação_", ":;:;:;Pedido_de_informação_", nome_anexos),
         nome_anexos = gsub(".*:;:;:;","",nome_anexos))

valid <- emp %>%
  group_by(assunto) %>%
  summarise(cont = n())

valid2 <- emp %>%
  group_by(macrotema) %>%
  summarise(cont = n())


## 3) criando datas ficticias

# Muitos pedidos não possuem datas. Queremos inserir datas fictícias que não distorçam as visualizacoes
# no dashboard. Para que isso ocorra, algumas coisas são importantes:
# 
# a) as datas criadas devem respeitar o tempo médio de resposta;
#     a1) Tirar a média para resposta do pedido e para resposta do recurso. Se tirarmos só uma média
#     geral e criar datas fictícias baseados só nisso, corremos o risco de distorcer muito o tempo
#     médio de resposta dos recursos.
# 
# b) devemos respeitar a proporção de pedidos por ano. Por exemplo: as datas criadas não podem ser
# todas de 2013, porque senão vamos distorcer o gráfico de pedidos por ano.

# emp1 <- emp %>%
#   mutate(data = as.Date(data, format= "%Y-%m-%d")) %>%
#   group_by(protocolo) %>%
#   mutate(min_data = min(data),
#          bol_imputar_data = as.numeric(is.na(data)),
#          dif_datas = max(data) - min_data,
#          data1 = if_else(is.na(data), min_data, data),
#          min_ano = format(min_data, "%Y")) %>%
#   ungroup()

# distrib_pedidos_ano <- emp1 %>%
#   group_by(protocolo) %>%
#   summarise(min_ano = min(min_ano, na.rm=T)) %>%
#   group_by(min_ano) %>%
#   summarise(total = n())

# distrib_pedidos_ano <-  distrib_pedidos_ano %>%
#   filter(!is.na(min_ano)) %>%
#   mutate(perc = total/sum(total))


# emp2 <- emp1 %>%
#   rowwise() %>%
#   mutate(aux_ano = ifelse(is.na(data1) && interacao == "Pedido", 
#                                sample(2012:2017, size=1, prob=distrib_pedidos_ano$perc),
#                                NA)) %>%
#   mutate(data2 = ifelse(is.na(aux_ano), NA, paste0(aux_ano, "-01-01")),
#          data2 = as.Date(data2))

# emp2 <- emp2 %>%
#   mutate(data_final = if_else(is.na(data), data2, data))
# removendo protocolo NA e protocolo de pedido duplicado

emp1 <- emp %>%
  mutate(data = as.Date(data, format= "%Y-%m-%d")) %>%
  mutate(bol_imputar_data = as.numeric(is.na(data))) %>%
  filter(!is.na(protocolo)) %>%
  mutate(dup1 = duplicated(paste0(protocolo, interacao)),
         dup2 = duplicated(paste0(protocolo, interacao), fromLast = T)) %>%
  filter(!dup1) %>%
  filter(!dup2)


emp2_pedidos <- emp1 %>%
  mutate(data = as.Date(data, format= "%Y-%m-%d")) %>%
  filter(interacao == "Pedido") %>%
  mutate(bol_pedido = 1)


emp2_resp <- emp1 %>%
  filter(interacao == "Resposta do Pedido") %>%
  mutate(bol_resp = 1)

emp2_rec1 <- emp1 %>%
  filter(interacao == "Recurso - 1º Instância") %>%
  mutate(bol_rec1 = 1)

emp2_resp_rec1 <- emp1 %>%
  filter(interacao == "Resposta do Recurso - 1º Instância") %>%
  mutate(bol_resp_rec1 = 1)

emp2_rec2 <- emp1 %>%
  filter(interacao == "Recurso - 2º Instância") %>%
  mutate(bol_rec2 = 1)

emp2_resp_rec2 <- emp1 %>%
  filter(interacao == "Resposta do Recurso - 2º Instância") %>%
  mutate(bol_resp_rec2 = 1)

emp2_rec3 <- emp1 %>%
  filter(interacao == "Recurso - 3º Instância") %>%
  mutate(bol_rec3 = 1)

emp2_resp_rec3 <- emp1 %>%
  filter(interacao == "Resposta do Recurso - 3º Instância") %>%
  mutate(bol_resp_rec3 = 1)

emp_spread <- emp2_pedidos %>%
  left_join(select(emp2_resp, protocolo, data, bol_resp), by = "protocolo") %>%
  left_join(select(emp2_rec1, protocolo, data, bol_rec1), by = "protocolo") %>%
  left_join(select(emp2_resp_rec1, protocolo, data, bol_resp_rec1), by = "protocolo") %>%
  left_join(select(emp2_rec2, protocolo, data, bol_rec2), by = "protocolo") %>%
  left_join(select(emp2_resp_rec2, protocolo, data, bol_resp_rec2), by = "protocolo") %>%
  left_join(select(emp2_rec3, protocolo, data, bol_rec3), by = "protocolo") %>%
  left_join(select(emp2_resp_rec3, protocolo, data, bol_resp_rec3), by = "protocolo") %>%
  rename(data_pedido = data.x, data_resposta = data.y,
         data_recurso1 = data.x.x, data_resp_rec1 = data.y.y,
         data_recurso2 = data.x.x.x, data_resp_rec2 = data.y.y.y,
         data_recurso3 = data.x.x.x.x, data_resp_rec3 = data.y.y.y.y)

tempo_resposta <- emp_spread %>%
  ungroup() %>%
  filter(bol_imputar_data == 0) %>%
  mutate(ano_pedido = format(data_pedido, "%Y"),
         dif_pedido_resposta = as.numeric(data_resposta - data_pedido)) %>%
  group_by(ano_pedido) %>%
  summarise(tempo_medio_pedido_resposta = mean(dif_pedido_resposta, na.rm=T), num_ped = n()) %>%
  mutate(total = sum(num_ped))

## imputando data para os casos em que há data da resposta, mas não do pedido.
emp_spread1 <- emp_spread  %>%
  ungroup() %>%
  mutate(ano_resposta = format(data_resposta, "%Y"),
         data_pedido = if_else(is.na(data_pedido) & !is.na(data_resposta),
                               if_else(ano_resposta == 2012, data_resposta - 14,
                                       if_else(ano_resposta == 2013, data_resposta - 23,
                                               if_else(ano_resposta == 2014, data_resposta - 18,
                                                       if_else(ano_resposta == 2015, data_resposta -23,
                                                               if_else(ano_resposta == 2016, data_resposta - 15, data_resposta - 13)))),
                                       data_pedido), data_pedido))


#3 mudando ordem da coluna, pra View ficar mais fácil de ver
ordem_coluna <- c(1:6,8:26, 7, 27:ncol(emp_spread1))
emp_spread1 <- emp_spread1[, ordem_coluna]


# distrib pedidos
distrib_pedidos_ano <- emp_spread1 %>%
  mutate(ano_pedido = format(data_pedido, "%Y")) %>%
  group_by(ano_pedido) %>%
  summarise(total = n())

distrib_pedidos_ano <-  distrib_pedidos_ano %>%
  filter(!is.na(ano_pedido)) %>%
  mutate(perc = total/sum(total))


# distrib recurso deu 11, vou colocar 10
emp_spread1 %>%
  mutate(ano_recurso = format(data_recurso1, "%Y"),
         dif = as.numeric(data_recurso1 - data_resposta)) %>%
  filter(!is.na(ano_recurso)) %>%
  summarise(mean(dif, na.rm=T))



## preenchendo pedido
emp_spread2 <- emp_spread1 %>%
  rowwise() %>%
  mutate(aux_ano = NA,
         aux_ano = ifelse(is.na(data_pedido),
                          sample(2012:2017, size=1, prob=distrib_pedidos_ano$perc),
                          NA)) %>%
  ungroup() %>%
  mutate(data_aux = ifelse(is.na(aux_ano), NA, paste0(aux_ano, "-01-01")),
         data_pedido = if_else(is.na(aux_ano), data_pedido, as.Date(data_aux)))


## preenchendo resposta
emp_spread2 <- emp_spread2 %>%
  mutate(ano_pedido = format(data_pedido, "%Y")) %>%
  group_by() %>%
  mutate(data_resposta = if_else(is.na(data_resposta) & bol_resp == 1,
                                 if_else(ano_pedido == 2012, data_pedido + 81,
                                         if_else(ano_pedido == 2013, data_pedido + 60,
                                                 if_else(ano_pedido == 2014, data_pedido + 53,
                                                         if_else(ano_pedido == 2015, data_pedido + 21,
                                                                 if_else(ano_pedido == 2016, data_pedido + 7, data_pedido + 5)))),
                                         data_resposta), data_resposta))



## preenchendo pedido recurso
emp_spread2 <- emp_spread2 %>%
  mutate(ano_pedido = format(data_pedido, "%Y")) %>%
  group_by() %>%
  mutate(data_recurso1 = if_else(is.na(data_recurso1) & bol_rec1 == 1,
                                 if_else(ano_pedido == 2012, data_resposta + 10,
                                         if_else(ano_pedido == 2013, data_resposta + 10,
                                                 if_else(ano_pedido == 2014, data_resposta + 10,
                                                         if_else(ano_pedido == 2015, data_resposta + 10,
                                                                 if_else(ano_pedido == 2016, data_resposta + 10, data_resposta + 10)))),
                                         data_recurso1), data_recurso1))

## preenchendo resposta recurso 1
emp_spread2 <- emp_spread2 %>%
  mutate(ano_pedido = format(data_pedido, "%Y")) %>%
  group_by() %>%
  mutate(data_resp_rec1 = if_else(is.na(data_resp_rec1) & bol_resp_rec1 == 1,
                                  if_else(ano_pedido == 2012, data_recurso1 + 5,
                                          if_else(ano_pedido == 2013, data_recurso1 + 5,
                                                  if_else(ano_pedido == 2014, data_recurso1 + 5,
                                                          if_else(ano_pedido == 2015, data_recurso1 + 5,
                                                                  if_else(ano_pedido == 2016, data_recurso1 + 5, data_recurso1 + 5)))),
                                          data_resp_rec1), data_resp_rec1))



## preenchendo pedido recurso 2
emp_spread2 <- emp_spread2 %>%
  mutate(ano_pedido = format(data_pedido, "%Y")) %>%
  group_by() %>%
  mutate(data_recurso2 = if_else(is.na(data_recurso2) & bol_rec2 == 1,
                                 if_else(ano_pedido == 2012, data_resp_rec1 + 10,
                                         if_else(ano_pedido == 2013, data_resp_rec1 + 10,
                                                 if_else(ano_pedido == 2014, data_resp_rec1 + 10,
                                                         if_else(ano_pedido == 2015, data_resp_rec1 + 10,
                                                                 if_else(ano_pedido == 2016, data_resp_rec1 + 10, data_resp_rec1 + 10)))),
                                         data_recurso2), data_recurso2))

## preenchendo resposta recurso 2
emp_spread2 <- emp_spread2 %>%
  mutate(ano_pedido = format(data_pedido, "%Y")) %>%
  group_by() %>%
  mutate(data_resp_rec2 = if_else(is.na(data_resp_rec2) & bol_resp_rec2 == 1,
                                  if_else(ano_pedido == 2012, data_recurso2 + 5,
                                          if_else(ano_pedido == 2013, data_recurso2 + 5,
                                                  if_else(ano_pedido == 2014, data_recurso2 + 5,
                                                          if_else(ano_pedido == 2015, data_recurso2 + 5,
                                                                  if_else(ano_pedido == 2016, data_recurso2 + 5, data_recurso2 + 5)))),
                                          data_resp_rec2), data_resp_rec2))



## preenchendo pedido recurso 3
emp_spread2 <- emp_spread2 %>%
  mutate(ano_pedido = format(data_pedido, "%Y")) %>%
  group_by() %>%
  mutate(data_recurso3 = if_else(is.na(data_recurso3) & bol_rec3 == 1,
                                 if_else(ano_pedido == 2012, data_resp_rec2 + 10,
                                         if_else(ano_pedido == 2013, data_resp_rec2 + 10,
                                                 if_else(ano_pedido == 2014, data_resp_rec2 + 10,
                                                         if_else(ano_pedido == 2015, data_resp_rec2 + 10,
                                                                 if_else(ano_pedido == 2016, data_resp_rec2 + 10, data_resp_rec2 + 10)))),
                                         data_recurso3), data_recurso3))

## preenchendo resposta recurso 3
emp_spread2 <- emp_spread2 %>%
  mutate(ano_pedido = format(data_pedido, "%Y")) %>%
  group_by() %>%
  mutate(data_resp_rec3 = if_else(is.na(data_resp_rec3) & bol_resp_rec3 == 1,
                                  if_else(ano_pedido == 2012, data_recurso3 + 5,
                                          if_else(ano_pedido == 2013, data_recurso3 + 5,
                                                  if_else(ano_pedido == 2014, data_recurso3 + 5,
                                                          if_else(ano_pedido == 2015, data_recurso3 + 5,
                                                                  if_else(ano_pedido == 2016, data_recurso3 + 5, data_recurso3 + 5)))),
                                          data_resp_rec3), data_resp_rec3))

View(emp_spread2)

emp_spread3 <- emp_spread2 %>%
  select(protocolo, data_pedido, data_resposta, data_recurso1, data_resp_rec1, 
         data_recurso2, data_resp_rec2, data_recurso3, data_resp_rec3)

vec_nome <- names(emp_spread3)
vec_nome <- gsub("data_", "", vec_nome)
names(emp_spread3) <- vec_nome

emp_gather <- emp_spread3 %>%
  gather(interacao, data, -protocolo) %>%
  filter(!is.na(data))

# rápida validação. Números devem estar próximos
emp_gather %>%
  group_by(interacao) %>%
  summarise(n())

emp %>%
  group_by(interacao) %>%
  summarise(n())

# deveria bater
# perdi respostas e recursos e respostas em 1a instância.
emp1 %>%
  group_by(interacao) %>%
  summarise(n())

# validei. Há respostas sem pedidos (segundo o protocolo. Ex. protoclo: 900.000.122.015)

emp_final <- emp %>%
  mutate(interacao_alt = interacao,
         interacao_alt = gsub("Pedido", "pedido", interacao_alt),
         interacao_alt = gsub("Resposta do pedido", "resposta", interacao_alt),
         interacao_alt = gsub("Recurso - 1º Instância", "recurso1", interacao_alt),
         interacao_alt = gsub("Resposta do recurso1", "resp_rec1", interacao_alt),
         interacao_alt = gsub("Recurso - 2º Instância", "recurso2", interacao_alt),
         interacao_alt = gsub("Resposta do recurso2", "resp_rec2", interacao_alt),
         interacao_alt = gsub("Recurso - 3º Instância", "recurso3", interacao_alt),
         interacao_alt = gsub("Resposta do recurso3", "resp_rec3", interacao_alt)) %>%
  inner_join(emp_gather, by = c("protocolo", "interacao_alt"="interacao"))

dim(emp_final)
emp_final <- emp_final %>%
  select(-data.x) %>%
  rename(data = data.y)

# rápida validação

emp_final %>%
  group_by(interacao) %>%
  summarise(n())

val <- emp_final %>%
  select(protocolo, data, interacao_alt) %>%
  spread(key=interacao_alt, value=data) %>%
  mutate(ano = format(pedido, "%Y"),
         dif = as.numeric(resposta - pedido)) %>%
  group_by(ano) %>%
  summarise(mean(dif, na.rm=T))

emp_final <- emp_final %>%
  select(-interacao_alt)
  
###corrigindo o nome de alguns anexos


valid <- emp_final %>%
  filter(!is.na(nome_anexos)) %>%
  select(nome_anexos)

save(valid, file = "valid.RData")
  
save(emp_final, file="emp_data_corrigida.RData")

#########################

##SEM DATA FICTICIA
save(emp, file = "emp.RData")

##COM DATA FICTICIA
save(emp_final, file = "emp_data_ficticia.RData")

write.xlsx(as.data.frame(emp), file="poderes_empilhados_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)


###versao para publicacao com relatorio

emp_pub <- emp %>%
  mutate(tem_anexo = ifelse(is.na(nome_anexos), NA, "Sim")) %>%
  select(-c(id, titulo, prorrogado, situacao, pasta_anexos, nome_anexos, subassunto, dataf))

write.xlsx(as.data.frame(emp_pub), file="emp_pub_03122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

save(emp_pub, file = "emp_pub_03122018.RData")
