library(dplyr)
library(readxl)
library(readr)
library(googlesheets)
library(janitor)
library(xlsx)
library(tidyverse)

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)")

##carregando todos os poderes
load(file = "emp_data_ficticia.RData")
emp <- emp_final

##retirando pedidos sem atendimento e sem data; organizando no template

emp_data <- emp %>%
  mutate(data = as.character(data)) %>%
  filter(!is.na(data),
         !is.na(atendimento),
         !is.na(protocolo)) %>%
  select(id, protocolo, titulo, conteudo, interacao, data, prorrogado,
         orgao, atendimento, situacao, uf, municipio, poder, esfera, pasta_anexos,
         nome_anexos, dataf)

#legislativo

leg_aep <- emp_data %>%
  filter(poder == "Legislativo") %>%
  arrange(protocolo)
  
qde <- leg_aep %>%
  group_by(interacao) %>%
  summarise(qde = n())

leg_aep1 <- leg_aep %>% slice(1:1000)
leg_aep2 <- leg_aep %>% slice(1001:2070)

write.xlsx(as.data.frame(leg_aep1), file="leg_aep1_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(leg_aep2), file="leg_aep2_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)


##judiciário

jud_aep <- emp_data %>%
  filter(poder == "Judiciário") %>%
  arrange(protocolo)

qde <- jud_aep %>%
  group_by(interacao) %>%
  summarise(qde = n())

jud_aep1 <- jud_aep %>% slice(1:1000)
jud_aep2 <- jud_aep %>% slice(1001:2000)
jud_aep3 <- jud_aep %>% slice(2001:2566)

write.xlsx(as.data.frame(jud_aep1), file="jud_aep1_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(jud_aep2), file="jud_aep2_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(jud_aep3), file="jud_aep3_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)


##ministério público

mp_aep <- emp_data %>%
  filter(poder == "Ministério Público") %>%
  arrange(protocolo)

qde <- mp_aep %>%
  group_by(interacao) %>%
  summarise(qde = n())

write.xlsx(as.data.frame(mp_aep), file="mp_aep_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

##executivo

###select na coluna destino

emp_data_cgu <- emp %>%
  mutate(data = as.character(data)) %>%
  filter(!is.na(data),
         !is.na(atendimento),
         !is.na(protocolo)) %>%
  select(id, protocolo, titulo, conteudo, interacao, data, prorrogado,
         orgao, atendimento, situacao, uf, municipio, poder, esfera, pasta_anexos,
         nome_anexos, destino, dataf)

exec_aep <- emp_data_cgu %>%
  filter(poder == "Executivo") %>%
  arrange(protocolo) %>%
  mutate(orgao = ifelse(esfera == "Federal", destino, orgao)) %>%
  select(-destino) %>%
  mutate(nome_anexos = ifelse(orgao == "Governo do Estado de Minas Gerais", NA, nome_anexos))

qde <- exec_aep %>%
  group_by(interacao) %>%
  summarise(qde = n())


exec_aep1 <- exec_aep %>% slice(1:2000)
exec_aep2 <- exec_aep %>% slice(2001:4000)
exec_aep3 <- exec_aep %>% slice(4001:6000)
exec_aep4 <- exec_aep %>% slice(6001:8000)
exec_aep5 <- exec_aep %>% slice(8001:10000)
exec_aep6 <- exec_aep %>% slice(10001:12000)
exec_aep7 <- exec_aep %>% slice(12001:13000)
exec_aep8 <- exec_aep %>% slice(13001:13484)

write.xlsx(as.data.frame(exec_aep1), file="exec_aep1_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(exec_aep2), file="exec_aep2_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(exec_aep3), file="exec_aep3_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(exec_aep4), file="exec_aep4_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(exec_aep5), file="exec_aep5_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(exec_aep6), file="exec_aep6_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(exec_aep7), file="exec_aep7_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(exec_aep8), file="exec_aep8_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)


##tcs

tc_aep <- emp_data %>%
  filter(poder == "Tribunais de Contas") %>%
  arrange(protocolo)

tc_valid <- emp %>%
  filter(poder == "Tribunais de Contas",
         !is.na(data))

qde <- tc_aep %>%
  group_by(interacao) %>%
  summarise(qde = n())


tc_aep1 <- tc_aep %>% slice(1:1000)
tc_aep2 <- tc_aep %>% slice(1001:2000)
tc_aep3 <- tc_aep %>% slice(2001:2718)

write.xlsx(as.data.frame(tc_aep1), file="tc_aep1_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(tc_aep2), file="tc_aep2_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(tc_aep3), file="tc_aep3_07122018.xlsx", sheetName="Folha1", 
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)