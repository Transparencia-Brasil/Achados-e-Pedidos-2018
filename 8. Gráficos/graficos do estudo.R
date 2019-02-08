###Graficos Colab

library(ggplot2)
library(dplyr)
library(scales)
library(forcats)
library(extrafont)
library(stringr)
library(readxl)


###tema dos gráficos
tema_tb <- function(tam_fonte = 16, fonte = "calibri", negrito ="bold"){
  
  ggplot2::theme_minimal(base_size = tam_fonte, base_family = fonte) +
    ggplot2::theme(axis.text = ggplot2::element_text(color = "black"),
                   legend.text = ggplot2::element_text(),
                   legend.position = "top",
                   plot.subtitle = ggplot2::element_text(size = tam_fonte - 2),
                   plot.title = ggplot2::element_text(size = tam_fonte + 2, face = negrito,
                                                      hjust = 0.5),
                   panel.grid = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(color = "gray90"),
                   panel.grid.major.x = ggplot2::element_line(color = "gray90"),
                   axis.line.x = ggplot2::element_line()
    )
}


## cria data frame auxiliar com a ordem das cores
atendimento_ordem <-  data.frame(atendimento = c("Atendido", "Parcialmente Atendido", "Não Atendido", "Não Classificado"),
                                 ordem_atend = 1:4)
# cria cores, na ordem do data frame (1, 2, 3, 4)

cores = c("#b35900", "#ff8000", "#ffa64d", "#ffcc99")


setwd("C:\\Users\\Renato\\Documents\\Transparencia-Brasil\\Colab\\3. Empilhamento final e correções")

load(file = "emp.RData")

###quebrando linha assuntos

emp1 <- emp %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  select(-c(aux0, aux, aux1, aux2))

# Gráfico 1 Distribuição dos pedidos – Executivo

total_niv_ex <- emp %>%
  filter(interacao == "Pedido",
         poder == "Executivo") %>%
  group_by(esfera) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont)

total_niv_ex_graf <- total_niv_ex %>%
  ggplot(aes(x = reorder(esfera, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Distribuição dos pedidos - Executivo")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .5), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(total_niv_ex_graf, file="total_niv_ex.png", scale = .6, width = 13, height = 7)


# Gráfico 2 Áreas temáticas mais solicitadas nos órgãos do Executivo

tema_medias_exec <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                               sheet = "texec")

tema_medias_exec <- tema_medias_exec %>%
  mutate(m = as.numeric(m)) %>%
  mutate(perc1 = paste0(round(m, 2)*100, "%")) %>%
  arrange(-m) %>%
  slice(1:3)

tema_medias_execg <- tema_medias_exec %>%
  ggplot(aes(x = reorder(tema, m), y = m)) + 
  geom_bar(stat="identity", fill = "#ff8000", width = .3) + 
  coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + 
  ylab("") + 
  ggtitle("Áreas temáticas com maiores médias no Executivo")  +
  geom_text(aes(label = perc1), hjust = -.5) +
  scale_y_continuous(limits = c(0, .5), breaks = scales::pretty_breaks(n = 5), labels = scales::percent)

ggsave(tema_medias_execg, file="tema_medias_execg.png", scale = .6, width = 14, height = 7)


# Gráfico 3 Assuntos mais solicitados nos órgãos do Executivo

medias_exec <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                          sheet = "exec")

medias_exec <- medias_exec %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  select(-c(aux0, aux, aux1, aux2)) %>%
  mutate(m = as.numeric(m)) %>%
  mutate(perc1 = paste0(round(m, 2)*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_execg <- medias_exec %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias no Executivo")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .2), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_execg, file="medias_execg.png", scale = .7, width = 14, height = 10)


# Gráfico 4 Atendimento dos assuntos mais solicitados - Executivo

atend_exec <- emp %>%
  filter(interacao == "Pedido",
         poder == "Executivo",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(pedidos = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_assunto = sum(pedidos)) %>%
  ungroup() %>%
  arrange(desc(total_assunto)) %>%
  mutate(ordem = dense_rank(-total_assunto)) %>%
  filter(ordem <= 10) %>%
  mutate(pedidos_perc = round(pedidos/total_assunto, 4),
         perc_atendido  = ifelse(atendimento == "Atendido", pedidos_perc, 0))


valid <- atend_exec %>%
  filter(atendimento == "Atendido")

atend_10_exec <- atend_exec %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  ggplot(aes(x = reorder(assunto2, perc_atendido), y = pedidos_perc, fill = reorder(atendimento, ordem_atend))) +
  geom_bar(stat = "identity") +  
  geom_text(aes(label=ifelse(atendimento=="Não Atendido", 
                             paste0(sprintf("%.0f", pedidos_perc*100),"%"),"")),
            position = position_stack(vjust = .5)) + coord_flip() + tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent) + xlab("") + ylab("") + 
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL)) + 
  ggtitle("Atendimento dos dez assuntos mais solicitados — Executivo")


ggsave(atend_10_exec, file="atend_10_exec.png", scale = .6, width = 15, height = 10)


# Gráfico 5 Assuntos mais solicitados no Executivo federal

t10execfed <- emp %>%
  filter(interacao == "Pedido",
         poder == "Executivo",
         esfera == "Federal") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10execfed_graf <- t10execfed %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  ggplot(aes(x = reorder(assunto2, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos no Executivo Federal")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .2), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10execfed_graf, file="t10execfed.png", scale = .6, width = 14, height = 10)


# Gráfico 6 Assuntos mais solicitados nos órgãos do Executivo estadual

medias_exeest <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                            sheet = "exeest")

medias_exeest <- medias_exeest %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  mutate(m = as.numeric(m)) %>%
  mutate(perc1 = paste0(round(m, 2)*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_exeestg <- medias_exeest %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias no Executivo Estadual")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .3), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_exeestg, file="medias_exeestg.png", scale = .6, width = 14, height = 10)


# Gráfico 7 Assuntos mais solicitados nos órgãos do Executivo municipal


medias_execmun <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                             sheet = "execmun ")

medias_execmun <- medias_execmun %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  mutate(m = as.numeric(m)) %>%
  mutate(perc1 = paste0(round(m, 2)*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_execmung <- medias_execmun %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias no Executivo Municipal")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .15), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_execmung, file="medias_execmung.png", scale = .7, width = 14, height = 10)


# Gráfico 8 Áreas temáticas mais solicitadas nos órgãos do Legislativo

tema_medias_leg <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                              sheet = "tleg")

tema_medias_leg <- tema_medias_leg %>%
  mutate(m = as.numeric(m)) %>%
  mutate(perc1 = paste0(round(m, 2)*100, "%")) %>%
  arrange(-m) %>%
  slice(1:3)

tema_medias_legg <- tema_medias_leg %>%
  ggplot(aes(x = reorder(tema, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Áreas temáticas com maiores médias no Legislativo")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .4), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(tema_medias_legg, file="tema_medias_legg.png", scale = .6, width = 14, height = 7)


# Gráfico 9 Assuntos mais solicitados nos órgãos do Legislativo

medias_leg <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                         sheet = "leg")

medias_leg <- medias_leg %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  mutate(m = round(as.numeric(m),2)) %>%
  mutate(perc1 = paste0(m*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_legg <- medias_leg %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias no Legislativo")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .25), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_legg, file="medias_legg.png", scale = .7, width = 14, height = 10) 


# Gráfico 10 Atendimento dos assuntos mais solicitados - Legislativo

atend_leg <- emp %>%
  filter(interacao == "Pedido",
         poder == "Legislativo",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(pedidos = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_assunto = sum(pedidos)) %>%
  ungroup() %>%
  arrange(desc(total_assunto)) %>%
  mutate(ordem = dense_rank(-total_assunto)) %>%
  filter(ordem <= 10) %>%
  mutate(pedidos_perc = round(pedidos/total_assunto, 4),
         perc_atendido  = ifelse(atendimento == "Atendido", pedidos_perc, 0))

valid <- atend_leg %>%
  filter(atendimento == "Atendido")


atend_10_leg <- atend_leg %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  ggplot(aes(x = reorder(assunto2, perc_atendido), y = pedidos_perc, fill = reorder(atendimento, ordem_atend))) +
  geom_bar(stat = "identity") +  
  geom_text(aes(label=ifelse(atendimento=="Não Atendido", 
                             paste0(sprintf("%.0f", pedidos_perc*100),"%"),"")),
            position = position_stack(vjust = .5)) + coord_flip() +
  tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent) + xlab("") + ylab("") + 
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL)) +
  ggtitle("Atendimento dos dez assuntos mais solicitados — Legislativo")


ggsave(atend_10_leg, file="atend_10_leg.png", scale = .6, width = 17, height = 10)


# Gráfico 11 Assuntos mais solicitados na Câmara dos Deputados

t10legfed <- emp %>%
  filter(interacao == "Pedido",
         poder == "Legislativo",
         esfera == "Federal") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10legfed_graf <- t10legfed %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  ggplot(aes(x = reorder(assunto2, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos na Câmara dos Deputados")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .4), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10legfed_graf, file="t10legfed.png", scale = .6, width = 14.5, height = 9)


# Gráfico 12 Assuntos mais solicitados nos órgãos do Legislativo municipal

medias_legmun <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                            sheet = "legmun")

medias_legmun <- medias_legmun %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  select(-c(aux0, aux, aux1, aux2)) %>%
  mutate(m = round(as.numeric(m),2)) %>%
  mutate(perc1 = paste0(m*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_legmung <- medias_legmun %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias no Legislativo Municipal")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .25), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_legmung, file="medias_legmung.png", scale = .7, width = 15, height = 10)


# Gráfico 13 Áreas temáticas mais solicitadas nos órgãos do Judiciário

tema_medias_jud <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                              sheet = "tjud")

tema_medias_jud <- tema_medias_jud %>%
  mutate(m = as.numeric(m)) %>%
  mutate(perc1 = paste0(round(m, 2)*100, "%")) %>%
  arrange(-m) %>%
  slice(1:3)

tema_medias_judg <- tema_medias_jud %>%
  ggplot(aes(x = reorder(tema, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Áreas temáticas com maiores médias no Judiciário")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .55), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(tema_medias_judg, file = "tema_medias_judg.png", scale = .6, width = 14, height = 7)


# Gráfico 14 Assuntos mais solicitados nos órgãos do Judiciário

medias_jud <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                         sheet = "jud")

medias_jud <- medias_jud %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  select(-c(aux0, aux, aux1, aux2)) %>%
  mutate(m = round(as.numeric(m),2)) %>%
  mutate(perc1 = paste0(m*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_judg <- medias_jud %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias no Judiciário")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .4), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_judg, file="medias_judg.png", scale = .7, width = 15, height = 10)


# Gráfico 15 Atendimento dos assuntos mais solicitados - Judiciário

atend_jud <- emp %>%
  filter(interacao == "Pedido",
         poder == "Judiciário",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(pedidos = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_assunto = sum(pedidos)) %>%
  ungroup() %>%
  arrange(desc(total_assunto)) %>%
  mutate(ordem = dense_rank(-total_assunto)) %>%
  filter(ordem <= 10) %>%
  mutate(pedidos_perc = round(pedidos/total_assunto, 4),
         perc_atendido  = ifelse(atendimento == "Atendido", pedidos_perc, 0))

valid <- atend_jud %>%
  filter(atendimento == "Atendido")

atend_10_jud <- atend_jud %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  ggplot(aes(x = reorder(assunto2, perc_atendido), y = pedidos_perc, fill = reorder(atendimento, ordem_atend))) +
  geom_bar(stat = "identity") +  
  geom_text(aes(label=ifelse(atendimento=="Não Atendido", 
                             paste0(sprintf("%.0f", pedidos_perc*100),"%"),"")),
            position = position_stack(vjust = .5)) + coord_flip() +
  tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent) + xlab("") + ylab("") + 
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL)) + 
  ggtitle("Atendimento dos dez assuntos mais solicitados — Judiciário")

ggsave(atend_10_jud, file="atend_10_jud.png", scale = .6, width = 15, height = 11)


# Gráfico 16 Assuntos mais solicitados nos TJs

medias_tj <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                        sheet = "tj")

medias_tj <- medias_tj %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  select(-c(aux0, aux, aux1, aux2)) %>%
  mutate(m = round(as.numeric(m),2)) %>%
  mutate(perc1 = paste0(m*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_tjg <- medias_tj %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias nos TJs")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .5), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_tjg, file="medias_tjg.png", scale = .7, width = 13, height = 10)


# Gráfico 17 Assuntos mais solicitados nos TRFs

medias_trf <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                         sheet = "trf")

medias_trf <- medias_trf %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  select(-c(aux0, aux, aux1, aux2)) %>%
  mutate(m = round(as.numeric(m),2)) %>%
  mutate(perc1 = paste0(m*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_trfg <- medias_trf %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias no TRFs")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .35), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_trfg, file="medias_trfg.png", scale = .7, width = 13, height = 10)


# Gráfico 18 Assuntos mais solicitados no STJ

t10stj <- emp %>%
  filter(interacao == "Pedido",
         poder == "Judiciário",
         orgao == "Superior Tribunal de Justiça") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10stj_graf <- t10stj %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  ggplot(aes(x = reorder(assunto2, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos no STJ")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .6), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10stj_graf, file="t10stj.png", scale = .6, width = 14, height = 10)


# Gráfico 19 Assuntos mais solicitados no CNJ

t10cnj <- emp %>%
  filter(interacao == "Pedido",
         poder == "Judiciário",
         orgao == "Conselho Nacional de Justiça") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10cnj_graf <- t10cnj %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  ggplot(aes(x = reorder(assunto2, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos mais pedidos no CNJ")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .3), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10cnj_graf, file="t10cnj.png", scale = .6, width = 14, height = 10)


# Gráfico 20 Áreas temáticas mais solicitadas nos Tribunais de Contas

tema_medias_tc <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                             sheet = "ttc")

tema_medias_tc <- tema_medias_tc %>%
  mutate(m = as.numeric(m)) %>%
  mutate(perc1 = paste0(round(m, 2)*100, "%")) %>%
  arrange(-m) %>%
  slice(1:3)

tema_medias_tcg <- tema_medias_tc %>%
  ggplot(aes(x = reorder(tema, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Áreas temáticas com maiores médias nos Tribunais de Contas")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .53), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(tema_medias_tcg, file = "tema_medias_tcg.png", scale = .6, width = 15, height = 7)


# Gráfico 21 Assuntos mais solicitados nos Tribunais de Contas

medias_tc <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                        sheet = "tc")

medias_tc <- medias_tc %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  select(-c(aux0, aux, aux1, aux2)) %>%
  mutate(m = round(as.numeric(m),2)) %>%
  mutate(perc1 = paste0(m*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_tcg <- medias_tc %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias nos Tribunais de Contas")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .52), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_tcg, file="medias_tcg.png", scale = .7, width = 13, height = 10)


# Gráfico 22 Atendimento dos assuntos mais solicitados - Tribunais de Contas

atend_tc <- emp %>%
  filter(interacao == "Pedido",
         poder == "Tribunais de Contas",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(pedidos = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_assunto = sum(pedidos)) %>%
  ungroup() %>%
  arrange(desc(total_assunto)) %>%
  mutate(ordem = dense_rank(-total_assunto)) %>%
  filter(ordem <= 10) %>%
  mutate(pedidos_perc = round(pedidos/total_assunto, 4),
         perc_atendido  = ifelse(atendimento == "Atendido", pedidos_perc, 0))

valid <- atend_tc %>%
  filter(atendimento == "Atendido")

atend_10_tc <- atend_tc %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  ggplot(aes(x = reorder(assunto2, perc_atendido), y = pedidos_perc, fill = reorder(atendimento, ordem_atend))) +
  geom_bar(stat = "identity") +  
  geom_text(aes(label=ifelse(atendimento=="Não Atendido", 
                             paste0(sprintf("%.0f", pedidos_perc*100),"%"),"")),
            position = position_stack(vjust = .5)) + coord_flip() +
  tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent) + xlab("") + ylab("") + 
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL)) + 
  ggtitle("Atendimento dos dez assuntos mais solicitados — 
          Tribunal de Contas")


###não sei por que "convenios, licitacoes"
#não está ordenando

ggsave(atend_10_tc, file="atend_10_tc.png", scale = .6, width = 14, height = 10)


# Gráfico 23 Assuntos mais solicitados no TCU

t10tcu <- emp %>%
  filter(interacao == "Pedido",
         orgao == "Tribunal de Contas da União") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10tcu_graf <- t10tcu %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  ggplot(aes(x = reorder(assunto2, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos no TCU")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .6), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10tcu_graf, file="t10tcu.png", scale = .6, width = 14, height = 10)

# Gráfico 24 Assuntos mais solicitados nos TCs estaduais

medias_tce <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                         sheet = "tce")

medias_tce <- medias_tce %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  mutate(m = as.numeric(m)) %>%
  mutate(perc1 = paste0(round(m, 2)*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_tceg <- medias_tce %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias nos TCs estaduais")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .6), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_tceg, file="medias_tceg.png", scale = .7, width = 13, height = 10)


# Gráfico 25 Assuntos mais solicitados nos TCs municipais

medias_tcm <- read_excel("C:/Users/Renato/Desktop/Colab classificados/relatorio final/RDatas_fixed/RDatas (template com datas arrumadas)/medias.xlsx",
                         sheet = "tcm")

medias_tcm <- medias_tcm %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  mutate(m = as.numeric(m)) %>%
  mutate(perc1 = paste0(round(m, 2)*100, "%")) %>%
  arrange(-m) %>%
  slice(1:10)

medias_tcmg <- medias_tcm %>%
  ggplot(aes(x = reorder(assunto2, m), y = m)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos com maiores médias nos TCs municipais")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .45), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(medias_tcmg, file="medias_tcmg.png", scale = .7, width = 13, height = 10)


# Gráfico 26 Assuntos solicitados do MP-PI

t10mppi <- emp %>%
  filter(interacao == "Pedido",
         orgao == "Ministério Público Estadual do Piauí") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10mppi_graf <- t10mppi %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  ggplot(aes(x = reorder(assunto2, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos no MP-PI")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .4), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10mppi_graf, file="t10mppi.png", scale = .6, width = 14, height = 10)


# Gráfico 27 Assuntos solicitados no MP-RJ

t10mprj <- emp %>%
  filter(interacao == "Pedido",
         orgao == "Ministério Público Estadual do Rio de Janeiro") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10mprj_graf <- t10mprj %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  ggplot(aes(x = reorder(assunto2, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos no MP-RJ")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .65), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10mprj_graf, file="t10mprj.png", scale = .6, width = 14, height = 10)
