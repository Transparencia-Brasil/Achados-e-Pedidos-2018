library(readxl)
library(ggplot2)
library(dplyr)

setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)")


###Temas

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

##1) executivo

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

##2) legislativo

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


##3) judiciario

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


##4) tc

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


######Assuntos

###1) executivo

#a) geral

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

#b) estadual


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

#c) municipal


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


#2) Legislativo

#a) geral

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


#b) Municipal

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


#3) Judiciário

#a) geral

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


#b) trf

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


#c) tj

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


#4) Tribunal de contas

#a) geral

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


#b) tce

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


#c) tcm

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
