###Graficos Colab

library(ggplot2)
library(dplyr)
library(scales)
library(forcats)
library(extrafont)
library(stringr)


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


setwd("C:\\Users\\Renato\\Desktop\\Colab classificados\\relatorio final\\RDatas_fixed\\RDatas (template com datas arrumadas)")

load(file = "emp.RData")

###quebrando linha assuntos

emp1 <- emp %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  select(-c(aux0, aux, aux1, aux2))

unique(emp$assunto2)

##como funciona o ggplot?
# layers 
# 1) dados -> eixos x, y
# 2) tipos de grafico
# 3) textos etc.


####GRÁFICOS GERAIS
###1) Quantidade de pedidos por poder
##falta %

total_pod <- emp %>%
  filter(interacao == "Pedido") %>%
  group_by(poder) %>%
  summarise(total = n())

total_pedido_poder <- total_pod %>%
  ggplot(aes(x = reorder(poder, total), y = total)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) +
  coord_flip() + tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Total de pedidos por poder") +
  geom_text(aes(label = total), hjust = -.5) + scale_y_continuous(limits = c(0, 8000), 
                                                                  breaks = scales::pretty_breaks(n = 8))

ggsave(total_pedido_poder, file="total_pedido_poder.png", scale = .7, width = 11, height = 7)

###2) Quantidade de pedidos por nível federativo (gráfico)
##falta %

total_niv <- emp %>%
  filter(interacao == "Pedido") %>%
  group_by(esfera) %>%
  summarise(total = n())

total_pedido_nivel <- total_niv %>%
  ggplot(aes(x = reorder(esfera, total), y = total)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Total de pedidos por nível federativo")  +
  geom_text(aes(label = total), hjust = -.5) + scale_y_continuous(limits = c(0, 6000), 
                                                                  breaks = scales::pretty_breaks(n = 8))

ggsave(total_pedido_nivel, file="total_pedido_nivel.png", scale = .7, width = 11, height = 7)


###4) Atendimento por poder
##falta apagar legenda embaixo

atendpod <- emp %>%
  filter(interacao == "Pedido",
         atendimento != "Não Classificado") %>%
  group_by(poder, atendimento) %>%
  summarise(total_atend = n()) %>%
  ungroup() %>%
  group_by(poder) %>%
  mutate(total_pedidos = sum(total_atend),
         perc_atend = round(total_atend/total_pedidos, 4))
  

atendimento_poder <- atendpod %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  mutate(atendimento = reorder(atendimento, ordem_atend)) %>%
  ggplot(aes(x = reorder(atendimento, ordem_atend), y = perc_atend)) + 
    geom_bar(stat="identity", aes(fill = atendimento)) +
  facet_wrap(~ poder, nrow = 1) + tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent,
                                                                 limits = c(0,.8),
                                                                 breaks = scales::pretty_breaks(n = 8)) +
  xlab("") + ylab("")  + ggtitle("Atendimento por poder") + 
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL))
  
ggsave(atendimento_poder , file="atendimento_poder.png", scale = .9, width = 11, height = 7)



##5) Atendimento por nível federativo
##falta apagar legenda embaixo

atendniv <- emp %>%
  filter(interacao == "Pedido",
         atendimento != "Não Classificado") %>%
  group_by(esfera, atendimento) %>%
  summarise(total_atend = n()) %>%
  ungroup() %>%
  group_by(esfera) %>%
  mutate(total_pedidos = sum(total_atend),
         perc_atend = round(total_atend/total_pedidos, 4))


atendimento_nivel <- atendniv %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  ggplot(aes(x = reorder(atendimento, ordem_atend), y = perc_atend)) + geom_bar(stat="identity", aes(fill = reorder(atendimento, ordem_atend))) +
  facet_wrap(~ esfera, nrow = 1) + tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent,
                                                                 limits = c(0,.8)) + xlab("") + ylab("") +
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL)) + 
  ggtitle("Atendimento por nível federativo")

ggsave(atendimento_nivel , file="atendimento_nivel.png", scale = .8, width = 10, height = 7)


##GRÁFICOS POR PODER
##6) ATENDIMENTO DOS DEZ MAIS SOLICITADOS
##a) executivo

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


#a1) 10 mais pedidos Executivo

t10exec <- emp %>%
  filter(interacao == "Pedido",
         poder == "Executivo") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)
  
t10exec_graf <- t10exec %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos no Executivo")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .2), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10exec_graf, file="t10exec.png", scale = .7, width = 13, height = 7)

#a2) 3 temas Executivo

tema3exec <- emp %>%
  filter(interacao == "Pedido",
         poder == "Executivo") %>%
  group_by(macrotema) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:3)

tema3exec_graf <- tema3exec %>%
  ggplot(aes(x = reorder(macrotema, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Três temas mais pedidos no Executivo")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .4), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(tema3exec_graf, file="tema3exec_graf.png", scale = .6, width = 13, height = 7)

#a3) 10 assuntos execefed

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

#a4) 10 assuntos execest

t10execest <- emp %>%
  filter(interacao == "Pedido",
         poder == "Executivo",
         esfera == "Estadual") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10execest_graf <- t10execest %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos no Executivo Estadual")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .25), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10execest_graf, file="t10execest.png", scale = .7, width = 13, height = 7)

#a5) 10 assuntos execmun

t10execmun <- emp %>%
  filter(interacao == "Pedido",
         poder == "Executivo",
         esfera == "Municipal") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10execmun_graf <- t10execmun %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos no Executivo Municipal")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .2), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10execmun_graf, file="t10execmun.png", scale = .8, width = 13, height = 7)


#a6) distribuicao  niveis executivo

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


##b) legislativo

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


#b1) 10 mais pedidos Legislativo

t10leg <- emp %>%
  filter(interacao == "Pedido",
         poder == "Legislativo") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10leg_graf <- t10leg %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos no Legislativo")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .3), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10leg_graf, file="t10leg.png", scale = .6, width = 14, height = 7)

#b2) 3 temas Legislativo

tema3leg <- emp %>%
  filter(interacao == "Pedido",
         poder == "Legislativo") %>%
  group_by(macrotema) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:3)

tema3leg_graf <- tema3leg %>%
  ggplot(aes(x = reorder(macrotema, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Três temas mais pedidos no Legislativo")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .45), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(tema3leg_graf, file="tema3leg_graf.png", scale = .7, width = 13, height = 7)

#b3) 10 assuntos legfed

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

#b4) 10 assuntos legfed

t10legmun <- emp %>%
  filter(interacao == "Pedido",
         poder == "Legislativo",
         esfera == "Municipal") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10legmun_graf <- t10legmun %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  ggplot(aes(x = reorder(assunto2, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos no Legislativo Municipal")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .27), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10legmun_graf, file="t10legmun.png", scale = .6, width = 14.5, height = 9)


##c) judiciário

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

#c1) 10 mais pedidos Judiciário

t10jud <- emp %>%
  filter(interacao == "Pedido",
         poder == "Judiciário") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10jud_graf <- t10jud %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos no Judiciário")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .4), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10jud_graf, file="t10jud.png", scale = .8, width = 14, height = 7)

#c2) 3 temas Judiciário

tema3jud <- emp %>%
  filter(interacao == "Pedido",
         poder == "Judiciário") %>%
  group_by(macrotema) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:3)

tema3jud_graf <- tema3jud %>%
  ggplot(aes(x = reorder(macrotema, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Três temas mais pedidos no Judiciário")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .55), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(tema3jud_graf, file="tema3jud_graf.png", scale = .8, width = 13, height = 7)

#c3) 10 mais pedidos TJs

t10tj <- emp %>%
  filter(interacao == "Pedido",
         poder == "Judiciário",
         orgao %in% c("Tribunal de Justiça de Pernambuco", "Tribunal de Justiça de São Paulo")) %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10tj_graf <- t10tj %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos nos TJs")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .55), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10tj_graf, file="t10tj.png", scale = .8, width = 14, height = 7)

#c4) 10 mais pedidos TRFs

t10trf <- emp %>%
  filter(interacao == "Pedido",
         poder == "Judiciário",
         orgao %in% c("Tribunal Regional Federal da 2ª Região", "Tribunal Regional Federal da 3ª Região")) %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10trf_graf <- t10trf %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos nos TRFs")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .4), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10trf_graf, file="t10trf.png", scale = .8, width = 14, height = 7)

#c5) 10 mais pedidos STJ

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

#c6) 10 mais pedidos CNJ

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

##d) ministério público

atend_mp <- emp %>%
  filter(interacao == "Pedido",
         poder == "Ministério Público",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(pedidos = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_assunto = sum(pedidos)) %>%
  ungroup() %>%
  arrange(desc(total_assunto)) %>%
  mutate(ordem = dense_rank(-total_assunto)) %>%
  filter(ordem <= 7) %>%
  mutate(pedidos_perc = round(pedidos/total_assunto, 4),
         perc_atendido  = ifelse(atendimento == "Atendido", pedidos_perc, 0))

valid <- atend_mp %>%
  filter(atendimento == "Atendido")

atend_10_mp <- atend_mp %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  ggplot(aes(x = reorder(assunto, perc_atendido), y = pedidos_perc, fill = reorder(atendimento, ordem_atend))) +
  geom_bar(stat = "identity") +  
  geom_text(aes(label=ifelse(atendimento=="Não Atendido", 
                             paste0(sprintf("%.0f", pedidos_perc*100),"%"),"")),
            position = position_stack(vjust = .5)) + coord_flip() +
  tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent) + xlab("") + ylab("") + 
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL)) + 
  ggtitle("Atendimento dos dez assuntos mais solicitados — Ministério Público")

ggsave(atend_10_mp, file="atend_10_mp.png", scale = 1, width = 13, height = 7)

#d1) 10 mais pedidos Ministério Público

t10mp <- emp %>%
  filter(interacao == "Pedido",
         poder == "Ministério Público") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10mp_graf <- t10mp %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos Ministério Público")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .35), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10mp_graf, file="t10mp.png", scale = .8, width = 14, height = 7)

#d2) 3 temas Ministério Público

tema3mp <- emp %>%
  filter(interacao == "Pedido",
         poder == "Ministério Público") %>%
  group_by(macrotema) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:3)

tema3mp_graf <- tema3mp %>%
  ggplot(aes(x = reorder(macrotema, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Três temas mais pedidos no Ministério Público")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .35), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(tema3mp_graf, file="tema3mp_graf.png", scale = .7, width = 13, height = 7)

#d3) 10 mais pedidos MPPI

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

#d3) 10 mais pedidos MPRJ

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


##e) tribunal de contas

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

#e1) 10 mais pedidos Tribunais de Contas

t10tc <- emp %>%
  filter(interacao == "Pedido",
         poder == "Tribunais de Contas") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont)

t10tc_graf <- t10tc %>%
  mutate(aux0 = str_count(assunto, "\\S+"),
         aux = word(assunto, 1, 3),
         aux1 = word(assunto, 4, aux0),
         aux2 = paste(aux,  "\n", aux1, sep= " "),
         assunto2 = ifelse(aux0 > 3, aux2, assunto)) %>%
  ggplot(aes(x = reorder(assunto2, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Assuntos no Tribunal de Contas")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .47), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10tc_graf, file="t10tc.png", scale = .6, width = 14, height = 10)

#e2) 3 temas Tribunais de Contas

tema3tc <- emp %>%
  filter(interacao == "Pedido",
         poder == "Tribunais de Contas") %>%
  group_by(macrotema) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:3)

tema3tc_graf <- tema3tc %>%
  ggplot(aes(x = reorder(macrotema, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Três temas mais pedidos no Tribunal de Contas")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .5), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(tema3tc_graf, file="tema3tc.png", scale = .7, width = 13, height = 7)

#e3) 10 mais pedidos TCU

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

#e3) 10 mais pedidos TCs estaduais

t10tce <- emp %>%
  filter(interacao == "Pedido",
         poder == "Tribunais de Contas",
         esfera == "Estadual") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10tce_graf <- t10tce %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos nos TCs estaduais")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .6), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10tce_graf, file="t10tce.png", scale = .7, width = 14, height = 7)

#e4) 10 mais pedidos TCs municipais

t10tcm <- emp %>%
  filter(interacao == "Pedido",
         poder == "Tribunais de Contas",
         esfera == "Municipal") %>%
  group_by(assunto) %>%
  summarise(cont = n()) %>%
  mutate(tot = sum(cont),
         perc = cont/tot,
         perc1 = paste0(round(cont/tot,2)*100, "%")) %>%
  arrange(-cont) %>%
  slice(1:10)

t10tcm_graf <- t10tcm %>%
  ggplot(aes(x = reorder(assunto, perc), y = perc)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) + coord_flip() +
  tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Dez assuntos mais pedidos nos TCs municipais")  +
  geom_text(aes(label = perc1), hjust = -.5) + scale_y_continuous(limits = c(0, .45), 
                                                                  breaks = scales::pretty_breaks(n = 5),
                                                                  labels = scales::percent)

ggsave(t10tcm_graf, file="t10tcm.png", scale = .7, width = 14, height = 7)

##7) DEZ MAIORES TAXAS DE NÃO ATENDIMENTO
##a) executivo

natend_exec <- emp %>%
  filter(poder == "Executivo",
         interacao == "Pedido",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(total_atend = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_ped = sum(total_atend),
         perc_atend = round(total_atend/total_ped, 4)) %>%
  filter(atendimento == "Não Atendido",
         perc_atend >= 0.4318) %>%
  arrange(desc(perc_atend))

natend_10_exec <- natend_exec %>%
  ggplot(aes(x = reorder(assunto, perc_atend), y = perc_atend)) + geom_bar(stat = "identity", fill = "#ff8000", width = .3) +
  coord_flip() + tema_tb(negrito = "plain") + xlab("") + ylab("") + scale_y_continuous(labels = scales::percent,
                                                                      limits = c(0, 1), 
                                                                      breaks = scales::pretty_breaks(n = 10)) + ggtitle("Dez maiores taxas de não atendimento — Executivo")

ggsave(natend_10_exec, file="natend_10_exec.png", scale = .9, width = 12, height = 7)


##b) Legislativo

natend_leg <- emp %>%
  filter(poder == "Legislativo",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(total_atend = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_ped = sum(total_atend),
         perc_atend = round(total_atend/total_ped, 4)) %>%
  filter(atendimento == "Não Atendido",
         perc_atend >= 0.1143) %>%
  arrange(desc(perc_atend))

natend_10_leg <- natend_leg %>%
  ggplot(aes(x = reorder(assunto, perc_atend), y = perc_atend)) + geom_bar(stat = "identity", fill = "#ff8000", width = .3) +
  coord_flip() + tema_tb(negrito = "plain") + xlab("") + ylab("") + scale_y_continuous(labels = scales::percent,
                                                                                       limits = c(0, 1), 
                                                                                       breaks = scales::pretty_breaks(n = 10)) + 
  ggtitle("Dez maiores taxas de não atendimento — Legislativo")

ggsave(natend_10_leg, file="natend_10_leg.png", scale = .9, width = 12, height = 7)


##c) Judiciário

natend_jud <- emp %>%
  filter(poder == "Judiciário",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(total_atend = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_ped = sum(total_atend),
         perc_atend = round(total_atend/total_ped, 4)) %>%
  filter(atendimento == "Não Atendido",
         perc_atend >= 0.1863) %>%
  arrange(desc(perc_atend))

natend_10_jud <- natend_jud %>%
  ggplot(aes(x = reorder(assunto, perc_atend), y = perc_atend)) + geom_bar(stat = "identity", fill = "#ff8000", width = .3) +
  coord_flip() + tema_tb(negrito = "plain") + xlab("") + ylab("") + scale_y_continuous(labels = scales::percent,
                                                                                       limits = c(0, 1), 
                                                                                       breaks = scales::pretty_breaks(n = 10)) + 
  ggtitle("Dez maiores taxas de não atendimento — Judiciário")

ggsave(natend_10_jud, file="natend_10_jud.png", scale = .8, width = 13, height = 7)


##d) Ministério Público

natend_mp <- emp %>%
  filter(poder == "Ministério Público",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(total_atend = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_ped = sum(total_atend),
         perc_atend = round(total_atend/total_ped, 4)) %>%
  filter(atendimento == "Não Atendido") %>%
  arrange(desc(perc_atend))

natend_10_mp <- natend_mp %>%
  ggplot(aes(x = reorder(assunto, perc_atend), y = perc_atend)) + geom_bar(stat = "identity", fill = "#ff8000", width = .3) +
  coord_flip() + tema_tb(negrito = "plain") + xlab("") + ylab("") + scale_y_continuous(labels = scales::percent,
                                                                                       limits = c(0, 1), 
                                                                                       breaks = scales::pretty_breaks(n = 10)) + 
  ggtitle("Taxas de não atendimento — Ministério Público")


ggsave(natend_10_mp, file="natend_10_mp.png", scale = .8, width = 13, height = 7)



##e) Tribunal de Contas

natend_tc <- emp %>%
  filter(poder == "Tribunais de Contas",
         atendimento != "Não Classificado") %>%
  group_by(assunto, atendimento) %>%
  summarise(total_atend = n()) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(total_ped = sum(total_atend),
         perc_atend = round(total_atend/total_ped, 4)) %>%
  filter(atendimento == "Não Atendido") %>%
  arrange(desc(perc_atend))

natend_10_tc <- natend_tc %>%
  ggplot(aes(x = reorder(assunto, perc_atend), y = perc_atend)) + geom_bar(stat = "identity", fill = "#ff8000", width = .3) +
  coord_flip() + tema_tb(negrito = "plain") + xlab("") + ylab("") + scale_y_continuous(labels = scales::percent,
                                                                                       limits = c(0, 1), 
                                                                                       breaks = scales::pretty_breaks(n = 10)) + 
  ggtitle("Taxas de não atendimento — Tribunal de Contas")


ggsave(natend_10_tc, file="natend_10_tc.png", scale = .8, width = 13, height = 7)


#8) Áreas temáticas por poder

## cria data frame auxiliar com a ordem das cores2
atendimento_ordem2 <-  data.frame(atendimento = c("atividade-fim", "controle social", 
                                                 "informação básica", "admissão",
                                                 "serviços e impostos", "processos",
                                                 "outros"),
                                 ordem_tema = 1:7)

# cria cores2, na ordem do data frame (1:7)
cores2 = c("#4d1f00",  "#993d00",  "#e65c00",  "#ff8533",
          "#ffb380", "#c2c2a3", "#e0e0d1")

tema_pod <- emp %>%
  filter(interacao == "Pedido") %>%
  group_by(macrotema, poder) %>%
  mutate(n_pedidos = n()) %>%
  ungroup() %>%
  group_by(poder, interacao) %>%
  mutate(total_pedidos = n(),
         perc_tema_poder = n_pedidos/total_pedidos) %>%
  ungroup() %>%
  group_by(macrotema, poder) %>%
  summarise(n_pedidos = max(n_pedidos),
            total_pedidos_poder = max(total_pedidos),
            perc_tema_poder = max(perc_tema_poder)) %>%
  ungroup() %>%
  group_by(poder) %>%
  mutate(perc_total = sum(perc_tema_poder))
    
tema_pod_graf <- tema_pod %>%
  ggplot(aes(x = reorder(poder, -perc_tema_poder), y = perc_tema_poder)) + geom_bar(stat = "identity", aes(fill = macrotema)) +
  coord_flip() + tema_tb(negrito = "plain") + xlab("") + ylab("") + scale_y_continuous(labels = scales::percent,
                                                                                       limits = c(0, 1), 
                                                                                       breaks = scales::pretty_breaks(n = 10)) + 
  scale_fill_manual(values= cores2, guide = guide_legend(title = NULL)) + ggtitle("Áreas temáticas por poder - %")


ggsave(tema_pod_graf, file="tema_pod.png", scale = .8, width = 13, height = 7)


#9) Pedidos por tema

ped_tema <- emp %>%
  filter(interacao == "Pedido") %>%
  group_by(macrotema) %>%
  summarise(total = n())

ped_tema_graf <- ped_tema %>%
  ggplot(aes(x = reorder(macrotema, total), y = total)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) +
  coord_flip() + tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Total de pedidos por área temática") +
  geom_text(aes(label = total), hjust = -.5) + scale_y_continuous(limits = c(0, 4500), 
                                                                  breaks = scales::pretty_breaks(n = 5))

ggsave(ped_tema_graf, file="ped_tema.png", scale = .7, width = 11, height = 7)

#10) Pedidos por assunto

ped_10_assunto <- emp %>%
  filter(interacao == "Pedido") %>%
  group_by(assunto) %>%
  summarise(total = n()) %>%
  filter(total >= 326)

ped_10_assunto_graf <- ped_10_assunto %>%
  ggplot(aes(x = reorder(assunto, total), y = total)) + geom_bar(stat="identity", fill = "#ff8000", width = .3) +
  coord_flip() + tema_tb(negrito = "plain") + xlab("") + ylab("") + ggtitle("Total de pedidos - Dez assuntos mais solicitados") +
  geom_text(aes(label = total), hjust = -.5) + scale_y_continuous(limits = c(0, 2100), 
                                                                  breaks = scales::pretty_breaks(n = 4))

ggsave(ped_10_assunto_graf, file="ped_10_assunto.png", scale = .7, width = 17, height = 7)

##11) Atendimento de atividade-fim por poder

## cria data frame auxiliar com a ordem das cores
atendimento_ordem3 <-  data.frame(atendimento = c("Atendido", "Parcialmente Atendido", "Não Atendido"),
                                 ordem_atend = 1:3)
# cria cores, na ordem do data frame (1, 2, 3)

cores3 = c("#b35900", "#ff8000", "#ffa64d")


atend_af <- emp %>%
  filter(interacao == "Pedido",
         macrotema == "atividade-fim",
         atendimento != "Não Classificado") %>%
  group_by(poder, atendimento) %>%
  summarise(pedidos = n()) %>%
  ungroup() %>%
  group_by(poder) %>%
  mutate(total_poder = sum(pedidos),
         pedidos_perc = round(pedidos/total_poder, 4)) %>%
  mutate(valid = sum(pedidos_perc))

atend_af_graf <- atend_af  %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  ggplot(aes(x = poder, y = pedidos_perc)) +
  geom_bar(stat = "identity", aes(fill = reorder(atendimento, ordem_atend))) + coord_flip() + 
  tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent) + xlab("") + ylab("") + 
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL)) + 
  ggtitle("Atividade-fim - Atendimento por poder")

ggsave(atend_af_graf, file="atend_af_graf.png", scale = .8, width = 10, height = 7)

##12) Atendimento de controle social por poder

atend_cs <- emp %>%
  filter(interacao == "Pedido",
         macrotema == "controle social",
         atendimento != "Não Classificado") %>%
  group_by(poder, atendimento) %>%
  summarise(pedidos = n()) %>%
  ungroup() %>%
  group_by(poder) %>%
  mutate(total_poder = sum(pedidos),
         pedidos_perc = round(pedidos/total_poder, 4)) %>%
  mutate(valid = sum(pedidos_perc))

atend_cs_graf <- atend_cs  %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  ggplot(aes(x = poder, y = pedidos_perc)) +
  geom_bar(stat = "identity", aes(fill = reorder(atendimento, ordem_atend))) + coord_flip() + 
  tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent) + xlab("") + ylab("") + 
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL)) + 
  ggtitle("Controle social - Atendimento por poder")


ggsave(atend_cs_graf, file="atend_cs.png", scale = .8, width = 10, height = 7)


##13) Atendimento de informação básica por poder

atend_ib <- emp %>%
  filter(interacao == "Pedido",
         macrotema == "informação básica",
         atendimento != "Não Classificado") %>%
  group_by(poder, atendimento) %>%
  summarise(pedidos = n()) %>%
  ungroup() %>%
  group_by(poder) %>%
  mutate(total_poder = sum(pedidos),
         pedidos_perc = round(pedidos/total_poder, 4)) %>%
  mutate(valid = sum(pedidos_perc))

atend_ib_graf <- atend_ib %>%
  inner_join(atendimento_ordem, by = "atendimento") %>%
  ggplot(aes(x = poder, y = pedidos_perc)) +
  geom_bar(stat = "identity", aes(fill = reorder(atendimento, ordem_atend))) + coord_flip() + 
  tema_tb(negrito = "plain") + scale_y_continuous(labels = scales::percent) + xlab("") + ylab("") + 
  scale_fill_manual(values= cores, guide = guide_legend(title = NULL)) + 
  ggtitle("Informação básica - Atendimento por poder")


ggsave(atend_ib_graf, file="atend_ib.png", scale = .8, width = 10, height = 7)

###PARTE 2
##########

##1) 10 MAIS PEDIDOS

