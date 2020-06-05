library(geobr)
library(tidyverse)
library(stringr)
library(ggspatial)
library(extrafont)
font_import()
loadfonts(device = "win")

#Mapa do Ceará
Ceara<-read_municipality(code_muni = "CE")
Ceara$name_muni<-str_to_lower(Ceara$name_muni) %>% abjutils::rm_accent()




dados<-read.csv2("corona_05_06.csv",header = TRUE,sep=";")
dados<-janitor::clean_names(dados)
dados<- dados %>% mutate(percentual_recuperados=(recuperados/casos_confirmados)*100)
names(dados)<-c("name_muni","casos_confirmados","recuperados","percentual_recuperados")
dados$percentual_recuperados<-round(dados$percentual_recuperados,2)



# Selecionando apenas duas variáveis
dados_percentuais<-dados %>% select(name_muni,percentual_recuperados)

## Nome das Cidades pra Minusculo e remover os acentos
dados_percentuais$name_muni<-str_to_lower(dados_percentuais$name_muni) %>% abjutils::rm_accent()

# Juntando os dados
mapa<-left_join(Ceara,dados_percentuais,by="name_muni")



### Código Mapa
Mapa_Perc_Rec<-mapa %>% 
  ggplot() +
  geom_sf(aes(fill = percentual_recuperados)) + 
  scale_fill_continuous(name = "Percentual de Recuperados", low = 'white', high = 'green4',
                        na.value = 'gray') + theme_bw() + ggtitle("Percentual de Recuperados por Município") +
  labs(caption = "Fonte: Integrasus - Atualização: 05/06/2020 às 09:39:56") +
  theme(text=element_text(size=8.5,  family="Arial Narrow",face = "bold")) 
# + annotation_scale(location = "br", width_hint = 0.3) +
#   annotation_north_arrow(location = "tr", which_north = "true", 
#                          pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
#                          style = north_arrow_fancy_orienteering)

Mapa_Perc_Rec

#### Contando quantas cidades tem até 50% de recuperados
dados_percentuais %>% count(percentual_recuperados<=50)
