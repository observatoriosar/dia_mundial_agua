#==============================================#
# OBSERVATORIO DE SANEAMENTO E MEIO
# AMBIENTE DO RECIFE (OSAR)
#==============================================#
# Publicacao Dia Mundia da Agua
#==============================================#
# Autor do Script: Claudio A. Monteiro
#==============================================#
# #usesoftwarelivre


#------------------------------------------#
# Pre-processamento
#------------------------------------------#

# instalar pacotes
# install.packages( c("readxl", "png", "grid", "ggplot2", "stringr", 
#                     "viridis", "maps", "raster", "ggrepel", "sp",
#                     "maptools", "dplyr", "stringr", "stringi", "rgdal"),
#                      dependencies = T)

# importar pacotes
pacotes <- c("readxl", "png", "grid", "ggplot2", "stringr", 
             "viridis", "maps", "raster", "ggrepel", "sp",
             "maptools", "dplyr", "stringr", "stringi", "rgdal")
lapply(pacotes, library, character.only = T)

# importar e configurar pacote espacial gpclib
if (!require(gpclib)) install.packages("gpclib", type="source")
library(gpclib)
gpclibPermit()


# importar dados
snis_data <- read_excel("dados/snis_data.xls")
code_muni <- read.csv("dados/BR_municipio_codigos.csv")
secas_PE <- read_excel("dados/secas_PE.xlsx")
shape_pe <- shapefile("pe_municipios/26MUE250GC_SIR.shp")
shape_pe <- shapefile(file.choose())

# importar objetos e metodos de uso geral
source("codigos/metodos_propriedades_gerais.R")

# importar imagem osar
osar <- readPNG("docs_imagens/osar_fundo-transparente.png")
osar <- rasterGrob(osar, interpolate=TRUE)

# selecionar dados e retirar casos faltantes
snis_data17 <- snis_data[snis_data$`Ano de Referência` == 2017,]
snis_data17 = snis_data17[complete.cases(snis_data17$`IN049 - Índice de perdas na distribuição`),]
snis_data = snis_data[complete.cases(snis_data$`IN049 - Índice de perdas na distribuição`),]
snis_data = snis_data[snis_data$`Ano de Referência` >= 2000,]

# combinar code_muni com snis_data17
colnames(snis_data17)[1] <- "code_muni"
snis_data17 <- merge(snis_data17, code_muni[,c(2,3,4)], by="code_muni")

# combinar dados de perdas de agua com poligonos espaciais
colnames(shape_pe@data)[2] <- "code_muni2"
shp_data <- merge(shape_pe, snis_data17, by = "code_muni2")

#==============================================#
#  Indice de Perdas no Abastecimento - PE
#==============================================#

# agregar media de PE por ano
snis_PE_ano <- aggregate(snis_data$`IN049 - Índice de perdas na distribuição`, 
                         by = list(snis_data$`Ano de Referência`), 
                         mean) 
snis_PE_ano$indice_perdas_distribuicao <- round(snis_PE_ano$x, 2)
snis_PE_ano$ano <- as.character(snis_PE_ano$Group.1)

# grafico de linha
ggplot(data = snis_PE_ano, aes(x = snis_PE_ano$ano, y = snis_PE_ano$indice_perdas_distribuicao, group =1))+
  geom_line(color = "#008989")+
  stat_smooth(method = lm, color= "#E69F00", se = F)+
  geom_text_repel(aes(label = snis_PE_ano$indice_perdas_distribuicao), size = 3)+
  scale_y_continuous(limits = c(0, 100))+
  labs(y = "% de Perdas", x = "", title = "Perdas de Água no Abastecimento em PE (1998-2017)")+
  tema_massa()+
  theme(axis.text.x = element_text(angle = 50))+
  ggsave("perdasPE.png", path = "resultados",width = 9, height = 4, units = "in" )


#==============================================#
# grafico de barra 10 cidades maiores perdas
#==============================================#

dev02 <- function(data, varID , legendaVar){
  
  # ordenar e selecionar casos
  dataOrd = data[order(data[[varID]]),]
  dataOrd$selec = c(1:length(data[[varID]])) # selecionar variavel
  dataOrd = dataOrd[dataOrd$selec <= 10 | dataOrd$selec >= (length(dataOrd$selec)-9) ,]
  
  # executar visualizacao para os 10 maiores (source metodos propriedade gerais)
  plot_more = orderPlot(dataOrd[11:20,], varID, legendaVar)+
    annotation_custom(osar, ymin=80, ymax=100, xmax = 3) 
  
  # salvar grafico
  ggsave(paste0('10cidades_', cleanStr(legendaVar), '.png'), 
         plot_more, 
         path = 'resultados',
         width = 7,
         height = 4,
         units = 'in')
  
  return(plot_more)
}

# executar funcao
dev02(snis_data17, 11, 'Perdas de Água no Abastecimento (%)')

#====================================#
# MAPA das perdas de agua
#====================================#

# ordenar
shp_data <- shp_data[order(shp_data$`IN049 - Índice de perdas na distribuição`),]

# tranformar shapefile em polygonsdataframe
data_fortity = fortify(shp_data, region = "NM_MUNICIP")

# extrair centroides dos poligonos
centroids.df = as.data.frame(coordinates(shp_data))
names(centroids.df) = c("Longitude", "Latitude")  #more sensible column localidades

# base para plotagem
localidade = shp_data@data$NM_MUNICIP
Freq = shp_data@data$`IN049 - Índice de perdas na distribuição`
map_dataframe = data.frame(localidade, Freq, centroids.df, nomes_centroides = "")
map_dataframe$nomes_centroides = as.character(map_dataframe$nomes_centroides)
map_dataframe$localidade = as.character(map_dataframe$localidade)
map_dataframe$nomes_centroides[174:179] = map_dataframe$localidade[174:179]

#---- mapa ----#
library(ggrepel)
library(viridis)
ggplot(data = map_dataframe, aes(map_id = localidade)) + 
  geom_map(aes(fill = shp_data$`IN049 - Índice de perdas na distribuição`), size = 0.2, colour = grey(0.85),  map = data_fortity) +
  expand_limits(x = data_fortity$long, y = data_fortity$lat) +
  scale_fill_viridis(name = "Perdas de Água no Sistema\n de Abastecimento em PE (%)", option= "D")+
  #scale_fill_gradient(name = "Número de Projetos", low="lightgreen", high= "darkblue")+
  #geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude), size = 3, color = "black") +
  labs(title = "")+
  coord_fixed(1) +
  theme_minimal()%+replace% 
  theme(legend.direction = "horizontal",
        legend.position=c(0.5, 0.6),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+annotation_custom(osar, ymin=-10, ymax = -8, xmin = -33.5, xmax = -32)
ggsave("mapa_PE.png", path = "resultados",width = 14, height = 6, units = "in" )


#============================#
# SECA em PE no tempo
#===========================#

# trasnformar em character para plotagem e retirar dias
secas_PE$mapa2 <- as.character(as.Date(secas_PE$Mapa))
secas_PE$mapa2 <- str_replace(secas_PE$mapa2, "-01", "")

# grafico de linha
ggplot(data = secas_PE, aes(x = secas_PE$mapa2, y = secas_PE$`S3-S4`, group =1))+
  geom_line(color = "#990000")+
  geom_area(fill = "#990000") +
  stat_smooth(method = lm, color= "#E69F00", se = F)+
  labs(y = "Nível de Seca", x = "", title = "Nível de Seca em PE (S3-S4)")+
  tema_massa()+
  theme(axis.text.x = element_text(angle = 60))+
  ggsave("seca_S3S4.png", path = "resultados",width = 10, height = 4, units = "in" )











