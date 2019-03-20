#-------------------------------------------#
# Observatorio de Saneamento 
# e Meio Ambiente do Recife OSAR
#------------------------------------------#

# importar pacotes
library(readxl); library(png)
library(grid)

# importar dados
snis_data <- read_excel("dados/snis_data.xls")

# importar objetos e metodos de uso geral
source("codigos/metodos_propriedades_gerais.R")

# importar imagem osar
osar <- readPNG("docs_imagens/osar_fundo-transparente.png")
osar <- rasterGrob(osar, interpolate=TRUE)

qplot(1:10, 1:10, geom="blank") +
  annotation_custom(osar, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point()

#-------------------------------------------------#
# pernambuco em comparacao com os demais estados
#-------------------------------------------------#

# grafico de barra media ultimos 10 anos

# grafico de barra media no ano mais recente

# grafico de linha PE / BA / CE

#------------------------------------------------#
# municipios de pernambuco
#------------------------------------------------#

# mapa perdas no sistema por municipio em PE media 10 anos

# mapa perdas no sistema por municipio em PE ano mais recente

#------------------------------------------------------------
# grafico de barra 10 cidades maiores perdas

snis_data17 <- snis_data[snis_data$`Ano de Referência` == 2017,]
snis_data17 = snis_data17[complete.cases(snis_data17$`IN049 - Índice de perdas na distribuição`),]

data =snis_data17
varId = 11
legendaVar = 'test'

dev02 <- function(data, varID , legendaVar){
  
  # ordenar e selecionar casos
  dataOrd = data[order(data[[varID]]),]
  dataOrd$selec = c(1:length(data[[varID]])) # selecionar variavel
  dataOrd = dataOrd[dataOrd$selec <= 10 | dataOrd$selec >= (length(dataOrd$selec)-9) ,]

  # executar visualizacao para os 10 maiores
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

dev02(snis_data17, 11, 'Perdas de Água no Abastecimento (%)')



