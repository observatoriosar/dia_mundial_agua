#==============================================#
# OBSERVATORIO DE SANEAMENTO E MEIO
# AMBIENTE DO RECIFE (OSAR)
#==============================================#
# Publicacao Dia Mundia da Agua
#==============================================#
# Autor do Script: Claudio A. Monteiro
#==============================================#
# #usesoftwarelivre


#------------------------------------#
# Pre-processamento de dados
#------------------------------------#

# importar pacotes
library(raster); library(readxl)

# importar dados
shape_pe <- shapefile("pe_municipios/26MUE250GC_SIR.shp")
shape_pe <- shapefile(file.choose())
snis_data <- read_excel("dados/snis_data.xls")
code_muni <- read.csv("dados/BR_municipio_codigos.csv")

# selecionar dados de 2017 e retirar casos faltantes
snis_data17 <- snis_data[snis_data$`Ano de Referência` == 2017,]
snis_data17 = snis_data17[complete.cases(snis_data17$`IN049 - Índice de perdas na distribuição`),]

# combinar bases com codigo do municipio
colnames(snis_data17)[1] <- "code_muni"
snis_data17 <- merge(snis_data17, code_muni[,c(2,3)], by="code_muni")

# combinar dados perdas de agua com poligonos
colnames(shape_pe@data)[2] <- "code_muni2"
shp_data <- merge(shape_pe, snis_data17, by = "code_muni2")

#---------------------------------#
# Autocorrelacao Espacial
#---------------------------------#








