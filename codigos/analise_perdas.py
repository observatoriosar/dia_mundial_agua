



# importar pacotes
import pandas as pd

# importar dados
df = pd.read_csv("dados/Desagregado-20190320094208.csv")
df = pd.ExcelFile('dados/snis_data.xls')
df = df.parse(0)


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

# grafico de barra 5 cidades com maior perda e 5 com menor
