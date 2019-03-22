#-------------------------------------------#
# OBSERVATORIO DE SANEAMENTO E MEIO
# AMBIENTE DO RECIFE (OSAR)
#-------------------------------------------#
# Publicacao Dia Mundia da Agua
#-------------------------------------------#
# Autor do Script: Claudio A. Monteiro
#------------------------------------------#

#=========================#
# limpar string

cleanStr <- function(string){
  library(stringi); library(stringr); library(dplyr)
  string = str_replace_all(string, ' ', '_')
  string = str_replace_all(string, '-', '_')
  string = str_replace_all(string, '%', '')
  string = str_replace_all(string, '/', '_')
  string = stri_trans_general(string, "latin-ascii")
  string = tolower(string)
  return(string)
}

#========================#
# tema ggplot

tema_massa <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.ticks = element_line(size = 1, colour = "grey70" ),
          axis.text.x = element_text(colour= "black",size=10,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=10,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=10,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=11,angle=90,hjust=.5,vjust=0.5,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(6, "mm"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"),
          plot.background = element_blank())
}

#=================================#
# gerar grafico ordenado

orderPlot = function(data, varID, legendaVar){
  
  data$Município = factor(data$Município, levels = unique(data$Município[order(data[[varID]])]), ordered=TRUE)
  
  # tratar label
  data$label = paste0(data[[varID]], "%")
  
  # produzir grafico
  plot = ggplot(data = data, aes(x = as.numeric(data$`Município`), y = data[[varID]]))+
    geom_bar(fill = "#008989", position = "dodge", stat="identity")+
    scale_y_continuous(limits = c(0, 100))+
    scale_x_continuous(breaks = c(1:10), labels = as.character(data$Município))+
    geom_label(aes(label = data$label), size = 2.8)+
    labs(x = '', y = legendaVar, title = paste("10 Cidades de PE com Maiores\n", legendaVar))+
    tema_massa()+
    coord_flip()
  
  return(plot)
}

