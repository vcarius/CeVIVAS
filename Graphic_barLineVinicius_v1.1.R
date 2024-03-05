  ##Instalando pacotes
  
  pacotes <- c("plotly","tidyverse","knitr","kableExtra","cowplot", "ggplot2",
               "dplyr", "lubridate", "tidyverse")
  
  if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
    instalador <- pacotes[!pacotes %in% installed.packages()]
    for(i in 1:length(instalador)) {
      install.packages(instalador, dependencies = T)
      break()}
    sapply(pacotes, require, character = T) 
  } else {
    sapply(pacotes, require, character = T) 
  }
  
  ###direcionar aonde vou salvar meus arquivos 
  setwd("/home/isabela/Documents/Artigo_1_Influenza/Epidemiology/")
  
  getwd()
  
  #carregar a planilha
  d1 <-read.csv(file = "graphic_suppl_proportion.csv", header = T, sep = ',')
  d2 <-read.csv(file = "graphic_suppl_HAnumber.csv", header = T, sep = ',')
  
  #plotar para inglês
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
  
  #informa que a coluna data é uma data
  str(d1$year) 
  d1$year <- as.Date(as.character(d1$year), format = "%Y")
  d2$year <- as.Date(as.character(d2$year), format = "%Y")
  
  str(d2$year) 
  #d2$year <- as.Date(paste(d2$year, "-01", sep=""))
  
  #transformar em numeros
  str(d1$proportion)
  
  #d1$Media_poder_amostra_por_Mes <- as.numeric(d1$Media_poder_amostra_por_Mes)
  
  str(d2$totalHA)
  str(d2$totalInfluenzaCases)
  
  #plotar 
  
  df_combined <- merge(d1, d2, by="year")
  df
  # Crie o gráfico
  plott <- ggplot(df_combined, aes(x=as.character(substr(year, 1, 4)))) +
    geom_bar(aes(y=totalInfluenzaCases, fill="Total Influenza Cases"), stat="identity", alpha=1) +
    geom_bar(aes(y=totalHA, fill="Total HA Gene"), stat="identity", alpha=1) +
    geom_line(aes(y=proportion*230, group=1, color="Proportion of HA gene"), size=1) +
    scale_y_continuous(sec.axis = sec_axis(~./230, name="Proportion of HA gene and Influenza cases (%)", breaks = seq(0, 55, by = 5) ),
                       limits = c(0, 15000)) +  # Define os limites do eixo y da esquerda
    
    labs(title="",
         y="Total of HA gene | Total of Subtyped Influenza Cases (n)",
         x="Year")+
    
    scale_fill_manual(values=c("Total Influenza Cases"="#fdae61", "Total HA Gene"="#2b83ba")) +
    scale_color_manual(values=c("Proportion of HA gene"="black"))+
    
    theme_classic() +
    
    theme(
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text=element_text(size=13),
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
      legend.position="top" # change legend position to the top
    )
  
    #guides(fill=FALSE, color=FALSE)  # Remove legend for fill and color
  
  # Crie o gráfico
  # plott <- ggplot(df_combined, aes(x=as.character(substr(year, 1, 4)))) +
  #   geom_bar(aes(y=totalInfluenzaCases), stat="identity", fill="yellow", alpha=1) +
  #   geom_bar(aes(y=totalHA), stat="identity", fill="blue", alpha=1) +
  #   geom_line(aes(y=proportion*230, group=1), color="red", size=1) +
  #   scale_y_continuous(sec.axis = sec_axis(~./230, name="Proportion of HA gene and Influenza cases (%)")) +
  #   #scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") + 
  #   
  #   labs(title="",
  #        y="Total of HA gene / Total of Flu Cases",
  #        x="Year")+
  #   theme_classic() +
  #   theme(
  #     axis.title.x = element_text(size = 16),
  #     axis.title.y = element_text(size = 16),
  #     #axis.text.x=element_text(angle = 90),
  #     axis.text=element_text(size=13),
  #     panel.background = element_rect(fill = "transparent"), # bg of the panel
  #     plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  #     #panel.grid.major = element_blank(), # get rid of major grid
  #     #panel.grid.minor = element_blank(), # get rid of minor grid
  #     legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  #     legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  #   )
  # 
  plott
  ggsave("graphic_barLine.pdf", plot= plott, dpi=300 , height = 10, width = 20, limitsize = FALSE)
  