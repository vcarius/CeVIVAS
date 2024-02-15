#########Sample power graph###############
#Installing packages
pacotes <- c("plotly","kableExtra","ggplot2", "tidyr", "dplyr",
             "lubridate", "stringr", "gridExtra", "grid")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Direct where I will save my files
setwd("/home/gabriela/Documentos/REDE_influenza/SOLICITACOES/IGOR/Figuras_PSP/")
getwd()

# Loading file
d1 <-read.csv(file = "dataset_poder_amostral_2.csv", header = T, sep = ';')

# Plot

bar <- ggplot(data = d1, aes(x = UPA, y = Positivos, fill = UPA)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Positivos / sum(Positivos) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 6, color = "black") +
  scale_y_continuous(position = "left", name = "Quantities of positives", breaks = seq(0, max(d1$Positivos), by = 50)) +
  labs(title = "PCR Test Results Conducted in the Year 2023", x = "UPAs") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16)
  ) +
  scale_fill_manual(
    values = c(
      "BUTANTAN" = "#8dd3c7",
      "JACANA" = "#ffffb3",
      "MARIA ANTONIETA" = "#bebada",
      "TATUAPE" = "#fb8072",
      "TITO LOPES" = "#80b1d3",
      "VERGUEIRO" = "#fdb462"
    ),
    name = ""
  )


bar2 <- bar + theme( rect = element_rect(fill = "transparent"))
bar2 <- bar2 +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA), # cor mais clara para o fundo do painel
    plot.background = element_rect(fill = "transparent", color = NA), # bg do gráfico
    #panel.grid.major = element_blank(), # remover grade principal
    #panel.grid.minor = element_blank(), # remover grade secundária
    legend.background = element_rect(fill = "transparent") # remover fundo da legenda
  )
bar2

# Save 
ggsave("Figura3_casos_posivos_poder_amostral.pdf", bar2, width = 16, height = 9, units = "in", dpi = 400)


######################sample power graph 2#########################
# Loading file 2
d2 <-read.csv(file = "dataset_poder_amostral.csv", header = T, sep = '\t')

# Plot the bar chart of expected and realized by UPA
d2_long <- d2 %>%
  pivot_longer(cols = c(Realizado, Esperado),
               names_to = "Tipo",
               values_to = "Valor")

bar2 <- ggplot(d2_long, aes(x = UPA, y = Valor, fill = Tipo)) +
  geom_col(position = "dodge") +
  labs(title = "Expected vs. Actual Sample Quantities in UPA",
       y = "Quantities",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 16)) +
  scale_fill_manual(values = c("#e78ac3", "#8da0cb"))

bar2

# Create bar chart of difference between expected and realized
dif <- ggplot(d2, aes(x = UPA, y = Diferença, fill = Diferença > 0)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Diferença), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Discrepancy Between Expected and Actual Values in UPA",
       x = "UPA",
       y = "Difference in Values") +
  scale_fill_manual(values = c("#fb9a99", "#b2df8a")) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5, size = 16))

dif

# Join all graphs into one
combined_plot <- grid.arrange(bar2,
                              dif, ncol = 2)

# Save
ggsave("Figura3_poder_amostral.pdf", combined_plot, width = 16, height = 9, units = "in", dpi = 400)

