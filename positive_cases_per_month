#####Graph % positive cases per month#####
# Installing packages
pacotes <- c("plotly","kableExtra","ggplot2", "tidyr", "dplyr",
             "lubridate", "stringr", "gridExtra", "grid", "reshape2")

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
d1 <-read.csv(file = "Contagem_meses_upas_ISR.csv", header = T, sep = '\t')

# Calculating the total number of samples for each UPA (Emergency care unit)
d1$total <- rowSums(d1[, -1])

# Converting values to percentage
df_porcentagem <- d1[, -c(1, ncol(d1))] / d1$total * 100

# Adding the UPA column back to the data
df_porcentagem <- cbind(UPA = d1$UPA, df_porcentagem)

# Transforming the dataframe into long format to facilitate plotting
df_long <- melt(df_porcentagem, id.vars = "UPA", 
                variable.name = "Mes", value.name = "Porcentagem")

# Defining the desired colors for each UPA
cores <- c("BUTANTÃ" = "#1b9e77",
           "JAÇANÃ" = "#d95f02",
           "MARIA ANTONIETA" = "#7570b3",
           "TATUAPÉ" = "#e7298a",
           "TITO LOPES" = "#66a61e",
           "VERGUEIRO" = "#1f78b4")

# Plotting the line graph
plot <- ggplot(df_long, aes(x = Mes, y = Porcentagem, color = UPA, group = UPA)) +
  geom_line(size = 0.8) + # Increase line thickness
  labs(x = "", y = "Positive Cases (%)", color = "UPA") +
  scale_color_manual(values = cores) + # Set colors manually
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate labels by 45 degrees
    axis.title.y = element_text(size = 14) # Increase the size of the y-axis title
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))

# Save 
ggsave("Positive_cases_per_month.pdf", plot, width = 10, height = 6, units = "in", dpi = 400)

