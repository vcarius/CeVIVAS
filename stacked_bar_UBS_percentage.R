##############stacked_bar_UBS_percentage###############
#Installing packages
pacotes <- c("plotly","tidyverse","kableExtra","ggplot2", "tidyr", "dplyr",
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
d1 <-read.csv(file = "BASE_UPAS_Taina_31012024_APENAS_2023.csv", header = T, sep = ';')

# select columns
d_sel <- select(d1, UBS, RESULT_INF_A, RESULT_INF_B, RESULT_COV)

# Create a list of specific UBS
ubs_list <- c("JACANA", "BUTANTAN", "MARIA ANTONIETA", "TATUAPE", "TITO LOPES", "VERGUEIRO")

################FLUA############################### ########
# Function to perform operations for a specific UBS
processar_ubs <- function(df, ubs_nome, col_resultado) {
  df %>%
    filter(UBS == ubs_nome) %>%
    group_by({{col_resultado}}) %>%
    summarise(COUNT = n()) 
}

# Loop over each UBS in the list
resultados_UBS <- lapply(ubs_list, function(ubs_nome) {
  processar_ubs(d_sel, ubs_nome, RESULT_INF_A)
})

# Assign results to named objects
names(resultados_UBS) <- paste0("contagem_A_", ubs_list)

# Assign individual results to named objects (optional)
list2env(resultados_UBS, envir = .GlobalEnv)


# Remove line without info
contagem_A_JACANA <- contagem_A_JACANA %>%
  slice(-1)

`contagem_A_MARIA ANTONIETA` <- `contagem_A_MARIA ANTONIETA` %>%
  slice(-1)

contagem_A_TATUAPE <- contagem_A_TATUAPE %>%
  slice(-1)

`contagem_A_TITO LOPES` <- `contagem_A_TITO LOPES` %>%
  slice(-1)

contagem_A_VERGUEIRO <- contagem_A_VERGUEIRO %>%
  slice(-1)

####CHANGE THE DATAFRAME######
contagem_A_JACANA <- contagem_A_JACANA %>%
  mutate(VIRUS = case_when(
    RESULT_INF_A == "DETECTABLE" ~ "FLUA",
    RESULT_INF_A == "INCONCLUSIVE" ~ "FLUA",
    RESULT_INF_A == "NOT-DETECTABLE" ~ "FLUA"
  )) %>%
  select(RESULT = RESULT_INF_A, COUNT, VIRUS)


contagem_A_BUTANTAN <- contagem_A_BUTANTAN %>%
  mutate(VIRUS = case_when(
    RESULT_INF_A == "DETECTABLE" ~ "FLUA",
    RESULT_INF_A == "INCONCLUSIVE" ~ "FLUA",
    RESULT_INF_A == "NOT-DETECTABLE" ~ "FLUA"
  )) %>%
  select(RESULT = RESULT_INF_A, COUNT, VIRUS)

`contagem_A_MARIA ANTONIETA` <- `contagem_A_MARIA ANTONIETA` %>%
  mutate(VIRUS = case_when(
    RESULT_INF_A == "DETECTABLE" ~ "FLUA",
    RESULT_INF_A == "INCONCLUSIVE" ~ "FLUA",
    RESULT_INF_A == "NOT-DETECTABLE" ~ "FLUA"
  )) %>%
  select(RESULT = RESULT_INF_A, COUNT, VIRUS)

contagem_A_TATUAPE <- contagem_A_TATUAPE %>%
  mutate(VIRUS = case_when(
    RESULT_INF_A == "DETECTABLE" ~ "FLUA",
    RESULT_INF_A == "INCONCLUSIVE" ~ "FLUA",
    RESULT_INF_A == "NOT-DETECTABLE" ~ "FLUA"
  )) %>%
  select(RESULT = RESULT_INF_A, COUNT, VIRUS)

`contagem_A_TITO LOPES` <- `contagem_A_TITO LOPES` %>%
  mutate(VIRUS = case_when(
    RESULT_INF_A == "DETECTABLE" ~ "FLUA",
    RESULT_INF_A == "INCONCLUSIVE" ~ "FLUA",
    RESULT_INF_A == "NOT-DETECTABLE" ~ "FLUA"
  )) %>%
  select(RESULT = RESULT_INF_A, COUNT, VIRUS)

contagem_A_VERGUEIRO <- contagem_A_VERGUEIRO %>%
  mutate(VIRUS = case_when(
    RESULT_INF_A == "DETECTABLE" ~ "FLUA",
    RESULT_INF_A == "INCONCLUSIVE" ~ "FLUA",
    RESULT_INF_A == "NOT-DETECTABLE" ~ "FLUA"
  )) %>%
  select(RESULT = RESULT_INF_A, COUNT, VIRUS)


###################################FLUB#################
# Loop over each UBS in the list
resultados_UBS_B <- lapply(ubs_list, function(ubs_nome) {
  processar_ubs(d_sel, ubs_nome, RESULT_INF_B)
})

# Assign results to named objects
names(resultados_UBS_B) <- paste0("contagem_B_", ubs_list)

# Assign individual results to named objects (optional)
list2env(resultados_UBS_B, envir = .GlobalEnv)

# Remove line without info
contagem_B_JACANA <- contagem_B_JACANA %>%
  slice(-1)

`contagem_B_MARIA ANTONIETA` <- `contagem_B_MARIA ANTONIETA` %>%
  slice(-1)

contagem_B_TATUAPE <- contagem_B_TATUAPE %>%
  slice(-1)

`contagem_B_TITO LOPES` <- `contagem_B_TITO LOPES` %>%
  slice(-1)

contagem_B_VERGUEIRO <- contagem_B_VERGUEIRO %>%
  slice(-1)

####CHANGE THE DATAFRAME######
contagem_B_JACANA <- contagem_B_JACANA %>%
  mutate(VIRUS = case_when(
    RESULT_INF_B == "DETECTABLE" ~ "FLUB",
    RESULT_INF_B == "INCONCLUSIVE" ~ "FLUB",
    RESULT_INF_B == "NOT-DETECTABLE" ~ "FLUB"
  )) %>%
  select(RESULT = RESULT_INF_B, COUNT, VIRUS)

contagem_B_BUTANTAN <- contagem_B_BUTANTAN %>%
  mutate(VIRUS = case_when(
    RESULT_INF_B == "DETECTABLE" ~ "FLUB",
    RESULT_INF_B == "INCONCLUSIVE" ~ "FLUB",
    RESULT_INF_B == "NOT-DETECTABLE" ~ "FLUB"
  )) %>%
  select(RESULT = RESULT_INF_B, COUNT, VIRUS)

`contagem_B_MARIA ANTONIETA` <- `contagem_B_MARIA ANTONIETA` %>%
  mutate(VIRUS = case_when(
    RESULT_INF_B == "DETECTABLE" ~ "FLUB",
    RESULT_INF_B == "INCONCLUSIVE" ~ "FLUB",
    RESULT_INF_B == "NOT-DETECTABLE" ~ "FLUB"
  )) %>%
  select(RESULT = RESULT_INF_B, COUNT, VIRUS)

contagem_B_TATUAPE <- contagem_B_TATUAPE %>%
  mutate(VIRUS = case_when(
    RESULT_INF_B == "DETECTABLE" ~ "FLUB",
    RESULT_INF_B == "INCONCLUSIVE" ~ "FLUB",
    RESULT_INF_B == "NOT-DETECTABLE" ~ "FLUB"
  )) %>%
  select(RESULT = RESULT_INF_B, COUNT, VIRUS)

`contagem_B_TITO LOPES` <- `contagem_B_TITO LOPES` %>%
  mutate(VIRUS = case_when(
    RESULT_INF_B == "DETECTABLE" ~ "FLUB",
    RESULT_INF_B == "INCONCLUSIVE" ~ "FLUB",
    RESULT_INF_B == "NOT-DETECTABLE" ~ "FLUB"
  )) %>%
  select(RESULT = RESULT_INF_B, COUNT, VIRUS)

contagem_B_VERGUEIRO <- contagem_B_VERGUEIRO %>%
  mutate(VIRUS = case_when(
    RESULT_INF_B == "DETECTABLE" ~ "FLUB",
    RESULT_INF_B == "INCONCLUSIVE" ~ "FLUB",
    RESULT_INF_B == "NOT-DETECTABLE" ~ "FLUB"
  )) %>%
  select(RESULT = RESULT_INF_B, COUNT, VIRUS)


##########################COV##########################
# Loop over each UBS in the list
resultados_UBS_C <- lapply(ubs_list, function(ubs_nome) {
  processar_ubs(d_sel, ubs_nome, RESULT_COV)
})

# Assign results to named objects
names(resultados_UBS_C) <- paste0("contagem_C_", ubs_list)

# Assign individual results to named objects (optional)
list2env(resultados_UBS_C, envir = .GlobalEnv)

#Remove line without info
contagem_C_JACANA <- contagem_C_JACANA %>%
  slice(-1)

`contagem_C_MARIA ANTONIETA` <- `contagem_C_MARIA ANTONIETA` %>%
  slice(-1)

contagem_C_TATUAPE <- contagem_C_TATUAPE %>%
  slice(-1)

`contagem_C_TITO LOPES` <- `contagem_C_TITO LOPES` %>%
  slice(-1)

contagem_C_VERGUEIRO <- contagem_C_VERGUEIRO %>%
  slice(-1)

####CHANGE THE DATAFRAME######
contagem_C_JACANA <- contagem_C_JACANA %>%
  mutate(VIRUS = case_when(
    RESULT_COV == "DETECTABLE" ~ "COV",
    RESULT_COV == "INCONCLUSIVE" ~ "COV",
    RESULT_COV == "NOT-DETECTABLE" ~ "COV"
  )) %>%
  select(RESULT = RESULT_COV, COUNT, VIRUS)

contagem_C_BUTANTAN <- contagem_C_BUTANTAN %>%
  mutate(VIRUS = case_when(
    RESULT_COV == "DETECTABLE" ~ "COV",
    RESULT_COV == "INCONCLUSIVE" ~ "COV",
    RESULT_COV == "NOT-DETECTABLE" ~ "COV"
  )) %>%
  select(RESULT = RESULT_COV, COUNT, VIRUS)

`contagem_C_MARIA ANTONIETA` <- `contagem_C_MARIA ANTONIETA` %>%
  mutate(VIRUS = case_when(
    RESULT_COV == "DETECTABLE" ~ "COV",
    RESULT_COV == "INCONCLUSIVE" ~ "COV",
    RESULT_COV == "NOT-DETECTABLE" ~ "COV"
  )) %>%
  select(RESULT = RESULT_COV, COUNT, VIRUS)

contagem_C_TATUAPE <- contagem_C_TATUAPE %>%
  mutate(VIRUS = case_when(
    RESULT_COV == "DETECTABLE" ~ "COV",
    RESULT_COV == "INCONCLUSIVE" ~ "COV",
    RESULT_COV == "NOT-DETECTABLE" ~ "COV"
  )) %>%
  select(RESULT = RESULT_COV, COUNT, VIRUS)

`contagem_C_TITO LOPES` <- `contagem_C_TITO LOPES` %>%
  mutate(VIRUS = case_when(
    RESULT_COV == "DETECTABLE" ~ "COV",
    RESULT_COV == "INCONCLUSIVE" ~ "COV",
    RESULT_COV == "NOT-DETECTABLE" ~ "COV"
  )) %>%
  select(RESULT = RESULT_COV, COUNT, VIRUS)

contagem_C_VERGUEIRO <- contagem_C_VERGUEIRO %>%
  mutate(VIRUS = case_when(
    RESULT_COV == "DETECTABLE" ~ "COV",
    RESULT_COV == "INCONCLUSIVE" ~ "COV",
    RESULT_COV == "NOT-DETECTABLE" ~ "COV"
  )) %>%
  select(RESULT = RESULT_COV, COUNT, VIRUS)

########################ADD FLUA FLUB COV###################### ########

jacana_combined <- rbind(contagem_A_JACANA, contagem_B_JACANA, contagem_C_JACANA)

butantan_combined <- rbind(contagem_A_BUTANTAN, contagem_B_BUTANTAN, contagem_C_BUTANTAN)

ma_combined <- rbind(`contagem_A_MARIA ANTONIETA`, `contagem_B_MARIA ANTONIETA`, `contagem_C_MARIA ANTONIETA`)

tatuape_combined <- rbind(contagem_A_TATUAPE, contagem_B_TATUAPE, contagem_C_TATUAPE)

tl_combined <- rbind(`contagem_A_TITO LOPES`, `contagem_B_TITO LOPES`, `contagem_C_TITO LOPES`)

vergueiro_combined <- rbind(contagem_A_VERGUEIRO, contagem_B_VERGUEIRO, contagem_C_VERGUEIRO)


# Plot
bar_ja <- ggplot(data = jacana_combined, aes(x = VIRUS, y = COUNT, fill = RESULT)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(COUNT / sum(COUNT) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_y_continuous(position = "left", name = "Number of cases") +
  labs(title = "UBS Jaçanã", x = "") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  ) +
  scale_fill_manual(
    values = c(
      "DETECTABLE" = "#b3cde3",
      "INCONCLUSIVE" = "#1f78b4",
      "NOT-DETECTABLE" = "#b2df8a"
    ),
    name = ""
  )

jacana <- bar_ja + theme( rect = element_rect(fill = "transparent"))
jacana <- jacana +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA), # cor mais clara para o fundo do painel
    plot.background = element_rect(fill = "transparent", color = NA), # bg do gráfico
    #panel.grid.major = element_blank(), # remover grade principal
    #panel.grid.minor = element_blank(), # remover grade secundária
    legend.background = element_rect(fill = "transparent") # remover fundo da legenda
  )
jacana


bar_bu <- ggplot(data = butantan_combined, aes(x = VIRUS, y = COUNT, fill = RESULT)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(COUNT / sum(COUNT) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_y_continuous(position = "left", name = "Number of cases") +
  labs(title = "UBS Butantan", x = "") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  ) +
  scale_fill_manual(
    values = c(
      "DETECTABLE" = "#b3cde3",
      "INCONCLUSIVE" = "#1f78b4",
      "NOT-DETECTABLE" = "#b2df8a"
    ),
    name = ""
  )

butantan <- bar_bu + theme( rect = element_rect(fill = "transparent"))
butantan <- butantan +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA), # cor mais clara para o fundo do painel
    plot.background = element_rect(fill = "transparent", color = NA), # bg do gráfico
    #panel.grid.major = element_blank(), # remover grade principal
    #panel.grid.minor = element_blank(), # remover grade secundária
    legend.background = element_rect(fill = "transparent") # remover fundo da legenda
  )
butantan


bar_ma <- ggplot(data = ma_combined, aes(x = VIRUS, y = COUNT, fill = RESULT)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(COUNT / sum(COUNT) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_y_continuous(position = "left", name = "Number of cases") +
  labs(title = "UBS Maria Antonieta", x = "") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  ) +
  scale_fill_manual(
    values = c(
      "DETECTABLE" = "#b3cde3",
      "INCONCLUSIVE" = "#1f78b4",
      "NOT-DETECTABLE" = "#b2df8a"
    ),
    name = ""
  )

ma <- bar_ma + theme( rect = element_rect(fill = "transparent"))
ma <- ma +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA), # cor mais clara para o fundo do painel
    plot.background = element_rect(fill = "transparent", color = NA), # bg do gráfico
    #panel.grid.major = element_blank(), # remover grade principal
    #panel.grid.minor = element_blank(), # remover grade secundária
    legend.background = element_rect(fill = "transparent") # remover fundo da legenda
  )
ma



bar_ta <- ggplot(data = tatuape_combined, aes(x = VIRUS, y = COUNT, fill = RESULT)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(COUNT / sum(COUNT) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_y_continuous(position = "left", name = "Number of cases") +
  labs(title = "UBS Tatuapé", x = "") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  ) +
  scale_fill_manual(
    values = c(
      "DETECTABLE" = "#b3cde3",
      "INCONCLUSIVE" = "#1f78b4",
      "NOT-DETECTABLE" = "#b2df8a"
    ),
    name = ""
  )

tatuape <- bar_ta + theme( rect = element_rect(fill = "transparent"))
tatuape <- tatuape +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA), # cor mais clara para o fundo do painel
    plot.background = element_rect(fill = "transparent", color = NA), # bg do gráfico
    #panel.grid.major = element_blank(), # remover grade principal
    #panel.grid.minor = element_blank(), # remover grade secundária
    legend.background = element_rect(fill = "transparent") # remover fundo da legenda
  )
tatuape


bar_tl <- ggplot(data = tl_combined, aes(x = VIRUS, y = COUNT, fill = RESULT)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(COUNT / sum(COUNT) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_y_continuous(position = "left", name = "Number of cases") +
  labs(title = "UBS Tito Lopes", x = "") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  ) +
  scale_fill_manual(
    values = c(
      "DETECTABLE" = "#b3cde3",
      "INCONCLUSIVE" = "#1f78b4",
      "NOT-DETECTABLE" = "#b2df8a"
    ),
    name = ""
  )

tl <- bar_tl + theme( rect = element_rect(fill = "transparent"))
tl <- tl +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA), # cor mais clara para o fundo do painel
    plot.background = element_rect(fill = "transparent", color = NA), # bg do gráfico
    #panel.grid.major = element_blank(), # remover grade principal
    #panel.grid.minor = element_blank(), # remover grade secundária
    legend.background = element_rect(fill = "transparent") # remover fundo da legenda
  )
tl



bar_vergueiro <- ggplot(data = vergueiro_combined, aes(x = VIRUS, y = COUNT, fill = RESULT)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(COUNT / sum(COUNT) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_y_continuous(position = "left", name = "Number of cases") +
  labs(title = "UBS Vergueiro", x = "") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  ) +
  scale_fill_manual(
    values = c(
      "DETECTABLE" = "#b3cde3",
      "INCONCLUSIVE" = "#1f78b4",
      "NOT-DETECTABLE" = "#b2df8a"
    ),
    name = ""
  )

vergueiro <- bar_vergueiro + theme( rect = element_rect(fill = "transparent"))
vergueiro <- vergueiro +
  theme(
    panel.background = element_rect(fill = "#f0f0f0", color = NA), # lighter color for panel background
    plot.background = element_rect(fill = "transparent", color = NA), # graph bg
    #panel.grid.major = element_blank(), # remove main grid
    #panel.grid.minor = element_blank(), # remove secundary grid
    legend.background = element_rect(fill = "transparent") # remover fundo da legenda
  )
vergueiro


# Join all graphs into one
combined_plot <- grid.arrange(
  jacana, butantan, ma,
  tatuape, tl, vergueiro,
  ncol = 2
)

# Save 
ggsave("Figura2_2_APENAS_2023.pdf", combined_plot, width = 16, height = 9, units = "in", dpi = 400)


