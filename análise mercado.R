setwd("C:/Users/erick/OneDrive/Área de Trabalho/UFPR/XR-Brain/Mercado")
getwd()

library(ggplot2)
library(tidyr)
library(dplyr)

# Lendo os dados
dados <- read.csv('Dados_mercado.csv', check.names = FALSE, sep = ",", header = TRUE) %>%
  rename(`Valor de Mercado em 2021` = `Valor de Mercado em 2021 (em bilhões de dólares)`,
         `Valor de Mercado em 2026` = `Valor de Mercado em 2026 (em bilhões de dólares)`) %>%
  select(`Setor`, `Valor de Mercado em 2021`, `Valor de Mercado em 2026`)
dim(dados)
head(dados)
View(dados)

# Filtrando o dataframe para remover o setor "Valor Global"
dados_filtrados <- dados %>%
  filter(`Setor` != "Valor Global")

# Convertendo para formato longo
dados_long <- dados_filtrados %>%
  pivot_longer(cols = c(`Valor de Mercado em 2021`, `Valor de Mercado em 2026`),
               names_to = "Ano",
               values_to = "Valor") %>%
  mutate(Ano = as.factor(Ano))

# Definindo a paleta de cores
cores <- c("#142850", "#27496d", "#0c7b93", "#00a8cc", "#00c9a7")

# Plotando o gráfico de barras
ggplot(dados_long, aes(x = Ano, y = Valor, fill = Setor)) +
  ylim(0, 25) +
  xlim(0.5, 2.5) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.8) +
  scale_fill_manual(values = cores) +
  labs(title = "Valor do mercado de XR (Realidade Estendida)",
       x = "Ano",
       y = "Valor (em bilhões de dólares)",
       fill = "Setor") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14, lineheight = 100),
        legend.text = element_text(size = 12, lineheight = 100),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "#E5E5E5", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  scale_x_discrete(labels = c("2021", "2026")) +
  labs(caption = "Fonte: (Grand View Research, 2020; MarketsandMarkets, 2021; 
       Mordor Intelligence, 2021; ResearchAndMarkets, 2020).") +
  scale_color_manual(values = c("#FFFFFF", "#E5E5E5"))

library(ggplot2)

# 1. Carregar conjunto de dados
dados <- read.csv("Dados_mercado.csv", stringsAsFactors = FALSE)

# Removendo o símbolo de porcentagem do CAGR
dados$CAGR <- as.numeric(gsub("%", "", dados$CAGR))

# Definindo a ordem das barras
dados$Setor <- factor(dados$Setor, levels = c("Educação em XR", 
                                              "Treinamento Corporativo em XR", 
                                              "Treinamento de Saúde em XR", 
                                              "Treinamento Industrial em XR",
                                              "Valor Global"))

# Definindo a paleta de cores
cores <- c("#142850", "#27496d", "#0c7b93", "#00a8cc", "#00c9a7")

# Plotando o gráfico de barras
ggplot(dados, aes(x = Setor, y = CAGR, fill = Setor)) +
  ylim(0, 70) +
  geom_bar(stat = "identity", width = 0.6, color = "white") +
  scale_fill_manual(values = cores) +
  labs(title = "CAGR do mercado de XR (Realidade Estendida)",
       y = "CAGR (em %)",
       fill = "") +
  theme_bw() +
  theme(legend.position = "right",
        legend.margin = margin(0, 0, 0, 2, "cm"),  # ajuste da margem direita
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text.x = element_blank(),  # remover nomes do eixo x
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_line(colour = "#E5E5E5", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  geom_text(aes(label = paste0(format(round(CAGR, 1), nsmall = 1), "%"), y = CAGR), 
            position = position_dodge(width = 0.6), vjust = -0.5, size = 4.5) +
  labs(caption = "Fonte: (Grand View Research, 2020; MarketsandMarkets, 2021; 
       Mordor Intelligence, 2021; ResearchAndMarkets, 2020).")




