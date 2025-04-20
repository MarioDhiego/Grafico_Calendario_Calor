

library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)

# 1. Criar banco de dados simulado
datas_2025 <- seq.Date(from = as.Date("2025-01-01"), 
                       to = as.Date("2025-12-31"), 
                       by = "day")

set.seed(123)  # para reprodutibilidade
df <- data.frame(
  data = datas_2025,
  estado = "Pará",
  vitimas = rpois(length(datas_2025), lambda = 5)  
  # média de 5 vítimas por dia
)

# 2. Preparar os dados para o gráfico
df_ba_2025 <- df %>%
  filter(estado == "Pará", year(data) == 2025) %>%
  mutate(
    dia = day(data),
    mes = month(data, label = TRUE, abbr = FALSE),
    semana = wday(data, label = TRUE, abbr = TRUE), # nome do dia da semana
    semana_do_mes = ceiling(day(data) / 7)          # posição da semana dentro do mês
  )

# 3. Criar gráfico com ggplot
p <- ggplot(df_ba_2025, aes(
  x = semana,
  y = -semana_do_mes,
  fill = vitimas,
  text = paste0("Data: ", data, "<br>Vítimas: ", vitimas)
)) +
  geom_tile(color = "white", size = 0.8) +
  facet_wrap(~mes, ncol = 3) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Dinâmica Temporal de Vítimas no Pará em 2025",
    fill = "Nº de Vítimas",
    x = "Dias da Semana",
    y = "Meses"
  ) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

# 4. Tornar interativo com plotly
ggplotly(p, tooltip = "text")
