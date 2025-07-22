# Instalando e carregando pacotes
install.packages("tidyverse")
library(tidyverse)

# Importando os dados
data_2019 <- read_csv('Dados_Viagens_2019_Q1.csv')
data_2020 <- read_csv('Dados_Viagens_2020_Q1.csv')

# Padronizando as colunas
data_2019 <- data_2019 %>%
  rename(
    ride_id = trip_id,
    started_at = start_time,
    ended_at = end_time,
    rideable_type = bikeid,
    rider_type = usertype
  ) %>%
  mutate(
    ride_id = as.character(ride_id),
    rideable_type = as.character(rideable_type)
  ) %>%
  select(ride_id, rideable_type, started_at, ended_at, rider_type)

data_2020 <- data_2020 %>%
  rename(rider_type = member_casual) %>%
  mutate(
    ride_id = as.character(ride_id),
    rideable_type = as.character(rideable_type)
  ) %>%
  select(ride_id, rideable_type, started_at, ended_at, rider_type)

# Unindo as duas tabelas 
dados_completos <- bind_rows(data_2019, data_2020)

# Transformando os dados
dados_completos <- dados_completos %>%
  mutate(
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = weekdays(started_at),
    day_num = as.POSIXlt(started_at)$wday + 1
  )

# Filtrando os dados
dados_completos_clean <- dados_completos %>%
  filter(ride_length > 0, ride_length < 1440)


# Padronizando os tipos de usuários
dados_completos_clean <- dados_completos_clean %>%
  mutate(
    rider_type = tolower(rider_type),  
    rider_type = case_when(
      rider_type %in% c("customer", "casual") ~ "casual",
      rider_type %in% c("subscriber", "member") ~ "member",
      TRUE ~ rider_type
    )
  )

table(dados_completos_clean$rider_type)

# Cálculos básicos
mean(dados_completos_clean$ride_length)

max(dados_completos_clean$ride_length)

dados_completos_clean %>%
  count(day_of_week) %>%
  arrange(desc(n)) %>%
  slice(1)

dados_completos_clean %>%
  group_by(rider_type) %>%
  summarise(media_duracao = mean(ride_length))

dados_completos_clean %>%
  group_by(rider_type) %>%
  summarise(maxima_duracao = max(ride_length))

dados_completos_clean %>%
  group_by(rider_type, day_of_week) %>%
  summarise(total = n()) %>%
  arrange(rider_type, desc(total)) %>%
  slice_max(order_by = total, n = 1)

media <- dados_completos_clean %>%
  group_by(rider_type) %>%
  summarise(duracao_media = round(mean(ride_length), 2))

maxima <- dados_completos_clean %>%
  group_by(rider_type) %>%
  summarise(duracao_maxima = round(max(ride_length), 2))

moda <- dados_completos_clean %>%
  group_by(rider_type, day_of_week) %>%
  summarise(total = n()) %>%
  arrange(rider_type, desc(total)) %>%
  slice_max(order_by = total, n = 1)

# Criando a tabela resumo
tabela_resumo <- media %>%
  left_join(maxima, by = "rider_type") %>%
  left_join(moda %>% select(rider_type, dia_mais_popular = day_of_week), by = "rider_type")


install.packages("ggplot2")
library(ggplot2)

library(ggplot2)
library(tidyverse)

# Criando as visualizações
grafico_1 <- dados_completos_clean %>%
  group_by(rider_type) %>%
  summarise(media_duracao = mean(ride_length)) %>%
  ggplot(aes(x = rider_type, y = media_duracao, fill = rider_type)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Duração média das viagens por tipo de usuário",
       x = "Tipo de usuário", y = "Minutos") +
  theme_minimal()

grafico_2 <- dados_completos_clean %>%
  group_by(day_of_week, rider_type) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = day_of_week, y = total, fill = rider_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de viagens por dia da semana",
       x = "Dia", y = "Quantidade") +
  theme_minimal()

grafico_3 <- dados_completos_clean %>%
  count(rider_type) %>%
  ggplot(aes(x = "", y = n, fill = rider_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Proporção de usuários") +
  theme_void()

library(ggplot2)

ordem_dias <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Gráfico aprimorado
grafico_2 <- dados_completos_clean %>%
  group_by(day_of_week, rider_type) %>%
  summarise(total = n(), .groups = "drop") %>%
  mutate(day_of_week = factor(day_of_week, levels = ordem_dias)) %>%
  ggplot(aes(x = day_of_week, y = total, fill = rider_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Volume de viagens por tipo de usuário e dia da semana",
    x = "Dia da semana", y = "Quantidade de viagens"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Criar tabela com percentual
dados_pizza <- dados_completos_clean %>%
  count(rider_type) %>%
  mutate(percentual = round(n / sum(n) * 100, 1),
         label = paste0(rider_type, "\n", percentual, "%"))

grafico_3 <- ggplot(dados_pizza, aes(x = "", y = n, fill = rider_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), size = 5, color = "white") +
  labs(title = "Proporção de usuários (casual vs member)") +
  theme_void()


# Os gráficos foram exportados manualmente pela aba:
# Plots > Export > Save as Image
