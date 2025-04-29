# Установка и загрузка пакетов
# install.packages(c("tidyverse", "tseries"))
library(tidyverse)  # Для работы с данными и визуализации
library(tseries)    # Для функции garch()

# --- Определение функций для GARCH процессов ---

# Функция для GARCH(1,0) (ARCH(1))
garch_process_1_0 <- function(a0, a1, num_obs) {
  if (a0 <= 0) stop("a0 должно быть положительным")
  if (a1 <= 0 || a1 >= 1) stop("a1 должно быть в диапазоне (0, 1)")  # Примечание 9
  h <- numeric(num_obs)
  process_values <- numeric(num_obs)
  epsilon <- rnorm(num_obs)
  h[1] <- a0 / (1 - a1)  # Начальная волатильность для стационарности
  process_values[1] <- sqrt(h[1]) * epsilon[1]
  for (t in 2:num_obs) {
    h[t] <- a0 + a1 * process_values[t - 1]^2
    process_values[t] <- sqrt(h[t]) * epsilon[t]
  }
  return(list(process_values = process_values, volatility = sqrt(h)))
}

# Функция для GARCH(3,0) (ARCH(3))
garch_process_3_0 <- function(a0, a1, a2, a3, num_obs) {
  if (a0 <= 0) stop("a0 должно быть положительным")
  if (a1 <= 0 || a2 <= 0 || a3 <= 0 || (a1 + a2 + a3) >= 1) stop("Сумма a1, a2, a3 должна быть меньше 1")  # Примечание 13
  h <- numeric(num_obs)
  process_values <- numeric(num_obs)
  epsilon <- rnorm(num_obs)
  h[1:3] <- a0 / (1 - a1 - a2 - a3)  # Начальная волатильность
  process_values[1:3] <- sqrt(h[1:3]) * epsilon[1:3]
  for (t in 4:num_obs) {
    h[t] <- a0 + a1 * process_values[t - 1]^2 + a2 * process_values[t - 2]^2 + a3 * process_values[t - 3]^2
    process_values[t] <- sqrt(h[t]) * epsilon[t]
  }
  return(list(process_values = process_values, volatility = sqrt(h)))
}

# Функция для GARCH(1,1)
garch_process_1_1 <- function(a0, a1, b1, num_obs) {
  if (a0 <= 0) stop("a0 должно быть положительным")
  if (a1 <= 0 || b1 <= 0 || (a1 + b1) >= 1) stop("a1 и b1 должны быть положительными и a1 + b1 < 1")  # Примечание 15
  h <- numeric(num_obs)
  process_values <- numeric(num_obs)
  epsilon <- rnorm(num_obs)
  h[1] <- a0 / (1 - a1 - b1)  # Начальная волатильность
  process_values[1] <- sqrt(h[1]) * epsilon[1]
  for (t in 2:num_obs) {
    h[t] <- a0 + a1 * process_values[t - 1]^2 + b1 * h[t - 1]
    process_values[t] <- sqrt(h[t]) * epsilon[t]
  }
  return(list(process_values = process_values, volatility = sqrt(h)))
}

# Функция для МНК-оценки GARCH(1,0)
mnk_garch_1_0 <- function(data) {
  # Преобразование к AR(1): h_n^2 = a0 + a1 h_{n-1}^2 + (a0 + a1 h_{n-1}^2) ξ_n
  h2 <- data^2
  n <- length(h2)
  x <- h2[1:(n-1)]  # h_{n-1}^2
  y <- h2[2:n]       # h_n^2
  # МНК: минимизация Σ(y - a0 - a1*x)^2
  fit <- lm(y ~ x)
  a0 <- coef(fit)[1]
  a1 <- coef(fit)[2]
  if (a0 <= 0 || a1 <= 0 || a1 >= 1) warning("Оценки не удовлетворяют условиям стационарности")
  return(c(a0 = a0, a1 = a1))
}

# Функция для прогноза на один шаг GARCH(3,0)
one_step_forecast <- function(data, params, p, n_ahead) {
  h2_forecast <- numeric(n_ahead)
  # Первые p значений для прогноза берутся из данных
  for (t in 1:n_ahead) {
    # Индексы для предыдущих значений: начинаем с конца обучающих данных
    start_idx <- length(data) - n_ahead - p + t
    h2_forecast[t] <- params["a0"] + sum(params[paste0("a", 1:p)] * data[start_idx:(start_idx + p - 1)]^2)
  }
  return(sqrt(h2_forecast))
}

# Параметры
n <- 1100
a0 <- 0.1
a1 <- 0.4
a1_3_0 <- 0.3
a2_3_0 <- 0.2
a3_3_0 <- 0.1
b1 <- 0.5

# Задание 1: Генерация и графики GARCH(1,0)
cat("Задание 1: Построение процесса GARCH(1,0) и волатильности\n")
garch_1_0 <- garch_process_1_0(a0, a1, n = 1000)
df_1_0 <- tibble(time = 1:1000, process = garch_1_0$process_values, volatility = garch_1_0$volatility)
p1 <- ggplot(df_1_0, aes(x = time, y = process)) +
  geom_line(color = "seagreen") +
  labs(title = "Процесс GARCH(1,0)", x = "Время", y = "h_n") +
  theme_minimal()
p2 <- ggplot(df_1_0, aes(x = time, y = volatility)) +
  geom_line(color = "blue") +
  labs(title = "Волатильность GARCH(1,0)", x = "Время", y = "σ_n") +
  theme_minimal()
print(p1)
print(p2)

# Задание 2: МНК-оценка для GARCH(1,0)
cat("\nЗадание 2: МНК-оценка параметров GARCH(1,0)\n")
mnk_estimates <- mnk_garch_1_0(garch_1_0$process_values)
cat("МНК-оценки: a0 =", mnk_estimates["a0"], ", a1 =", mnk_estimates["a1"], "\n")

# Задание 3: Оценка параметров GARCH(1,0) с помощью garch()
cat("\nЗадание 3: Оценка параметров GARCH(1,0) с помощью garch()\n")
garch_fit_1_0 <- garch(garch_1_0$process_values, order = c(0, 1), trace = FALSE)
cat("Оценки garch():\n")
print(coef(garch_fit_1_0))

# Задание 4: Генерация, оценка и прогноз для GARCH(3,0)
cat("\nЗадание 4: Построение GARCH(3,0), оценка параметров и прогноз\n")
garch_3_0 <- garch_process_3_0(a0, a1_3_0, a2_3_0, a3_3_0, n)
df_3_0 <- tibble(time = 1:n, process = garch_3_0$process_values, volatility = garch_3_0$volatility)
p3 <- ggplot(df_3_0, aes(x = time, y = process)) +
  geom_line(color = "seagreen") +
  labs(title = "Процесс GARCH(3,0)", x = "Время", y = "h_n") +
  theme_minimal()
p4 <- ggplot(df_3_0, aes(x = time, y = volatility)) +
  geom_line(color = "blue") +
  labs(title = "Волатильность GARCH(3,0)", x = "Время", y = "σ_n") +
  theme_minimal()
print(p3)
print(p4)

# Разделение данных на обучающую и тестовую выборки
train_size <- 1000
train_data <- garch_3_0$process_values[1:train_size]
test_data <- garch_3_0$process_values[(train_size + 1):n]

# Оценка параметров GARCH(3,0)
garch_fit_3_0 <- garch(train_data, order = c(0, 3), trace = FALSE)
cat("Оценки garch() для GARCH(3,0):\n")
print(coef(garch_fit_3_0))

# Прогноз волатильности на один шаг
params <- coef(garch_fit_3_0)
# Передаем данные: последние p значений из обучающей выборки + тестовая выборка
forecast_vol <- one_step_forecast(garch_3_0$process_values[(train_size - 2):n], params, p = 3, n_ahead = 100)

# График модулей процесса и прогнозов
df_plot <- tibble(
  time = 1:100,
  abs_process = abs(test_data[1:100]),
  forecast_vol = forecast_vol
)
p5 <- ggplot(df_plot, aes(x = time)) +
  geom_line(aes(y = abs_process, color = "Модуль процесса"), size = 1) +
  geom_line(aes(y = forecast_vol, color = "Прогноз волатильности"), linetype = "dashed", size = 1) +
  labs(title = "Модуль GARCH(3,0) и прогноз волатильности", x = "Время", y = "Значения") +
  scale_color_manual(values = c("Модуль процесса" = "blue", "Прогноз волатильности" = "red")) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())
print(p5)

# Задание 5: Генерация и оценка GARCH(1,1)
cat("\nЗадание 5: Построение GARCH(1,1) и оценка параметров\n")
garch_1_1 <- garch_process_1_1(a0, a1, b1, n = 1000)
df_1_1 <- tibble(time = 1:1000, process = garch_1_1$process_values, volatility = garch_1_1$volatility)
p6 <- ggplot(df_1_1, aes(x = time, y = process)) +
  geom_line(color = "seagreen") +
  labs(title = "Процесс GARCH(1,1)", x = "Время", y = "h_n") +
  theme_minimal()
p7 <- ggplot(df_1_1, aes(x = time, y = volatility)) +
  geom_line(color = "blue") +
  labs(title = "Волатильность GARCH(1,1)", x = "Время", y = "σ_n") +
  theme_minimal()
print(p6)
print(p7)

# Оценка параметров GARCH(1,1)
garch_fit_1_1 <- garch(garch_1_1$process_values, order = c(1, 1), trace = FALSE)
cat("Оценки garch() для GARCH(1,1):\n")
print(coef(garch_fit_1_1))

