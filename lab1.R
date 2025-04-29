# Установка и загрузка пакетов
#install.packages(c("tidyverse", "forecast"))
library(tidyverse)  # Для работы с данными и визуализации
library(forecast)   # Для ARIMA и прогнозирования

# Параметры
n <- 1000
t1 <- 0.3   # |θ| < 1
t2 <- 1     # |θ| = 1
t3 <- 3     # |θ| > 1
tetta <- c(t1, t2, t3)

# Задание 1: AR(1) процесс
ar <- function(n, theta) {
  x <- numeric(n)
  x[1] <- rnorm(1)
  for (k in 2:n) {
    x[k] <- theta * x[k-1] + rnorm(1)
  }
  return(x)
}

# Генерация данных
AR1 <- ar(n, t1)
AR2 <- ar(n, t2)
AR3 <- ar(n, t3)

# Функция для построения графика
printFunc <- function(data, title = "AR(1) процесс") {
  ggplot(data = tibble(x = seq_along(data), y = data), aes(x, y)) +
    geom_line(color = "green") +
    labs(title = title, x = "Наблюдение", y = "Значения") +
    theme_minimal()
}

# Построение графиков
printFunc(AR1, "AR(1) с theta = 0.3")
printFunc(AR2, "AR(1) с theta = 1")
printFunc(AR3, "AR(1) с theta = 3")

# Задание 2: МНК-оценка параметра θ
MNK <- function(AR, k = 2, n = length(AR)) {
  # Аналитическое решение: θ = Σ(x_i * x_{i-1}) / Σ(x_{i-1}^2)
  x <- AR[k:n]
  x_lag <- AR[(k-1):(n-1)]
  theta_hat <- sum(x * x_lag) / sum(x_lag^2)
  return(theta_hat)
}

MNK1 <- MNK(AR1)
cat("МНК-оценка theta для AR1:", MNK1, "\n")

# Задание 3: ММП-оценка параметра θ
MMP <- function(AR) {
  # ММП совпадает с МНК при гауссовском шуме
  theta_hat <- MNK(AR)
  cat("ММП-оценка theta:", theta_hat, "\n")
  cat("Вывод: при гауссовском шуме МНК и ММП оценки совпадают, так как минимизируется одна и та же сумма квадратов.\n")
  return(theta_hat)
}

MMP1 <- MMP(AR1)

# Задание 4: МНК-оценки для k = 10, ..., n
vector_MNK <- function(AR, kmin, kmax) {
  map_dbl(kmin:kmax, ~ MNK(AR, k = .x, n = length(AR)))
}

# Новый процесс для задания 4
ntheta <- 0.8
nAR <- ar(n, ntheta)
MNK_estimates <- vector_MNK(nAR, 10, n)
cat("МНК-оценка для k = 10:", MNK_estimates[1], "\n")

# График оценок
printFunc(MNK_estimates, "МНК-оценки theta для k = 10, ..., 1000")

# Задание 5: AR(2) процесс и проверка стационарности
ar2 <- function(theta1, theta2, n) {
  x <- numeric(n)
  x[1:2] <- rnorm(2)
  for (k in 3:n) {
    x[k] <- theta1 * x[k-1] + theta2 * x[k-2] + rnorm(1)
  }
  return(x)
}

# Параметры и генерация данных
theta1 <- 0.4
theta2 <- 0.1
nAR2 <- ar2(theta1, theta2, n)

# Проверка стационарности
stationarity <- function(theta1, theta2) {
  # Характеристическое уравнение: λ^2 - θ1λ - θ2 = 0
  discrim <- theta1^2 + 4 * theta2
  if (discrim < 0) {
    # Комплексные корни
    m <- sqrt(theta1^2 / 4 + abs(discrim) / 4)
    return(m < 1)
  } else {
    # Действительные корни
    lambda1 <- (theta1 + sqrt(discrim)) / 2
    lambda2 <- (theta1 - sqrt(discrim)) / 2
    return(abs(lambda1) < 1 && abs(lambda2) < 1)
  }
}

# Построение графика после проверки
if (stationarity(theta1, theta2)) {
  cat("Процесс AR(2) стационарен\n")
  printFunc(nAR2, "Стационарный AR(2) процесс")
} else {
  cat("Процесс AR(2) не стационарен\n")
}

# Задание 6: ARIMA для AR(2)
theta1 <- 0.6
theta2 <- -0.4
x <- ar2(theta1, theta2, n)

# Подгонка ARIMA модели
arima_model <- arima(x, order = c(2, 0, 0), include.mean = FALSE)

# Прогноз на 10 шагов
forecasted_values <- forecast(arima_model, h = 10)

# Визуализация прогноза
autoplot(forecasted_values) +
  labs(title = "Прогноз ARIMA(2,0,0)", x = "Время", y = "Значения") +
  theme_minimal()

