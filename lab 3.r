if (!require(tseries)) {
  install.packages("tseries")
}
library(tseries)

# Задание 1: Моделирование процесса AR(2)ARCH(3)
n <- 2100
theta <- c(-0.3, 0.4)  # Параметры AR(2)
A <- c(1, 0.2, 0.1, 0.2)  # Параметры ARCH(3)

epsilon <- rnorm(n)

# Инициализация векторов
x <- numeric(n)
sigma2 <- numeric(n)

# Начальные значения
sigma2[1:3] <- 1  # Начальная дисперсия
x[1:3] <- rnorm(3)  # Случайные начальные значения

# Генерация процесса AR(2)ARCH(3)
for (i in 4:n) {
  sigma2[i] <- A[1] + A[2] * x[i-1]^2 + A[3] * x[i-2]^2 + A[4] * x[i-3]^2
  x[i] <- theta[1] * x[i-1] + theta[2] * x[i-2] + sqrt(sigma2[i]) * epsilon[i]
}

# График процесса
plot(x, type = "l", col = "blue", main = "AR(2)ARCH(3) процесс", 
     xlab = "Время", ylab = "x_n")

# Задание 2: Разделение на обучающую и тестовую выборки
train_size <- floor(20/21 * n)
test_size <- n - train_size

train_data <- x[1:train_size]
test_data <- x[(train_size + 1):n]

# Графики выборок
plot(train_data, type = "l", col = "green", main = "Обучающая выборка", 
     xlab = "Время", ylab = "x_n")
plot(test_data, type = "l", col = "red", main = "Тестовая выборка", 
     xlab = "Время", ylab = "x_n")

# Задание 3: Оценка параметров
# Шаг 3a: Оценка параметров AR(2)
ar_model <- arima(train_data, order = c(2, 0, 0), include.mean = FALSE)
theta_hat <- coef(ar_model)[1:2]
cat("Оцененные параметры θ:", theta_hat, "\n")

# Шаг 3b: Оценка параметров ARCH(3)
residuals <- residuals(ar_model)
garch_model <- garch(residuals, order = c(0, 3))
A_hat <- coef(garch_model)
cat("Оцененные параметры A:", A_hat, "\n")

# График оцененной дисперсии
sigma_hat <- fitted(garch_model)[,1]^2
plot(sigma_hat, type = "l", col = "orange", 
     main = "Оцененная дисперсия ARCH(3)", xlab = "Время", ylab = "σ^2")

# Задание 4: Прогнозирование на один шаг
x_forecast <- numeric(test_size)
sigma_forecast <- numeric(test_size)

for (i in 1:test_size) {
  idx <- train_size + i
  Xn <- c(x[idx-1], x[idx-2])
  x_forecast[i] <- sum(theta_hat * Xn)
  sigma2_temp <- A_hat[1] + A_hat[2] * x[idx-1]^2 + A_hat[3] * x[idx-2]^2 + A_hat[4] * x[idx-3]^2
  sigma_forecast[i] <- sqrt(max(sigma2_temp, 0))
}

# Границы волатильности
upper_bound <- x_forecast + sigma_forecast
lower_bound <- x_forecast - sigma_forecast

# График прогнозов с исправленным ylim
ylim_range <- range(c(test_data, x_forecast, upper_bound, lower_bound), na.rm = TRUE, finite = TRUE)
if (!all(is.finite(ylim_range))) {
  warning("Некоторые значения для ylim не конечны, заменяем на разумные границы")
  ylim_range <- c(min(test_data, na.rm = TRUE), max(test_data, na.rm = TRUE))
}

plot(test_data, type = "l", col = "turquoise", 
     main = "Прогноз на 1 шаг вперед", xlab = "Наблюдения", ylab = "x_n",
     ylim = ylim_range)
points(x_forecast, col = "black", pch = 1)
lines(upper_bound, col = "red", lty = 2)
lines(lower_bound, col = "red", lty = 2)

legend("topleft", legend = c("Реальные значения", "Прогнозы", "Границы волатильности"),
       col = c("turquoise", "black", "red"), lty = c(1, NA, 2), pch = c(NA, 1, NA))

# Задание 5-7: Загрузка и визуализация данных MSFT
data <- read.table("A:/Applications/MM2/labs/MMII-labs/MSFT.csv", header = TRUE, sep = ",", dec = ",")

# Преобразование даты
data$Дата <- as.Date(data$Дата, format = "%d.%m.%Y")

# Проверка типа данных столбца Цена и преобразование, если нужно
if (!is.numeric(data$Цена)) {
  data$Цена <- as.numeric(as.character(data$Цена))
  if (any(is.na(data$Цена))) {
    warning("После преобразования столбца Цена появились NA, удаляем их")
    data <- data[!is.na(data$Цена), ]
  }
}
if (any(is.na(data$Цена))) {
  warning("В данных есть пропущенные значения в столбце Цена, удаляем их")
  data <- data[!is.na(data$Цена), ]
}

# График динамики цен
plot(data$Дата, data$Цена, type = "l", col = "blue", 
     main = "Динамика цен акций Microsoft", xlab = "Дата", ylab = "Цена закрытия")

# Задание 8: Приведение данных к стационарному виду
log_returns <- diff(log(data$Цена))
cat("Первые 5 значений логарифмической доходности:", head(log_returns, 5), "\n")

# Проверка на NA в логарифмических доходностях
if (any(is.na(log_returns))) {
  warning("В логарифмических доходностях есть NA, удаляем их")
  log_returns <- na.omit(log_returns)
}

# Задание 9: График доходностей
plot(log_returns, type = "l", col = "green", 
     main = "Логарифмическая доходность акций Microsoft", 
     xlab = "Наблюдения", ylab = "Доходность z_k")

# Задание 10: Моделирование AR(2)ARCH(3) для доходностей
# Разделение данных
n_z <- length(log_returns)
if (n_z < 4) {
  stop("Недостаточно данных для моделирования после удаления NA")
}
train_size_z <- floor(20/21 * n_z)
test_size_z <- n_z - train_size_z

train_data_z <- log_returns[1:train_size_z]
test_data_z <- log_returns[(train_size_z + 1):n_z]

# Оценка AR(2) параметров
ar_model_z <- arima(train_data_z, order = c(2, 0, 0), include.mean = FALSE)
theta_hat_z <- coef(ar_model_z)[1:2]
cat("Оцененные параметры θ для {z_n}:", theta_hat_z, "\n")

# Оценка ARCH(3) параметров
residuals_z <- residuals(ar_model_z)
garch_model_z <- garch(residuals_z, order = c(0, 3))
A_hat_z <- coef(garch_model_z)
cat("Оцененные параметры A для {z_n}:", A_hat_z, "\n")

# График оцененной дисперсии
sigma_hat_z <- fitted(garch_model_z)[,1]^2
plot(sigma_hat_z, type = "l", col = "orange", 
     main = "Оцененная дисперсия ARCH(3) для доходностей", 
     xlab = "Время", ylab = "σ^2")

# Прогнозирование на один шаг
x_forecast_z <- numeric(test_size_z)
sigma_forecast_z <- numeric(test_size_z)

for (i in 1:test_size_z) {
  idx <- train_size_z + i
  Xn_z <- c(log_returns[idx-1], log_returns[idx-2])
  x_forecast_z[i] <- sum(theta_hat_z * Xn_z)
  sigma2_temp <- A_hat_z[1] + A_hat_z[2] * log_returns[idx-1]^2 + 
    A_hat_z[3] * log_returns[idx-2]^2 + A_hat_z[4] * log_returns[idx-3]^2
  sigma_forecast_z[i] <- sqrt(max(sigma2_temp, 0))
}

# Границы волатильности
upper_bound_z <- x_forecast_z + sigma_forecast_z
lower_bound_z <- x_forecast_z - sigma_forecast_z

# График прогнозов с исправленным ylim
ylim_range_z <- range(c(test_data_z, x_forecast_z, upper_bound_z, lower_bound_z), na.rm = TRUE, finite = TRUE)
if (!all(is.finite(ylim_range_z))) {
  warning("Некоторые значения для ylim не конечны, заменяем на разумные границы")
  ylim_range_z <- c(min(test_data_z, na.rm = TRUE), max(test_data_z, na.rm = TRUE))
}

plot(test_data_z, type = "l", col = "turquoise", 
     main = "Прогноз на 1 шаг для доходностей", 
     xlab = "Наблюдения", ylab = "Доходность",
     ylim = ylim_range_z)
points(x_forecast_z, col = "black", pch = 1)
lines(upper_bound_z, col = "red", lty = 2)
lines(lower_bound_z, col = "red", lty = 2)

legend("topleft", legend = c("Реальные значения", "Прогнозы", "Границы волатильности"),
       col = c("turquoise", "black", "red"), lty = c(1, NA, 2), pch = c(NA, 1, NA))
