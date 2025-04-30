if (!require("stats")) install.packages("stats")
library(stats)

# Задание параметров модели
initial_value <- 100
drift <- 0.5
volatility <- 0.8
sqrt_step <- 0.01
time_step <- sqrt_step^2
num_points <- 1001

# Функция для моделирования геометрического броуновского движения
model_gbm <- function(S0, a, sigma, delta, n) {
  # Генерация гауссовских приращений
  noise <- rnorm(n, mean = 0, sd = sqrt(delta))
  
  # Моделирование броуновского движения
  brownian <- cumsum(c(0, noise[-1]))
  
  # Временная шкала
  time_grid <- seq(0, (n-1) * delta, by = delta)
  
  # Вычисление траектории S_t
  drift_term <- (a - 0.5 * sigma^2) * time_grid
  diffusion_term <- sigma * brownian
  trajectory <- S0 * exp(drift_term + diffusion_term)
  
  return(list(time = time_grid, S = trajectory))
}

# Моделирование процесса
gbm_result <- model_gbm(initial_value, drift, volatility, time_step, num_points)

# Извлечение результатов
time_grid <- gbm_result$time
prices <- gbm_result$S

# Построение графика траектории
plot(time_grid, prices, type = "l", col = "darkgreen", lwd = 2,
     xlab = "Время", ylab = "Цена актива S(t)",
     main = "Траектория геометрического броуновского движения")
grid()

# Вычисление логарифмических приращений
log_diff <- diff(log(prices))

# Оценка параметров
mean_log_diff <- mean(log_diff)
var_log_diff <- var(log_diff)

# Вычисление оценок a и sigma^2
est_sigma_sq <- var_log_diff / time_step  # Оценка волатильности
est_drift <- mean_log_diff / time_step + est_sigma_sq / 2  # Оценка сноса

# Вывод результатов
cat("Оценка сноса (a):", round(est_drift, 4), "\n")
cat("Оценка волатильности (sigma^2):", round(est_sigma_sq, 4), "\n")

