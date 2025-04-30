# Лабораторная работа №7: Моделирование страхового процесса и вероятности разорения

# Задание 1: Моделирование N_t ----
time_horizon <- 50  # Фиксированное время
rate_lambda <- 2    # Интенсивность Пуассона
num_sim <- 1000     # Число реализаций
counts_Nt <- numeric(num_sim)  # Массив для хранения N_t

for (i in 1:num_sim) {
  current_time <- 0  # Текущее время
  event_count <- 0   # Счётчик событий
  
  while (current_time < time_horizon) {
    interval <- rexp(1, rate = rate_lambda)  # Генерация интервала
    current_time <- current_time + interval
    if (current_time < time_horizon) event_count <- event_count + 1
  }
  
  counts_Nt[i] <- event_count  # Сохранение результата
}

# Задание 2: Гистограмма и функция вероятности ----

# Построение гистограммы N_t
hist(counts_Nt, breaks = 25, prob = TRUE, col = "lightgreen", 
     main = "Гистограмма N_t с пуассоновской плотностью", 
     xlab = "Количество событий", ylab = "Плотность", ylim = c(0, 0.06))

# Наложение пуассоновской функции вероятности
curve((rate_lambda * time_horizon)^x / factorial(x) * exp(-rate_lambda * time_horizon), 
      add = TRUE, col = "darkblue", lwd = 3)

# Задание 3: Моделирование процесса U_t ----

# Функция для моделирования капитала U_t
model_capital <- function(init_capital, premium_rate, event_rate, claim_mean, max_time) {
  times <- c(0)      # Моменты времени T_i
  capitals <- c(init_capital)  # Значения капитала U(T_i)
  
  while (sum(times) < max_time) {
    inter_event <- rexp(1, event_rate)  # Межсобытийный интервал
    claim <- rexp(1, 1 / claim_mean)    # Страховая выплата
    
    capitals <- c(capitals, capitals[length(capitals)] + premium_rate * inter_event - claim)
    times <- c(times, inter_event)
  }
  
  list(times = cumsum(times), capitals = capitals)  # Возвращаем процесс
}

# Параметры
max_time <- 100    # Максимальное время
claim_mean <- 1    # Средняя выплата
premium_rate1 <- 3 # Случай (a): условие (15) выполнено
premium_rate2 <- 1.5 # Случай (b): условие (15) не выполнено

# Случай (a): условие (15) выполнено
sim_case1 <- model_capital(init_capital = 50, premium_rate = premium_rate1, 
                           event_rate = rate_lambda, claim_mean = claim_mean, max_time = max_time)

# Случай (b): условие (15) не выполнено
sim_case2 <- model_capital(init_capital = 50, premium_rate = premium_rate2, 
                           event_rate = rate_lambda, claim_mean = claim_mean, max_time = max_time)

# Построение графиков
# Случай (a)
plot(sim_case1$times, sim_case1$capitals, type = "s", col = "green", 
     main = "Капитал U_t (условие ρ > 0)", 
     xlab = "Время", ylab = "Капитал")

# Случай (b)
plot(sim_case2$times, sim_case2$capitals, type = "s", col = "purple", 
     main = "Капитал U_t (условие ρ < 0)", 
     xlab = "Время", ylab = "Капитал")

# Задание 4: Вероятность разорения ----

# Параметры
num_trials <- 1000    # Количество реализаций
max_time <- 1000      # Максимальное время
premium_rate <- 1     # Премия
event_rate <- 0.3     # Интенсивность
claim_mean <- 3       # Средняя выплата
init_capital <- 100   # Начальный капитал

# Массив для индикаторов разорения
ruin_flags <- numeric(num_trials)

# Моделирование процесса и проверка разорения
for (j in 1:num_trials) {
  capital <- init_capital
  current_time <- 0
  
  while (current_time < max_time && capital >= 0) {
    inter_event <- rexp(1, rate = event_rate)
    current_time <- current_time + inter_event
    if (current_time >= max_time) break
    
    claim <- rexp(1, rate = 1 / claim_mean)
    capital <- capital + premium_rate * inter_event - claim
    
    if (capital < 0) {
      ruin_flags[j] <- 1
      break
    }
  }
}

# Вычисление выборочной вероятности разорения
ruin_prob <- mean(ruin_flags)

# Вывод результата
cat("Оценка вероятности разорения:", ruin_prob, "\n")

# Проверка условия Лундберга
rho <- premium_rate / (event_rate * claim_mean) - 1
lundberg_bound <- exp(- (1 / claim_mean) * (rho / (1 + rho)) * init_capital)

cat("Верхняя граница по Лундбергу:", lundberg_bound, "\n")

