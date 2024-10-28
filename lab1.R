##-----лабораторная работа №1 AR(p)
AR=function(n,tetta) {
  x=numeric(n)
  x[1]=rnorm(1)
  for(k in 2:n) {
    epsilon=rnorm(1)
    x[k]=tetta*x[k-1]+epsilon
  }
  return(x)
}
n=400
AR1=AR(n,0.7)
AR2=AR(n,1)
AR3=AR(n,1.3)
plot(AR1)
plot(AR2)
plot(AR3)

MNK=(sum(AR1[2:n] * AR1[1:(n-1)])/sum(AR1[1:(n-1)]^2))

MP_function = function(tetta) {
  sum((AR1[2:n] - tetta * AR1[1:(n-1)])^2)
}

optimal_tetta = optimize(MP_function, interval=c(-100, 100))$minimum

# Вывод результатов
cat("Оценка МНК:", MNK, "\n")
cat("Оптимальное значение тетта по МП:", optimal_tetta, "\n")


rm(list = ls()); graphics.off()
