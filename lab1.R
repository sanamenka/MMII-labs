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
AR1=AR(400,0.5)
AR2=AR(600,1)
AR3=AR(700,1.3)
plot(AR1)
plot(AR2)
plot(AR3)

rm(list = ls()); graphics.off()
