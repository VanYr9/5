##############################
## EAA1520 - Taller N° 5    ##
## 19 y 21 de abril de 2023 ##
##############################

########################################
## Propiedades Asintoticas de los EMV ##
########################################

## Plantar una semilla de simulación
set.seed(20230419)

## Ejemplo 1: X1,...,Xn iid Bernoulli(p) = Binomial(size = 1, prob = p)
n = 300
p = 0.2
X = rbinom(n = n, size = 1, prob = p)

## Opción 1: Obtener un expresión para el EMV del parametro "p".
mean(X)

## Opción 2: Utilizar fitdist() de la libreria "fitdistrplus".
aux = fitdistrplus::fitdist(data = X, distr = "binom", 
                            method = "mle", 
                            fix.arg = list(size = 1), 
                            start = list(prob = 0.1))
p.gorro = aux$estimate
p.gorro

## Como producto además esta función evalúa CCR en el EMV
(p.gorro)*(1-p.gorro)/n
aux$sd^2

## Ejemplo 2: X1,...,Xn iid Poisson(lambda)
n = 300
lambda = 20
X = rpois(n = n, lambda = lambda)

## Opción 1: Obtener un expresión para el EMV del parametro "lambda".
mean(X)

## Opción 2: Utilizar fitdist() de la libreria "fitdistrplus".
aux = fitdistrplus::fitdist(data = X, distr = "pois", method = "mle")
lambda.gorro = aux$estimate
lambda.gorro

## Como producto además evalúa CCR en el EMV
lambda.gorro/n
aux$sd^2

## Ejemplo 3: X1,...,Xn iid Binomial(m, p)
n = 300
p = 0.2
m = 10
X = rbinom(n = n, size = m, prob = p)
aux = fitdistrplus::fitdist(data = X, distr = "binom", 
                            method = "mle", 
                            fix.arg = list(size = m), 
                            start = list(prob = 0.5))
p.gorro = aux$estimate
p.gorro

## Ejemplo 4: X1,...,Xn iid Gamma(shape = k, rate = nu) con k conocido
n = 100
k = 8
nu = 0.4
X = rgamma(n = n, shape = k, rate = nu)
aux = fitdistrplus::fitdist(data = X, 
                            distr = "gamma", 
                            method = "mle", 
                            fix.arg = list(shape = k))

aux$estimate
k/mean(X)

## CCR estimada
aux$estimate^2/(n*k)
aux$sd^2

## Ejemplo 5: X1,...,Xn iid Gamma(shape = k, rate = nu) con k desconocido
fitdistrplus::fitdist(data = X, 
                      distr = "gamma", 
                      method = "mle")
                      
######################################
## Distribución Normal (Asintótica) ##
######################################

## Caso Bernoulli(pi) --> pi.emv aprox Normal(pi, pi*(1-pi)/n)

n = 200
p = 0.4
p.emv = c()
for(i in 1:10000){
X = rbinom(n, size = 1, prob = p)
p.emv[i] = mean(X)
}

hist(p.emv, freq = F)
curve(dnorm(x, mean = p, sd = sqrt(p*(1-p)/n)), from = 0, to = 1, n = 1000, add = T, col = "red", lwd = 2)

## Caso Exp(nu) --> nu.emv aprox Normal(nu, nu^2/n)

n = 500
nu = 0.4
nu.emv = c()
theta = c() ## theta = g(nu) = 1/nu
for(i in 1:10000){
X = rexp(n, rate = nu)
nu.emv[i] = 1/mean(X)
theta[i] = 1/nu.emv[i]
}

hist(nu.emv, freq = F)
curve(dnorm(x, mean = nu, sd = sqrt(nu^2/n)), from = 0, to = 1, n = 1000, add = T, col = "red", lwd = 2)

hist(theta, freq = F)
curve(dnorm(x, mean = 1/nu, sd = sqrt((-1/nu^2)^2)*sqrt(nu^2/n)),
      from = 0, to = 5, n = 1000, add = T, col = "red", lwd = 2)
