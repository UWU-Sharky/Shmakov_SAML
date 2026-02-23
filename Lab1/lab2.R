## Подгрузка библиотек----------

library(modeest)
library(moments)
library(ggplot2)
library(modeest)

## Задание 1------------------

set.seed(123)

# Задание 1.1-----------------

alpha <- 2
beta <- 0.5
lambda <- 0.5
data_exp <- rexp(n = 150, rate = lambda)
data_gamma <- rgamma(n = 150,shape = alpha, rate = beta)


# Задание 1.2-----------------
plot(ecdf(data_exp), main = "Эмпирическая функция распределения rexp",
     xlab = "x", ylab = "F(x)", col = "blue")

hist_1 <- hist(data_exp, plot = TRUE, col = "lightgray", main = "Полигон частот для rexp")
lines(hist_1$mids, hist_1$counts, col = "red", type = "l")

plot(ecdf(data_gamma), main = "Эмпирическая функция распределения rgeom",
     xlab = "x", ylab = "F(x)", col = "blue")

hist_2 <- hist(data_gamma, plot = TRUE, col = "lightgray", main = "Полигон частот для rgeom")
lines(hist_2$mids, hist_2$counts, col = "red", type = "l")

# Задание 1.3-----------------

# Выборочное среднее
sample_mean_exp <- mean(data_exp)
sample_mean_exp
sample_mean_gamma <- mean(data_gamma)
sample_mean_gamma

# Дисперсия
sample_variance_exp <- var(data_exp)
sample_variance_exp
sample_variance_gamma <- var(data_gamma)
sample_variance_gamma

# СКО
sd_exp <- sd(data_exp)
sd_exp
sd_gamma <- sd(data_gamma)
sd_gamma

# Медиана
median_exp <- median(data_exp)
median_exp
median_gamma <- median(data_gamma)
median_gamma

# Мода
mlv_exp <- mlv(data_exp)
mlv_exp
mlv_gamma <- mlv(data_gamma)
mlv_gamma

# Коэффициент асимметрии
skewness_value_exp <- skewness(data_exp)
skewness_value_exp
skewness_value_gamma <- skewness(data_gamma)
skewness_value_gamma

# Коэффициент эксцесса
kurtosis_value_exp <- kurtosis(data_exp)
kurtosis_value_gamma <- kurtosis(data_gamma)

# Задание 1.4-----------------

theoretical_mean_exp <- 1 / lambda
theoretical_variance_exp <- 1 / (lambda^2)

theoretical_mean_gamma <- alpha / beta
theoretical_variance_gamma <- alpha / (beta^2)

results <- data.frame(
  Статистика = c(
    "Мат. ожидание (теоретическое)",
    "Мат. ожидание (выборочное)",
    "Дисперсия (теоретическая)",
    "Дисперсия (выборочная)",
    "Мат. ожидание (теоретическое)",
    "Мат. ожидание (выборочное)",
    "Дисперсия (теоретическая)",
    "Дисперсия (выборочная)"
  ),
  Распределение = c(
    "Экспоненциальное",
    "Экспоненциальное",
    "Экспоненциальное",
    "Экспоненциальное",
    "Гамма",
    "Гамма",
    "Гамма",
    "Гамма"
  ),
  Значение = c(
    theoretical_mean_exp,
    sample_mean_exp,
    theoretical_variance_exp,
    sample_variance_exp,
    theoretical_mean_gamma,
    sample_mean_gamma,
    theoretical_variance_gamma,
    sample_variance_gamma
  )
)

print(results)

# Задание 1.4-----------------

lambda_hat <- 1 / sample_mean_exp
cat("Оценка параметра lambda_hat:",  lambda_hat, "\n")

beta_hat <- alpha / (sample_mean_gamma + 1)  # +1, так как rgeom генерирует количество неудач
cat("Оценка параметра beta_hat:", beta_hat, "\n")

# Задание 1.5-----------------

# Хи^2 для экспоненциального распред.
exphc = hist(data_exp,plot=FALSE)$counts

exphb = hist(data_exp,plot=FALSE)$breaks

k = length(exphc)

exphb[1]=-Inf; exphb[k+1]=Inf

pnth=pnorm(exphb,theoretical_mean_exp,theoretical_variance_exp)

thfr=pnth[2:(k+1)]-pnth[1:k]

chisq.test(exphc,p=thfr)

# Хи^2 для геометрического распред.
gammamhc = hist(data_gamma,plot=FALSE)$counts

gammahb = hist(data_gamma,plot=FALSE)$breaks

k = length(gammamhc)

gammahb[1]=-Inf; gammahb[k+1]=Inf

pnth=pnorm(gammahb,theoretical_mean_gamma,theoretical_variance_gamma)

thfr=pnth[2:(k+1)]-pnth[1:k]

chisq.test(gammamhc,p=thfr)

