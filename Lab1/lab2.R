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

plot(ecdf(data_geom), main = "Эмпирическая функция распределения rgeom",
     xlab = "x", ylab = "F(x)", col = "blue")

hist_2 <- hist(data_geom, plot = TRUE, col = "lightgray", main = "Полигон частот для rgeom")
lines(hist_2$mids, hist_2$counts, col = "red", type = "l")

# Задание 1.3-----------------

# Выборочное среднее
sample_mean_exp <- mean(data_exp)
sample_mean_exp
sample_mean_geom <- mean(data_geom)
sample_mean_geom

# Дисперсия
sample_variance_exp <- var(data_exp)
sample_variance_exp
sample_variance_geom <- var(data_geom)
sample_variance_geom

# СКО
sd_exp <- sd(data_exp)
sd_exp
sd_geom <- sd(data_geom)
sd_geom

# Медиана
median_exp <- median(data_exp)
median_exp
median_geom <- median(data_geom)
median_geom

# Медиана
mlv_exp <- mlv(data_exp)
mlv_exp
mlv_geom <- mlv(data_geom)
mlv_geom

# Коэффициент асимметрии
skewness_value_exp <- skewness(data_exp)
skewness_value_exp
skewness_value_geom <- skewness(data_geom)
skewness_value_geom

# Коэффициент эксцесса
kurtosis_value_exp <- kurtosis(data_exp)
kurtosis_value_geom <- kurtosis(data_geom)

# Задание 1.4-----------------

theoretical_mean_exp <- 1 / lambda
theoretical_variance_exp <- 1 / (lambda^2)

theoretical_mean_geom <- q / p
theoretical_variance_geom <- q / (p^2)

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
    "Геометрическое",
    "Геометрическое",
    "Геометрическое",
    "Геометрическое"
  ),
  Значение = c(
    theoretical_mean_exp,
    sample_mean_exp,
    theoretical_variance_exp,
    sample_variance_exp,
    theoretical_mean_geom,
    sample_mean_geom,
    theoretical_variance_geom,
    sample_variance_geom
  )
)

print(results)

# Задание 1.4-----------------

lambda_hat <- 1 / sample_mean_exp
cat("Оценка параметра lambda_hat:",  lambda_hat, "\n")

p_hat <- 1 / (sample_mean_geom + 1)  # +1, так как rgeom генерирует количество неудач
cat("Оценка параметра p_hat:", p_hat, "\n")


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
geomhc = hist(data_geom,plot=FALSE)$counts

geomhb = hist(data_geom,plot=FALSE)$breaks

k = length(geomhc)

geomhb[1]=-Inf; geomhb[k+1]=Inf

pnth=pnorm(geomhb,theoretical_mean_geom,theoretical_variance_geom)

thfr=pnth[2:(k+1)]-pnth[1:k]

chisq.test(geomhc,p=thfr)
