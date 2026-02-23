## Подгрузка библиотек----------

library(modeest)
library(moments)
library(ggplot2)
library(modeest)

## Задание 1------------------

set.seed(123)

# Задание 1.1-----------------


p <- 0.3
q <- 1-p
n <- 10
data_binom <- rbinom(150, size = n, prob = p)
data_geom <- rgeom(n = 150, prob = p)

# Задание 1.2-----------------
plot(ecdf(data_binom), main = "Эмпирическая функция распределения rbinom",
     xlab = "x", ylab = "F(x)", col = "blue")

hist_1 <- hist(data_binom, plot = TRUE, col = "lightgray", main = "Полигон частот для rexp")
lines(hist_1$mids, hist_1$counts, col = "red", type = "l")

plot(ecdf(data_geom), main = "Эмпирическая функция распределения rgeom",
     xlab = "x", ylab = "F(x)", col = "blue")

hist_2 <- hist(data_geom, plot = TRUE, col = "lightgray", main = "Полигон частот для rgeom")
lines(hist_2$mids, hist_2$counts, col = "red", type = "l")

# Задание 1.3-----------------

# Выборочное среднее
sample_mean_binom <- mean(data_binom)
sample_mean_binom
sample_mean_geom <- mean(data_geom)
sample_mean_geom

# Дисперсия
sample_variance_binom <- var(data_binom)
sample_variance_binom
sample_variance_geom <- var(data_geom)
sample_variance_geom

# СКО
sd_binom <- sd(data_binom)
sd_binom
sd_geom <- sd(data_geom)
sd_geom

# Медиана
median_binom <- median(data_binom)
median_binom
median_geom <- median(data_geom)
median_geom

# Мода
mlv_binom <- mlv(data_binom)
mlv_binom
mlv_geom <- mlv(data_geom)
mlv_geom

# Коэффициент асимметрии
skewness_value_binom <- skewness(data_binom)
skewness_value_binom
skewness_value_geom <- skewness(data_geom)
skewness_value_geom

# Коэффициент эксцесса
kurtosis_value_binom <- kurtosis(data_binom)
kurtosis_value_geom <- kurtosis(data_geom)

# Задание 1.4-----------------

theoretical_mean_binom <- n*p
theoretical_variance_binom <- n*p*q

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
    "Биномиальное",
    "Биномиальное",
    "Биномиальное",
    "Биномиальное",
    "Геометрическое",
    "Геометрическое",
    "Геометрическое",
    "Геометрическое"
  ),
  Значение = c(
    theoretical_mean_binom,
    sample_mean_binom,
    theoretical_variance_binom,
    sample_variance_binom,
    theoretical_mean_geom,
    sample_mean_geom,
    theoretical_variance_geom,
    sample_variance_geom
  )
)

print(results)

# Задание 1.4-----------------

np_hat <- 1 / sample_mean_binom
cat("Оценка параметра np_hat:",  np_hat, "\n")

p_hat <- 1 / (sample_mean_geom + 1)  # +1, так как rgeom генерирует количество неудач
cat("Оценка параметра p_hat:", p_hat, "\n")
 

# Задание 1.5-----------------

# Хи^2 для экспоненциального распред.
binomhc = hist(data_binom,plot=FALSE)$counts

binomhb = hist(data_binom,plot=FALSE)$breaks

k = length(binomhc)

binomhb[1]=-Inf; binomhb[k+1]=Inf

pnth=pnorm(binomhb,theoretical_mean_binom,theoretical_variance_binom)

thfr=pnth[2:(k+1)]-pnth[1:k]

chisq.test(binomhc,p=thfr)

# Хи^2 для геометрического распред.
geomhc = hist(data_geom,plot=FALSE)$counts

geomhb = hist(data_geom,plot=FALSE)$breaks

k = length(geomhc)

geomhb[1]=-Inf; geomhb[k+1]=Inf

pnth=pnorm(geomhb,theoretical_mean_geom,theoretical_variance_geom)

thfr=pnth[2:(k+1)]-pnth[1:k]

chisq.test(geomhc,p=thfr)
