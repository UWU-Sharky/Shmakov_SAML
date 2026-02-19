################################
## Работа с распределениями   ##
## Работка с графикой         ##
################################

## Работа с распределениями (потом будет подробнее) -----

# генерирует выборку 100 значений из нормальной генеральной совокупности 
# с нулевым средним и единичной дисперсией
x=rnorm(100,0,1) 
length(x)
sum(x)
sum(x)/length(x)
mean(x)

## Графика -----------

hist(x) # гистограмма  

palette() # основная палитра цветов
colors()  # полная палитра цветов

hist(x,col="blue") 
hist(x,density=6,angle=45, col="blue") 
hist(x,density=c(1:10),angle=45, col="blue") 

x11()   # новое графическое окно
hist(x,density=c(1:10),angle=45, col=colors()[610:620]) 

x11()
hist(x,freq=FALSE,density=6,angle=45, col=colors()[610:622]) # гистограмма относительных частот

# Некоторые значения функции hist
hist(x,plot=FALSE)$breaks # границы интервалов разбиения
hist(x,plot=FALSE)$counts # абсолютные частоты

x11()
qqnorm(x, col='red')  # квантильный график

lines(c(-3,3),c(-3,3))

## Пример с реальными данными
getwd()
#setwd("D:/Stat2017/R/")

data1=read.table("kids_ru.txt",header=TRUE, sep='\t')
data1
table(data1$Вес)

x11()

plot(data1$Вес~data1$Рост)
plot(Вес~Рост, data=data1) # то же самое, отличаются только подписи по осям

plot(Вес~Рост, data=data1, 
     xlab="Рост", ylab="Вес", main="Зависимость Рост-Вес")

plot(data1$Вес~data1$Рост, data=data1, 
     xlab="Рост", ylab="Вес", main="Зависимость Рост-Вес",
     col="blue")

plot(data1$Вес~data1$Рост, data=data1, 
     xlab="Рост", ylab="Вес", main="Зависимость Рост-Вес",
     col="blue", pch= 6 )

plot(data1$Вес~data1$Рост, data=data1, 
     xlab="Рост", ylab="Вес", main="Зависимость Рост-Вес",
     col="blue", type="o")


data_s=data1[order(data1$Рост),]

plot(data_s$Вес~data_s$Рост, data=data_s, 
     xlab="Рост", ylab="Вес", main="Зависимость Рост-Вес",
     col="blue", type="o")

cor(data1$Вес,data1$Рост) # корреляция Пирсона (cor выдает только значение коэф-та)
cor.test(data1$Вес,data1$Рост, method="spearman") # корреляция Спирмена

boxplot(Вес~Пол, data=data1) # диаграмма размаха, коробки с усами
boxplot(Рост~Пол, data=data1)

## Управление графическими окнами ------

x11()
layout(matrix(1:6, 3, 2))
layout.show(6)

mat = matrix(c(1,1,2,3,4,4), 3, 2)
layout(mat)
layout.show(4)

mat_b=matrix(c(1:2),1,2)
mat_b
layout(mat_b)
layout.show(2)

#l=layout(mat)
#layout.show(l)

boxplot(Вес~Пол, data=data1) # рисуем (автоматически) в первое окно
boxplot(Рост~Пол, data=data1) # рисуем во второе окно

