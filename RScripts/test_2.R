#########################################
### Рабочая директория                ###
### Импорт данных из внешнего файла   ###
### Экспорт данных                    ###
### Работа с data.frame (продолжение) ###
#########################################

## Проверка и установка пути ---------

# getwd() Посмотреть текущий путь по умолчанию можно с помощью функции  
# setwd("c:/…") – изменить директорию по умолчанию

getwd()

setwd("C:/Users/user/Documents") # Установить нужный путь!

# Проверить, что в указанной папке содержатся необходимые файлы

## Импорт данных из файла txt -------------

data1=read.table("kids_ru.txt",header=TRUE, sep='\t')
data1

## Сортировка, работа с таблицей ------------

data1[,2]

data1$Рост

data1[data1$Пол=="f",]

data1[data1$Пол=="m",]

data1[order(data1$Пол, data1$Рост),]

dim(data1[data1$Пол=="f",])
dim(data1[data1$Пол=="m",])

## Экспорт данных в текстовый файл
data_f=data1[data1$Пол=="f",]
? write.table # если вдруг забыли порядок аргументов
write.table(data_f,"kids_f.txt", sep="\t")

data_m=data1[data1$Пол=="m",]
write.table(data_m,"kids_all.txt", sep="\t", quote=FALSE)
write.table(data_f,"kids_all.txt", sep="\t", append=TRUE, quote=FALSE)

numb_kid=nrow(data1)+1
new_kid=c(numb_kid,"f",134,27,30,117)
write(new_kid,"kids_all.txt", sep="\t",append=TRUE, ncolumns=length(new_kid))

## Установка пакетов ------------

installed.packages() # просмотр всех установленных пакетов
# install.packages() # установить пакеты
# library() # загрузка пакета 

## Импорт данных из файла xls ------------
# Импортировать данные в формате *.xls 
#можно с помощью аналогичной функции read.xls() 
# библиотеки ReadWriteXls с аналогичными параметрами 
# или  с помощью более простой функции read_excel() пакета readxl.

# Для начала установим пакета readxl,
# если пакет еще не установлен
install.packages("readxl") # установить пакеты
library(readxl) # загрузка пакета 

data2=read_excel("kids_ru.xlsx")
data2

install.packages("xlsx")
library(xlsx)
write.xlsx(data2, "mydata.xlsx")

# Если возникли проблемы с установкой пакетов для чтения xls  файлов, 
# можно их сохранить в csv и импортировать операцией read.csv(), 
# аналогично read.table(), указав нужный разделитель (, или ;)

data3=read.csv("kids_ru.csv",header=TRUE, sep=";")
data3
