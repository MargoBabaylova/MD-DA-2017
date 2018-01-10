#Пользуясь примером из лекции файл (5.0.R) проанализируйте данные
#о возрасте и физ. характеристиках молюсков
#https://archive.ics.uci.edu/ml/datasets/abalone
datafrm <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header = TRUE, sep = ",")
summary(datafrm)
colnames(datafrm)
colnames(datafrm) <- c("sex", "length", "diameter", "height",
                "whole_weight", "shucked_weight",
                "viscera_weight", "shell_weight", "rings")

colnames(datafrm)
datafrm$sex <- factor(c("Female", "Infant", "Male")[datafrm$sex])
par(mfrow = c(1, 3))
hist(datafrm$diameter, main = "Диаметр, мм")
hist(datafrm$height, main = "Высота, мм")
hist(datafrm$whole_weight, main = "Полный вес, гр")
#Видим ассиметрию https://en.wikipedia.org/wiki/Skewness
#и выбросы (от них нужно избавиться)

#Визулизируем возможные зависимости
par(mfrow = c(1, 2))
plot(datafrm$diameter, datafrm$whole_weight, 'p', main = "Зависимость веса от диаметра")
plot(datafrm$height, datafrm$whole_weight, 'p', main = "Зависимость веса от высоты")

par(mfrow = c(1, 1))
par(ask = F)
#Хорошо видна зависимость, нужно её исследовать
#построить линейные модели при помощи функции lm, посмотреть их характеристики
linear.model.d_w <- lm(datafrm$diameter ~ datafrm$whole_weight, data = datafrm)
summary(linear.model.d_w)
linear.model.h_w <- lm(datafrm$height ~ datafrm$whole_weight, data = datafrm)
summary(linear.model.h_w)

plot(linear.model.d_w, main = "Зависимость веса от диаметра")
plot(linear.model.h_w, main = "Зависимость веса от высоты")

#избавиться от выборосов, построить ещё модели и проверить их
datafrm.noout <- datafrm[datafrm$height < 0.4 & datafrm$diameter > 0.2 & datafrm$diameter < 0.6 & datafrm$whole_weight < 1.7 & datafrm$whole_weight > 0.1,]
hist(datafrm.noout$diameter, main = "Диаметр, мм")
hist(datafrm.noout$height, main = "Высота, мм")
hist(datafrm.noout$whole_weight, main = "Полный вес, гр")

linear.model.d_w <- lm(datafrm.noout$diameter ~ datafrm.noout$whole_weight, data = datafrm.noout)
summary(linear.model.d_w)
linear.model.h_w <- lm(datafrm.noout$height ~ datafrm.noout$whole_weight, data = datafrm.noout)
summary(linear.model.h_w)

plot(linear.model.d_w, main = "Зависимость веса от диаметра")
plot(linear.model.h_w, main = "Зависимость веса от высоты")

#разделить массив данных на 2 случайные части
datalength <- nrow(datafrm.noout)
testindex <- seq(1, trunc(datalength * 0.7), by = 1)
controlindex <- seq(round(datalength * 0.3)+1, datalength, by = 1)
sample.test <- datafrm.noout[testindex,]
sample.control <- datafrm.noout[controlindex,]

#подогнать модель по первой части
linear.model.d_w <- lm(sample.test$diameter ~ sample.test$whole_weight, data = sample.test)
linear.model.h_w <- lm(sample.test$height ~ sample.test$whole_weight, data = sample.test)

plot(linear.model.d_w, main = "Зависимость веса от диаметра")
plot(linear.model.h_w, main = "Зависимость веса от высоты")

#спрогнозировать (функция predict) значения во второй части
predicted.d_w <- predict(linear.model.d_w, sample.control)
predicted.h_w <- predict(linear.model.h_w, sample.control)

#проверить качесвто прогноза
cor(sample.control$whole_weight, predicted.d_w)
cor(sample.control$whole_weight, predicted.h_w)