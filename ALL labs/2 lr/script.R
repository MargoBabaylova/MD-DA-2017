# Загрузка данных
library(MASS)
data(Cars93)
View(Cars93)

# ЗАДАНИЕ 1

summary(Cars93)

# Найдите среднюю цену машин с задним приводом
mean(Cars93[Cars93$DriveTrain == "Rear", "Price"])

# Найдите минимальное число лошадиных сил автомобиля для 7 пассажиров. Для 6 пассажиров
min(Cars93[Cars93$Passengers == 7, "Horsepower"])
min(Cars93[Cars93$Passengers == 6, "Horsepower"])
# Найдите машины с максимальным, минимальным и средним(медианой) расстоянием, которая машина может проехать по трассе. Вам понадобятся 2 колонки, чтобы рассчитать расстояние. Какие?
distances <- Cars93["MPG.highway"] * Cars93["Fuel.tank.capacity"]
as.character(Cars93$Make[which(distances == max(distances))])
as.character(Cars93$Make[which(distances == min(distances))])
as.character(Cars93$Make[which(distances == median(distances$MPG.highway))])

# ЗАДАНИЕ 2
# Ниже приведён пример кода, который старается оптимизировать выпуск продукции ориентируясь на доступные ресурсы
factory.run <- function(o.cars = 1, o.trucks = 1) {
    factory <- matrix(c(40, 1, 60, 3), nrow = 2, dimnames = list(c("трудодни", "сталь"), c("автомобили", "грузовики")))
    warehouse <- c(1600, 70) #Доступно материалов на складе
    names(warehouse) <- rownames(factory)
    reserve <- c(8, 1)
    names(reserve) <- rownames(factory)
    output <- c(o.cars, o.trucks)
    names(output) <- colnames(factory)

    steps <- 0 # Счётчик числа шагов цикла
    repeat {
        steps <- steps + 1
        needed <- factory %*% output # Подсчитаем ресурсы, которые нам нужны для производства требуемого кол-ва машин
        message(steps)
        print(needed)
        # Если ресурсов достаточно и остаток меньше или равен резерву, то мы произвели максимум возможного.
        # Нужно прекращать
        if (all(needed <= warehouse) && all((warehouse - needed) <= reserve)) {
            break ()
        }
        # Если заявка слишком большая и ресурсов недостаточно, уменьшим её на 10%
        if (all(needed > warehouse)) {
            output <- output * 0.9
            next ()
        }
        # Если всё наоброт, то увеличим на 10%
        if (all(needed < warehouse)) {
            output <- output * 1.1
            next ()
        }
        # Если мы потребили одного ресурса слишком много, а другого недостаточно,
        # то увеличим план на случайную величину
        output <- output * (1 + runif(length(output), min = -0.1, max = 0.1))
    }

    return(output)
}
# Запустите эту функцию factory.run(). С каким входными значениями функция вызвана? Какой получился результат?
factory.run()
#Функция вызвана со стандартными значениями (1 автомобиль и 1 грузовик). Судя по результатам расчетов выгодно будет выпустить 9 автомобилей и 20 грузовиков

# Повторите вызов 4 раза. Полученные ответы отличаются от полученных ранее? Если да, почему? Если нет, почему?
factory.run()
factory.run()
factory.run()
factory.run()
#Результаты различаются

# В приведённом коде, переменные steps и output находятся внутри алгоритма. Измените функцию так, чтобы она возвращала число шагов и произведённое количество машин.
factory.run <- function(o.cars = 1, o.trucks = 1) {
    factory <- matrix(c(40, 1, 60, 3), nrow = 2, dimnames = list(c("трудодни", "сталь"), c("автомобили", "грузовики")))
    warehouse <- c(1600, 70) #Доступно материалов на складе
    names(warehouse) <- rownames(factory)
    reserve <- c(8, 1)
    names(reserve) <- rownames(factory)
    output <- c(o.cars, o.trucks)
    names(output) <- colnames(factory)

    steps <- 0 # Счётчик числа шагов цикла
    repeat {
        steps <- steps + 1
        needed <- factory %*% output # Подсчитаем ресурсы, которые нам нужны для производства требуемого кол-ва машин
        # Если ресурсов достаточно и остаток меньше или равен резерву, то мы произвели максимум возможного.
        # Нужно прекращать
        if (all(needed <= warehouse) && all((warehouse - needed) <= reserve)) {
            break ()
        }
        # Если заявка слишком большая и ресурсов недостаточно, уменьшим её на 10%
        if (all(needed > warehouse)) {
            output <- output * 0.9
            next ()
        }
        # Если всё наоброт, то увеличим на 10%
        if (all(needed < warehouse)) {
            output <- output * 1.1
            next ()
        }
        # Если мы потребили одного ресурса слишком много, а другого недостаточно,
        # то увеличим план на случайную величину
        output <- output * (1 + runif(length(output), min = -0.1, max = 0.1))
    }
    print(needed)
    message(steps)
    return(trunc(output))
}
factory.run()
# Установите план равный тридцати автомобилям и 20 грузовикам и выполните функцию
factory.run(o.cars = 30, o.trucks = 20)