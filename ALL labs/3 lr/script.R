# Исходные данные
# Файл forbes.htm содержит список богатейших американцев по версии журнала Форбс. На его примере потренируемся в разборе html страниц.

    #Задание 1

#Используйте команду readLines для загрузки файла в текстовый вектор html.
forbes <- readLines("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/forbes.htm")
#Сколько строк в файле ?
length(forbes)
#Сколько символов в файле ?
sum(nchar(forbes))

patterns.profit <- "[$][,0-9]+ [BMT]"
forbes.isprofit <- grep(patterns.profit, forbes)

forbes.profits <- regmatches(forbes, regexpr(patterns.profit, forbes))

length(forbes.profits) == 100

forbes.profits.stats <- table(forbes.profits)
as.data.frame(forbes.profits.stats)

    #Задание 2
M <- 10**9
forbes.profits.modify <- gsub(",", "\\.", forbes.profits)
worths <- as.double(regmatches(forbes.profits.modify, regexpr("[0-9.]+", forbes.profits.modify))) * M

length(worths)
typeof(worths)
all(worths > 1 * M)
as.data.frame(head(worths))

#Используйте вектор worths, чтобы выяснить следующее:
median(worths)
mean(worths)

length(worths[which(worths > 5 * M)])
length(worths[which(worths > 10 * M)])
length(worths[which(worths > 25 * M)])

sum(worths)

sum(worths[1:5]) * 100 / sum(worths)
sum(worths[1:20]) * 100 / sum(worths)

Household_net_worth <- 96.196 * M
Household_net_worth * 100 / sum(worths)