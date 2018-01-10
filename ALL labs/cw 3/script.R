#Загрузите данные о землятресениях
anss <- readLines("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/earthquakes_2011.html", warn = FALSE)
#Выберите строки, которые содержат данные с помощью регулярных выражений и функции grep
pattern.seismo.stats <- "\\d{4}(\\/\\d{2}){2}\\s(\\d{2}:){2}\\d{2}\\.\\d{2}(,[^,]*){10},\\d*"

anss.isdata <- grepl(pattern = pattern.seismo.stats, x = anss)
seismo.stats.data <- regmatches(anss, regexpr(pattern = pattern.seismo.stats, text = anss))
#Проверьте что все строки (all.equal) в результирующем векторе подходят под шаблон.
all.equal(anss[which(anss.isdata)], seismo.stats.data)