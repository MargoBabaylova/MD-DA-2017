#������������� ������. ������

#�������� ������ (��������� ���������� ������� ����� � ������� setwd) ��� ������� ������ ����
data = read.csv("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/diet.csv", row.names = 1)
summary(data)
#����������� �� ���������� � ����������� �������, ��� ��� ������
#https://www.sheffield.ac.uk/polopoly_fs/1.547015!/file/Diet_data_description.docx
#https://www.sheffield.ac.uk/mash/data
colnames(data) <- c("gender", "age", "height", "initial.weight",
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])
#������� ����� ������� - ���������
data$weight.loss = data$initial.weight - data$final.weight
#������������� ���� �� �������� �� ����� ����
boxplot(weight.loss ~ diet.type, data = data, col = "light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h = 0, col = "green")

#�������� ���������������� �� ������
table(data$diet.type)

#������ ��������� �������
#library(gplots) #���������� ��������������� � ������� install.packages
#install.packages("gplots")
plotmeans(weight.loss ~ diet.type, data = data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN = sd)


#��� �������� ANOVA ������ ���������� ������� aov, ������� ������ �������� ������ lm
#���� �� ������������ ��������
fit <- aov(weight.loss ~ diet.type, data = data)
summary(fit)

#�������� �������� ����� �������� ���������� ��� ���� �����
TukeyHSD(fit)

#Tukey honest significant differences test)
#library(multcomp)
#install.packages("multcomp")
par(mar = c(5, 4, 6, 2))
tuk <- glht(fit, linfct = mcp(diet.type = "Tukey"))
plot(cld(tuk, level = .05), col = "lightgrey")

#�������
#�������� �������� �� ������ � ���������� �� ���
plot(data$weight.loss, data$diet.type)
data.noout <- data[data$weight.loss > 0,]
plot(data.noout$weight.loss, data.noout$diet.type)

#�������� ��������� ��� ����� � �������� ���������� � ��������� � ���
table(data.noout$diet.type)

plotmeans(weight.loss ~ diet.type, data = data.noout)

fit <- aov(weight.loss ~ diet.type, data = data.noout)
summary(fit)

TukeyHSD(fit)
tuk <- glht(fit, linfct = mcp(diet.type = "Tukey"))
plot(cld(tuk, level = .05), col = "lightgrey")

#������� �������� https://www.sheffield.ac.uk/polopoly_fs/1.547015!/file/Diet_data_description.docx
#� ���������� ��������� ������� �� ����

# Are there gender differences for weight lost?
data.noout$gender <- factor(c("Female", "Male")[as.ordered(data.noout$gender)])

boxplot(weight.loss ~ gender, data = data.noout, col = "light gray",
        ylab = "Weight loss (kg)", xlab = "Gender")

plotmeans(weight.loss ~ gender, data = data.noout)
aggregate(data.noout$weight.loss, by = list(data.noout$gender), FUN = sd)

# Effect of diet and gender on weight lost
# Means plot of weight lost by diet and gender

data.noout$typebygender <- interaction(data.noout$diet.type, data.noout$gender)
fit <- aov(weight.loss ~ typebygender, data = data.noout)
summary(fit)

TukeyHSD(fit)
tuk <- glht(fit, linfct = mcp(typebygender = "Tukey"))
plot(cld(tuk, level = .05), col = "lightgrey")

# Add height to either ANOVA
data.noout$height <- cut(data.noout$height, 3, labels = c('small', 'mid', 'tall'))

fit <- aov(weight.loss ~ typebygender * height, data = data.noout)
summary(fit)

TukeyHSD(fit)