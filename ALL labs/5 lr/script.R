#��������� �������� �� ������ ���� (5.0.R) ��������������� ������
#� �������� � ���. ��������������� ��������
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
hist(datafrm$diameter, main = "�������, ��")
hist(datafrm$height, main = "������, ��")
hist(datafrm$whole_weight, main = "������ ���, ��")
#����� ���������� https://en.wikipedia.org/wiki/Skewness
#� ������� (�� ��� ����� ����������)

#������������ ��������� �����������
par(mfrow = c(1, 2))
plot(datafrm$diameter, datafrm$whole_weight, 'p', main = "����������� ���� �� ��������")
plot(datafrm$height, datafrm$whole_weight, 'p', main = "����������� ���� �� ������")

par(mfrow = c(1, 1))
par(ask = F)
#������ ����� �����������, ����� � �����������
#��������� �������� ������ ��� ������ ������� lm, ���������� �� ��������������
linear.model.d_w <- lm(datafrm$diameter ~ datafrm$whole_weight, data = datafrm)
summary(linear.model.d_w)
linear.model.h_w <- lm(datafrm$height ~ datafrm$whole_weight, data = datafrm)
summary(linear.model.h_w)

plot(linear.model.d_w, main = "����������� ���� �� ��������")
plot(linear.model.h_w, main = "����������� ���� �� ������")

#���������� �� ���������, ��������� ��� ������ � ��������� ��
datafrm.noout <- datafrm[datafrm$height < 0.4 & datafrm$diameter > 0.2 & datafrm$diameter < 0.6 & datafrm$whole_weight < 1.7 & datafrm$whole_weight > 0.1,]
hist(datafrm.noout$diameter, main = "�������, ��")
hist(datafrm.noout$height, main = "������, ��")
hist(datafrm.noout$whole_weight, main = "������ ���, ��")

linear.model.d_w <- lm(datafrm.noout$diameter ~ datafrm.noout$whole_weight, data = datafrm.noout)
summary(linear.model.d_w)
linear.model.h_w <- lm(datafrm.noout$height ~ datafrm.noout$whole_weight, data = datafrm.noout)
summary(linear.model.h_w)

plot(linear.model.d_w, main = "����������� ���� �� ��������")
plot(linear.model.h_w, main = "����������� ���� �� ������")

#��������� ������ ������ �� 2 ��������� �����
datalength <- nrow(datafrm.noout)
testindex <- seq(1, trunc(datalength * 0.7), by = 1)
controlindex <- seq(round(datalength * 0.3)+1, datalength, by = 1)
sample.test <- datafrm.noout[testindex,]
sample.control <- datafrm.noout[controlindex,]

#��������� ������ �� ������ �����
linear.model.d_w <- lm(sample.test$diameter ~ sample.test$whole_weight, data = sample.test)
linear.model.h_w <- lm(sample.test$height ~ sample.test$whole_weight, data = sample.test)

plot(linear.model.d_w, main = "����������� ���� �� ��������")
plot(linear.model.h_w, main = "����������� ���� �� ������")

#��������������� (������� predict) �������� �� ������ �����
predicted.d_w <- predict(linear.model.d_w, sample.control)
predicted.h_w <- predict(linear.model.h_w, sample.control)

#��������� �������� ��������
cor(sample.control$whole_weight, predicted.d_w)
cor(sample.control$whole_weight, predicted.h_w)