# �������� ������
library(MASS)
data(Cars93)
View(Cars93)

# ������� 1

summary(Cars93)

# ������� ������� ���� ����� � ������ ��������
mean(Cars93[Cars93$DriveTrain == "Rear", "Price"])

# ������� ����������� ����� ��������� ��� ���������� ��� 7 ����������. ��� 6 ����������
min(Cars93[Cars93$Passengers == 7, "Horsepower"])
min(Cars93[Cars93$Passengers == 6, "Horsepower"])
# ������� ������ � ������������, ����������� � �������(��������) �����������, ������� ������ ����� �������� �� ������. ��� ����������� 2 �������, ����� ���������� ����������. �����?
distances <- Cars93["MPG.highway"] * Cars93["Fuel.tank.capacity"]
as.character(Cars93$Make[which(distances == max(distances))])
as.character(Cars93$Make[which(distances == min(distances))])
as.character(Cars93$Make[which(distances == median(distances$MPG.highway))])

# ������� 2
# ���� ������� ������ ����, ������� ��������� �������������� ������ ��������� ������������ �� ��������� �������
factory.run <- function(o.cars = 1, o.trucks = 1) {
    factory <- matrix(c(40, 1, 60, 3), nrow = 2, dimnames = list(c("��������", "�����"), c("����������", "���������")))
    warehouse <- c(1600, 70) #�������� ���������� �� ������
    names(warehouse) <- rownames(factory)
    reserve <- c(8, 1)
    names(reserve) <- rownames(factory)
    output <- c(o.cars, o.trucks)
    names(output) <- colnames(factory)

    steps <- 0 # ������� ����� ����� �����
    repeat {
        steps <- steps + 1
        needed <- factory %*% output # ���������� �������, ������� ��� ����� ��� ������������ ���������� ���-�� �����
        message(steps)
        print(needed)
        # ���� �������� ���������� � ������� ������ ��� ����� �������, �� �� ��������� �������� ����������.
        # ����� ����������
        if (all(needed <= warehouse) && all((warehouse - needed) <= reserve)) {
            break ()
        }
        # ���� ������ ������� ������� � �������� ������������, �������� � �� 10%
        if (all(needed > warehouse)) {
            output <- output * 0.9
            next ()
        }
        # ���� �� �������, �� �������� �� 10%
        if (all(needed < warehouse)) {
            output <- output * 1.1
            next ()
        }
        # ���� �� ��������� ������ ������� ������� �����, � ������� ������������,
        # �� �������� ���� �� ��������� ��������
        output <- output * (1 + runif(length(output), min = -0.1, max = 0.1))
    }

    return(output)
}
# ��������� ��� ������� factory.run(). � ����� �������� ���������� ������� �������? ����� ��������� ���������?
factory.run()
#������� ������� �� ������������ ���������� (1 ���������� � 1 ��������). ���� �� ����������� �������� ������� ����� ��������� 9 ����������� � 20 ����������

# ��������� ����� 4 ����. ���������� ������ ���������� �� ���������� �����? ���� ��, ������? ���� ���, ������?
factory.run()
factory.run()
factory.run()
factory.run()
#���������� �����������

# � ���������� ����, ���������� steps � output ��������� ������ ���������. �������� ������� ���, ����� ��� ���������� ����� ����� � ������������ ���������� �����.
factory.run <- function(o.cars = 1, o.trucks = 1) {
    factory <- matrix(c(40, 1, 60, 3), nrow = 2, dimnames = list(c("��������", "�����"), c("����������", "���������")))
    warehouse <- c(1600, 70) #�������� ���������� �� ������
    names(warehouse) <- rownames(factory)
    reserve <- c(8, 1)
    names(reserve) <- rownames(factory)
    output <- c(o.cars, o.trucks)
    names(output) <- colnames(factory)

    steps <- 0 # ������� ����� ����� �����
    repeat {
        steps <- steps + 1
        needed <- factory %*% output # ���������� �������, ������� ��� ����� ��� ������������ ���������� ���-�� �����
        # ���� �������� ���������� � ������� ������ ��� ����� �������, �� �� ��������� �������� ����������.
        # ����� ����������
        if (all(needed <= warehouse) && all((warehouse - needed) <= reserve)) {
            break ()
        }
        # ���� ������ ������� ������� � �������� ������������, �������� � �� 10%
        if (all(needed > warehouse)) {
            output <- output * 0.9
            next ()
        }
        # ���� �� �������, �� �������� �� 10%
        if (all(needed < warehouse)) {
            output <- output * 1.1
            next ()
        }
        # ���� �� ��������� ������ ������� ������� �����, � ������� ������������,
        # �� �������� ���� �� ��������� ��������
        output <- output * (1 + runif(length(output), min = -0.1, max = 0.1))
    }
    print(needed)
    message(steps)
    return(trunc(output))
}
factory.run()
# ���������� ���� ������ �������� ����������� � 20 ���������� � ��������� �������
factory.run(o.cars = 30, o.trucks = 20)