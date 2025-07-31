# Vectors
Group_A <- c(-0.17460092, 0.57131273, -0.97957078, 0.55760611, -1.38675079,
             0.87678217, 0.02424806, 0.37201613, -1.08039729, 0.29898955,
             0.14407196, 0.23606151, 1.16615124, -0.34826371, -0.12453904,
             0.20398317, 0.58846349, -0.74028122, 1.76338615, 0.01492071,
             0.19413667, -0.34928128, -0.95047882, 1.57190112, -0.39559300,
             0.04158754, -0.46319252, -1.07299956, -0.27920085, 0.84563163)

Group_B <- c(3.1131149, 1.9480742,2.5673920, 1.7320103, 4.0895170, 1.1603928,
             1.3140735, 2.5380473, 0.8589795, 0.5685645, 1.9639550,2.6961566,
             2.5803225, 3.4920269, 1.3917766, 2.1461159, 2.6398042,0.7714907,
             1.9098055, 1.6603716, 1.9494934, 1.4430121, 2.7838871,1.7334802,
             3.5283881, 2.9197496, 1.9893398, 3.3018724, 0.1881463, 2.1493005)

# 2-sample t-test - Practice 
boxplot(Group_A, Group_B, names=c("Group A", "Group B"))
hist(Group_A)
hist(Group_B)

### LINE 20 and 21 are the same since R runs the basic t test as 2 sided
t.test(Group_A,Group_B)
t.test(Group_A,Group_B,alternative="two.sided")
t.test(Group_A,Group_B,alternative="less")
t.test(Group_A,Group_B,alternative="greater")


# To implement a t-test in R:mu is a hypothesized mean for a population
 t.test(data1 , mu= x) # 1-sample t-test
> t.test(data1,data2,paired=TRUE) # matched pairs t-test
> t.test(data1,data2) # 2-sample t-test

# Checking assumptions: Are my data normally distributed?
> hist(data1) # check for a bell-shaped histogram
> plot(density(data1)) # check for bell-shaped curve
> qqnorm(data1) # check for straight line