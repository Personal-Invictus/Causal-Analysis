#【数据探索】
setwd("D:/Roja_D/NTU Learn/T3/Causal Analysis/Group Project/exploration")
library(AER)
library(data.table)
library(ggplot2)
data(CPS1985)
data=data.table(CPS1985)

dt=data #这个dt等会会被覆盖

summary(dt)
colnames(dt)
# "wage"       "education"  "experience" "age"        "ethnicity" 
# "region"     "gender"     "occupation" "sector"     "union"     
# "married"  

plot(density(dt$wage),xlab = 'wage',main="Distribution of Age")

ggplot(data = dt, aes(x = age, y = wage)) +
  geom_point() +
  facet_grid(. ~ ethnicity) +
  # 为绘图添加标题，标题内容为"AA Delays by Month"
  labs(title = "age vs wage by ethn")

pairs(~ wage+education+experience+age+
        ethnicity+region+
        gender+occupation+
        sector+union+married,
        panel=panel.smooth,  # 平滑曲线
        span=0.75, # 取值范围是 0 到 1，当span接近 1 时，曲线会更平滑，更像线性回归直线
        data = dt)

barplot(table(dt$education),col='red')
plot(density(dt$experience),xlab = 'wage',main="Distribution of Age")

pairs(~ education+experience,
      panel=panel.smooth,  # 平滑曲线
      span=0.75, # 取值范围是 0 到 1，当span接近 1 时，曲线会更平滑，更像线性回归直线
      data = dt)


ggplot(dt, aes(x = age, y = wage, fill = region)) +
  stat_summary(fun = "mean", geom = "col", position = "dodge") +
  labs(title = "wage by age",
       x = "age",
       y = "wage",
       fill = "region") +
  theme_minimal()

ggplot(dt, aes(x = occupation, y = age, fill = region)) +
  stat_summary(fun = "mean", geom = "col", position = "dodge") +
  labs(title = "age vs occupation by region",
       x = "occupation",
       y = "age",
       fill = "region") +
  theme_minimal()

ggplot(dt, aes(x = sector, y = education, fill = region)) +
  stat_summary(fun = "mean", geom = "col", position = "dodge") +
  labs(title = "sector vs edu by region",
       x = "sector",
       y = "education",
       fill = "region") +
  theme_minimal()

ggplot(dt, aes(x = sector, y = wage, fill = gender)) +
  stat_summary(fun = "mean", geom = "col", position = "dodge") +
  labs(title = "sector vs wage by gender",
       x = "sector",
       y = "wage",
       fill = "gender") +
  theme_minimal()

ggplot(dt, aes(x = occupation, y = wage, fill = gender)) +
  stat_summary(fun = "mean", geom = "col", position = "dodge") +
  labs(title = "occupation vs wage by gender",
       x = "occupation",
       y = "wage",
       fill = "gender") +
  theme_minimal()


#======================================================
#【matching】
library(caret) 

train_idx <- createDataPartition(data$wage, p=0.3, list=FALSE)
dt <- data[train_idx, ] #训练集
test_dt <- data[-train_idx, ] #测试集
test_dt
test_input = test_dt[,-1]
test_input

#因果：性别是否会影响工资
#Y(Result)：wage
#X(Treatment)：gender
#控制变量subgroup：occupation

library(Matching)  # via M Distance
library(tableone)  # standard Table 1 to check covariate balance
# xvars <- c("education","experience","age","ethnicity","region",
#            "occupation","sector","union","married") #这里面不能有gender

table1.prematch <- CreateTableOne(vars = xvars, strata = "gender", data = dt, test = F)
print(table1.prematch, smd = T)  ## All SMDs are >> 0.2. Hence severe imbalanced. Not comparable.

set.seed(8)
m.ps <- glm(gender ~ 
              education+experience+age+
              ethnicity+region+
              occupation+sector+
              union+married, family = binomial, data = dt)
summary(m.ps)
ps <- m.ps$fitted.values

# Matching on Propensity Score without caliper. qlogis() is the logit() function
matching.ps <- Match(Tr = (dt$gender == 'male'), X = qlogis(ps), ties = T,caliper = 0.05)
matched.ps <- dt[unlist(matching.ps[c("index.treated", "index.control")]),]

# Table 1 after matching via Propensity Score without caliper
table1.psmatch <- CreateTableOne(vars = xvars, strata = "gender", data = matched.ps, test = F)
print(table1.psmatch, smd = T) #你会发现还是有很多的SMD>0.2，这不太好，不过比起用m距离，至少logit回归可以考虑到其他分类变量

y_tre <- matched.ps$wage[matched.ps$gender == 'male']
y_con <- matched.ps$wage[matched.ps$gender == 'female']
t.test(y_tre, y_con, paired = T) 

#======================================================
#【Causal Forest，基于matching过的代码】
matched=matched.ps

library(grf)
ATE <- round(mean(matched[matched$gender == 'male',]$wage) - mean(matched[matched$gender == 'female',]$wage),2)
ATE

X <- matched[, c(-1,-7)] #去掉wage和gender
X_data <- model.matrix(~. , data=X)  #会自动在X中去构建subgroup，而不是指定occupation
Y <- matched$wage
W <- ifelse(matched$gender=='male',1,0) #treatment
m.cf <- causal_forest(X_data, Y, W) #X必须要是matrix，无法自己转换分类变量


varimp <- variable_importance(m.cf)
varimp
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X)[ranked.vars]

dim(X)

# cf.pred <- predict(m.cf)
summary(m.cf)
test_data <- model.matrix(~., data = test_input[,-6]) #再拿掉gender
cf.pred <- predict(m.cf, newdata = test_data) #这个test_input是没有wage的

results <- data.frame(test_input, cf.pred$predictions)
results 
#这个prediction的数值，代表给定用户profile得到的ATE
#若数值为2.68，意味着平均而言，如果给定其他变量，在这些变量水平一样的一个男性和一个女性比较小，男人工资比女人大2.68（存在因果关系）


加上预测能力评价









