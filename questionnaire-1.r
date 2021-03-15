data <-
  read.csv2(
    'A.I. in fashion design.csv',
    sep = ',',
    encoding = 'UTF-8',
    fileEncoding = "UTF-8-BOM"
  )
data$gender <- as.factor(data$gender)
data$occupation <- as.factor(data$occupation)
data$will_to_buy <- as.factor(data$will_to_buy)
data$how_much_to_pay <- as.factor(data$how_much_to_pay)
data$create_own_design <- as.factor(data$create_own_design)
data$all_pic <- data$pic_1 + data$pic_2 + data$pic_3 + data$pic_4

model_1 <-
  lm(pic_1 ~ age + gender + occupation + living_in + expenditure_on_fashion,
     data = data)
model_2 <-
  lm(pic_2 ~ age + gender + occupation + living_in + expenditure_on_fashion,
     data = data)
model_3 <-
  lm(pic_3 ~ age + gender + occupation + living_in + expenditure_on_fashion,
     data = data)
model_4 <-
  lm(pic_4 ~ age + gender + occupation + living_in + expenditure_on_fashion,
     data = data)
model_sum <-
  lm(all_pic ~ age + gender + occupation + living_in + expenditure_on_fashion,
     data = data)
model_own_design <-
  glm(
    create_own_design ~ age + gender + living_in + expenditure_on_fashion,
    data = data,
    family = binomial(link = 'logit'),
    control = list(maxit = 25)
  )
model_will_to_buy <-
  glm(
    will_to_buy ~ age + gender + living_in + expenditure_on_fashion,
    data = data,
    family = binomial(link = 'logit'),
    control = list(maxit = 25)
  )
model_how_much <-
  glm(
    how_much_to_pay ~ age + gender + living_in + expenditure_on_fashion,
    data = data,
    family = binomial(link = 'logit'),
    control = list(maxit = 25)
  )