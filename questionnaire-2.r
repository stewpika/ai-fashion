data <-
  read.csv2(
    'Future of fashion design_numerical.csv',
    sep = ',',
    encoding = 'UTF-8',
    fileEncoding = "UTF-8-BOM"
  )
data$gender <- as.factor(data$gender)
data$occupation <- as.factor(data$occupation)
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
