rm(list=ls()) 
install.packages("Hmisc")
library(Hmisc) 

data <- read.csv("C:/Users/admin/files/Covid_R/COVID19_line_list_data.csv")
describe(data) 


data$death_dummy <- as.integer(data$death !=0)
unique(data$death_dummy)
sum(data$death_dummy / nrow(data))
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
#is this statistically significant?
t.test(alive$age, dead$age, alternative = "greater", conf.level = 0.95)
#normally, if p-value > 0.05, we fail to reject the null hypothesis
#here, p-value = 1, so we fail to reject the null hypothesis and 
#conclude that this is statistically significant
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) # 8.5%
mean(women$death_dummy, na.rm = TRUE)#3.7%
#is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative = "greater", conf.level = 0.99)
#p-value = 0.001053 < 0.05, so this is statistically significant

