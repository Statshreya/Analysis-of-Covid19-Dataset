rm(list=ls()) #removes all variable stored previously
install.packages("Hmisc")
library(Hmisc) #import

data <- read.csv("C:/Users/manoj/Downloads/Covid_R/COVID19_line_list_data.csv")
describe(data) #Hmisc command

#cleaned up death column
data$death_dummy <- as.integer(data$death !=0)
unique(data$death_dummy)
# death rate
sum(data$death_dummy / nrow(data))

#AGE 
#claim: people who die are older
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
#is this statistically significant?

t.test(alive$age, dead$age, alternative = "greater", conf.level = 0.95)
#normally, if p-value > 0.05, we fail to reject the null hypothesis
#here, p-value = 1, so we fail to reject the null hypothesis and 
#conclude that this is statistically significant

#Gender
#claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) # 8.5%
mean(women$death_dummy, na.rm = TRUE)#3.7%
#is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative = "greater", conf.level = 0.99)
#p-value = 0.001053 < 0.05, so this is statistically significant

