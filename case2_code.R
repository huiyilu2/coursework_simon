
#import data from csv
student_data <- read.csv("C:/Users/odyss/Downloads/student_data.csv", header=T)
score <- read.csv("C:/Users/odyss/Downloads/evals_data.csv")

#compute aggregated score into same scale for 4 subjects
score$agg.score <- score$score_reading/8 + score$score_writing + score$score_mathNoCalc/8 + score$score_mathCalc/8

#look into the aggregate score summary and divide group into upper and lower class based on median
score.intake <- subset(score, program == "intake")
summary(score.intake$agg.score)
score.intake$class <- ifelse(score.intake$agg.score <= 251.5, "lower", "upper")
score <- merge(score.intake, score, all = T)

#fill in group info for each student
library(tidyr)
score.improvement <- score.improvement %>% fill(class)
score.improvement$is.upper <-ifelse(score.improvement$class == "upper", 1,0)

#calculate the difference of score by student, namely the improvement
library(dplyr)
score.improvement <- score %>% group_by(student_id) %>% 
  mutate(diff = agg.score - lag(agg.score))
score.improvement$diff <- ifelse(is.na(score.improvement$diff), 0, score.improvement$diff)

#calculate time difference, namely the participated duration of program
score.improvement <-mutate(score.improvement,timediff = difftime(date,lag(date),units ="days"))
score.improvement$timediff <- as.numeric(score.improvement$timediff)
score.improvement$timediff <- ifelse(is.na(score.improvement$timediff), 0, score.improvement$timediff)

#convert whether have participated program to dummy variable
score.improvement$skills <- ifelse(score.improvement$program == "skills", 1,0)
score.improvement$tutoring <- ifelse(score.improvement$program == "tutoring", 1,0)
score.improvement$refresh <- ifelse(score.improvement$program == "refresh", 1,0)

#convert location to dummy variable
score.improvement$is.online <- ifelse(score.improvement$location == "online", 1,0)

#calculate number of the program for each student
library(dplyr)
score.improvement <- score.improvement %>% group_by(student_id) %>% 
  mutate(program.number = length(unique(program))-1)

#subset to take out the intake data
score.improvement <- subset(score.improvement, program != "intake")

#run multi-variable regression
score.reg <- lm(data = score.improvement, formula = diff ~ skills + tutoring +refresh + timediff + is.online + is.upper + program.number + 0)
summary(score.reg)

#other insights
#why no progress or even have decreased score
decline <- subset(score.improvement, diff <=0)
decline.reg <- lm(data = decline, formula = diff ~ skills + tutoring + refresh + timediff + is.online + is.upper + program.number + 0)
summary(decline.reg)


#compare online and offline
online <- subset(score.improvement, location == "online")
offline <- subset(score.improvement, location == "center")   
summary(online$diff)
summary(offline$diff)

#conduct analysis for upper/lower class students
upper <- subset(score.improvement, class == "upper")
lower <- subset(score.improvement, class == "lower")
summary(lm(data = upper, formula = diff ~ skills + tutoring +refresh + timediff + is.online  + program.number + 0))
summary(lm(data = lower, formula = diff ~ skills + tutoring +refresh + timediff + is.online  + program.number + 0))
