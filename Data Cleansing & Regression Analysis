
student_data <- read.csv("C:/Users/odyss/Downloads/student_data.csv", header=T)
score <- read.csv("C:/Users/odyss/Downloads/evals_data.csv")
score$agg.score <- score$score_reading/8 + score$score_writing + score$score_mathNoCalc/8 + score$score_mathCalc/8
score.intake <- subset(score, program == "intake")
summary(score.intake$agg.score)

#define the group
score.intake$class <- ifelse(score.intake$agg.score <= 251.5, "lower", "upper")
score <- merge(score.intake, score, all = T)

#calculate the difference
library(dplyr)
score.improvement <- score %>% group_by(student_id) %>% 
  mutate(diff = agg.score - lag(agg.score))
score.improvement$diff <- ifelse(is.na(score.improvement$diff), 0, score.improvement$diff)

#fill in group info
library(tidyr)
score.improvement <- score.improvement %>% fill(class)
score.improvement$is.upper <-ifelse(score.improvement$class == "upper", 1,0)

#calculate time difference
score.improvement <-mutate(score.improvement,timediff = difftime(date,lag(date),units ="days"))
score.improvement$timediff <- as.numeric(score.improvement$timediff)
score.improvement$timediff <- ifelse(is.na(score.improvement$timediff), 0, score.improvement$timediff)

#convert program to dummy variable
score.improvement$skills <- ifelse(score.improvement$program == "skills", 1,0)
score.improvement$tutoring <- ifelse(score.improvement$program == "tutoring", 1,0)
score.improvement$refresh <- ifelse(score.improvement$program == "refresh", 1,0)

#convert location to dummy variable
score.improvement$is.online <- ifelse(score.improvement$location == "online", 1,0)

#subset without intake
score.improvement <- subset(score.improvement, program != "intake")

#test with multi-variable regression
summary(lm(data = score.improvement, formula = diff ~ skills + tutoring + refresh + timediff + is.online + is.upper))

#detect colinear
score.test <- score.improvement[,11:17]
cor(score.test)

