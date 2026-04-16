# Predictive Modelling Final Exam Score

Load required libraries

library(car)

Load data

Read dataset
finalexam <- read.csv("final_exam.csv", header = TRUE)


Data summary

summary(finalexam)


# Full regression model

finalexam_model <- lm(
  FinalExamScores ~ StudyHours + Attendance + PreviousExamScores +
    SleepHours + ParticipationinStudyGroups + HomeworkCompletion +
    ExtracurricularActivities + ClassParticipation,
  data = finalexam
)

summary(finalexam_model)

# Histogram of final exam scores

hist(
  finalexam$FinalExamScores,
  main = "Histogram of Final Exam Scores",
  xlab = "Final Exam Scores"
)

# Boxplot: Attendance vs Scores

finalexam$AttendanceLabel <- factor(
  finalexam$Attendance,
  levels = c(0, 1),
  labels = c("<80%", ">80%")
)

boxplot(
  FinalExamScores ~ AttendanceLabel,
  data = finalexam,
  main = "Final Exam Scores by Attendance",
  xlab = "Attendance",
  ylab = "Final Exam Score",
  col = c("lightcoral", "lightblue")
)

# Scatterplot: Previous vs Final scores

exam_fit <- lm(FinalExamScores ~ PreviousExamScores, data = finalexam)

plot(
  finalexam$PreviousExamScores,
  finalexam$FinalExamScores,
  xlab = "Previous Exam Scores",
  ylab = "Final Exam Scores",
  main = "Final vs Previous Exam Scores"
)

abline(exam_fit)

# Reduced regression model

finalexam_reduced <- lm(
  FinalExamScores ~ StudyHours + PreviousExamScores +
    SleepHours + ParticipationinStudyGroups + HomeworkCompletion,
  data = finalexam
)

summary(finalexam_reduced)

Multicollinearity check

vif(finalexam_reduced)

Residual diagnostics

par(mfrow = c(2, 2))
plot(finalexam_reduced)

# Prediction: No study group participation

newdata_no_sg <- data.frame(
  StudyHours = 1:6,
  Attendance = 1,
  PreviousExamScores = 70,
  SleepHours = 7,
  ParticipationinStudyGroups = 0,
  HomeworkCompletion = 1,
  ExtracurricularActivities = 2,
  ClassParticipation = 0.2
)

predicted_no_sg <- predict(finalexam_model, newdata_no_sg)

# Plot predictions
par(mfrow = c(1, 1))
plot(
  1:6, predicted_no_sg,
  type = "l",
  xlab = "Study Hours per Week",
  ylab = "Predicted Final Exam Score",
  main = "Predicted Scores (No Study Groups)"
)
points(1:6, predicted_no_sg)


# Prediction: With study group participation

newdata_sg <- data.frame(
  StudyHours = 1:6,
  Attendance = 1,
  PreviousExamScores = 70,
  SleepHours = 7,
  ParticipationinStudyGroups = 1,
  HomeworkCompletion = 1,
  ExtracurricularActivities = 2,
  ClassParticipation = 0.2
)

predicted_sg <- predict(finalexam_model, newdata_sg)

# Plot predictions
plot(
  1:6, predicted_sg,
  type = "l",
  xlab = "Study Hours per Week",
  ylab = "Predicted Final Exam Score",
  main = "Predicted Scores (With Study Groups)"
)
points(1:6, predicted_sg)
