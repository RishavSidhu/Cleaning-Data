#3.1
##a
df <- read.csv("dirty_iris.csv", stringsAsFactors = F)
sapply(df, class)
##b
cases <- complete.cases(df)
cases
sum(cases)
complete_cases <- sum(cases)/length(cases)*100
complete_cases
##c
is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}
i <- data.frame(sapply(df, is.special))
for(j in 1:ncol(i)){
  tmp <- rep(NA, length(df[i[[j]], j]))
  df[i[[j]], j] <- tmp
}
df
#3.2
##a
library(editrules)
E <- editfile("rules.txt")
E
##b
ve <- violatedEdits(E, df)
summary(ve)
plot(ve)
##c
per_correct <- 90/150*100
per_correct
##d
E1 <- editset(c("Sepal.Length > Petal.Length"))
ve1 <- violatedEdits(E1, df)
ve1
##e
boxplot(df$Sepal.Length)
outliers <- boxplot.stats(df$Sepal.Length)$out
outliers
df1 <- df
i <- df1$Sepal.Length %in% outliers
df1$Sepal.Length[i] <- rep(NA, length(outliers))
boxplot(df1$Sepal.Length)
#3.3
##a
if (!require("deducorrect")) install.packages("deducorrect")
library(deducorrect)
R <- correctionRules("correction.txt")
cor <- correctWithRules(R, df1)
cor$corrections
df1 <- cor$corrected
##b
le <- localizeErrors(E, df1)
summary(le$adapt)
summary(violatedEdits(E, df1))

i <- which(df1$Petal.Length <= 0)
df1[i, "Petal.Length"] <- NA
i <- which(df1$Sepal.Length <= 0)
df1[i, "Sepal.Length"] <- NA
i <- which(df1$Petal.Width <= 0)
df1[i, "Petal.Width"] <- NA
i <- which(df1$Sepal.Width <= 0)
df1[i, "Sepal.Width"] <- NA

i <- which(df$Petal.Length >= df$Sepal.Length)
df[i, "Petal.Length"] <- NA

le <- localizeErrors(E, df1)
summary(violatedEdits(E, df1))
