## ----Q4, echo=SOLUTIONS, eval=SOLUTIONS, results='hide'-----------------------
cardiac <- read.table('data/cardiacdata.txt', header = TRUE, sep = "\t", stringsAsFactors = TRUE)

str(cardiac)

# recode the two categorical variables as factors, keeping the originals
cardiac$Fsex <- factor(cardiac$sex, levels = c(1, 2),
                       labels = c("Female", "Male"))

cardiac$Fsmoking <- factor(cardiac$smoking, levels = c(1, 2, 3),
                           labels = c("Current", "Ex", "Never"))

str(cardiac)

#  $ Fsex    : Factor w/ 2 levels "Female","Male": 2 2 2 2 2 2 2 1 1 2 ...
#  $ Fsmoking: Factor w/ 3 levels "Current","Ex",..: NA NA NA NA NA NA NA 1 1 1 ...


## ----Q5, echo=SOLUTIONS, eval=SOLUTIONS, results='hide'-----------------------
table(cardiac$Fsmoking, cardiac$Fsex)

  #           Female Male
  # Current       26   24
  # Ex            15   37
  # Never         37   17

# the pattern is almost reversed between the sexes: most of the men are
# ex-smokers, most of the women have never smoked. The smallest cell has 15
# patients, which is small but not unusable.

table(cardiac$Fsmoking, cardiac$Fsex, useNA = "ifany")

  #           Female Male
  # Current       26   24
  # Ex            15   37
  # Never         37   17
  # <NA>           0    7

# all 7 of the patients with no smoking status are men, and not one is a
# woman. Missing values spread evenly through a dataset are a nuisance;
# missing values concentrated in one group are a bias, because every analysis
# that quietly drops them drops men only. You cannot tell which you have
# without looking, and `useNA = "ifany"` is how you look.


## ----Q6, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=8, fig.height=8------------
par(mfrow = c(2, 2))
dotchart(cardiac$bmi, main = "bmi")
dotchart(cardiac$systolic, main = "systolic")
dotchart(cardiac$tchol, main = "total cholesterol")
dotchart(cardiac$alcohol, main = "alcohol")

# the bmi plot is the striking one: a single point sits so far to the right
# that every other patient is squashed into a narrow strip on the left. You
# cannot see the shape of the bmi distribution at all, because one value is
# setting the scale for all 163.


## ----Q7, echo=SOLUTIONS, eval=SOLUTIONS, results='hide'-----------------------
which(cardiac$bmi > 100)
cardiac$bmi[161]

# the same three fixes you made in Exercise 4 Q1. Your cleaning lives in your
# script, not in the data file, so it has to run again every time you import
# the raw data. That is exactly what makes it reproducible, and it is the
# reason for writing it down rather than editing the spreadsheet.
cardiac$bmi[cardiac$bmi > 100] <- NA
cardiac$triglyceride[cardiac$triglyceride == 0] <- NA
cardiac$hdlchol[cardiac$hdlchol == 0] <- NA

dotchart(cardiac$bmi, main = "bmi")

# with that one value gone the axis rescales and you can see the rest of the
# patients properly: most between about 20 and 30, thinning out to four above
# 35, the largest of them at 44.44.

# What else stands out? One patient with a bmi of 44.44, one with a systolic
# pressure of 230 mmHg, and in the alcohol plot a handful of patients reporting
# 41, 64 and 82 units in a week when the median is 2.

# What should you do about them? NOTHING. Every one of those values is
# perfectly possible. A BMI of 44 is severe obesity, a systolic pressure of 230
# is a hypertensive crisis, and 82 units a week is a great deal of alcohol but
# people really do drink that much. These are not errors, they are patients.
# The difference between this question and the last one is the difference
# between a value that CANNOT be right and a value you did not expect, and only
# the first of those is yours to change. Deleting inconvenient data because it
# looks untidy is scientific fraud.


## ----Q8, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=4.5----------
par(mfrow = c(1, 2))
hist(cardiac$bmi, main = "", xlab = "bmi")
hist(cardiac$alcohol, main = "", xlab = "alcohol (units/week)")

# bmi is roughly symmetric, with most patients in the middle and a modest tail
# to the right. alcohol looks quite different: most patients drink little or
# nothing, and a small number drink a great deal, which gives a long tail
# stretching out to 82 units. You'll come back to alcohol in Q9.


## ----Q9, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=4.5----------
cardiac$alcohol_sqrt <- sqrt(cardiac$alcohol)

par(mfrow = c(1, 2))
hist(cardiac$alcohol, main = "untransformed", xlab = "alcohol (units/week)")
hist(cardiac$alcohol_sqrt, main = "square root", xlab = "sqrt(alcohol)")

# Yes. The heavy drinkers are no longer strung out along a long tail, the
# values are spread much more evenly across the range, and all 163 patients
# are still in the plot.

# Why not a log, as you used in Exercise 4? Try it and see what happens:
# hist(log(cardiac$alcohol))
# 58 of these patients drank nothing at all and log(0) is -Inf, so the plot you
# get is built from 105 patients rather than 163. R doesn't warn you, it
# doesn't produce an error, and the histogram looks perfectly reasonable.
# Triglyceride didn't have this problem, because its single zero had already
# been set to NA back in Exercise 4 Q1.


## ----Q10, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=4.5---------
par(mfrow = c(1, 2))

# Easy one. Carrying more weight raises blood pressure, not the other way round,
# so bmi is explanatory and goes on x. The relationship is real but weak, which
# is normal.
plot(cardiac$bmi, cardiac$systolic,
     xlab = "bmi", ylab = "systolic (mmHg)")

# Harder one. Neither clearly causes the other; both are markers of the same
# underlying metabolic state. If you have to choose, a raised triglyceride is
# usually taken to drive HDL down rather than the reverse, so triglyceride goes
# on x. The relationship is negative.
plot(cardiac$triglyceride, cardiac$hdlchol,
     xlab = "triglyceride (mmol/l)", ylab = "HDL cholesterol (mmol/l)")


## ----Q11, echo=SOLUTIONS, eval=SOLUTIONS--------------------------------------
# note: Fsmoking is the recoded smoking variable created in Q4
boxplot(hdlchol ~ Fsmoking, data = cardiac,
        xlab = "smoking status", ylab = "HDL cholesterol (mmol/l)")

# the patients who have never smoked have a higher HDL cholesterol than either
# the current smokers or the ex-smokers. Their median is 1.60, against 1.31 for
# the current smokers and 1.21 for the ex-smokers, and it sits above the upper
# quartile of both of the other groups.


## ----Q12, echo=SOLUTIONS, eval=SOLUTIONS, results='hide'----------------------
# violin plot. install.packages("vioplot") first if you have not already
library(vioplot)
vioplot(hdlchol ~ Fsmoking, data = cardiac, xlab = "smoking status",
        ylab = "HDL cholesterol (mmol/l)", col = "lightblue")

# the boxplot gives you the quartiles, the violin gives you the shape as well.
# Here they agree, which is reassuring rather than dull: it means the medians
# are not being propped up by one odd cluster of patients.

# now the same plot again, sent to a file instead of to the screen. Sizes are
# in inches. Nothing appears in the plot pane while these three lines run, and
# without the dev.off() the file would be left open and unreadable.
pdf('output/ex5_hdl_violin.pdf', width = 7, height = 5)
vioplot(hdlchol ~ Fsmoking, data = cardiac, xlab = "smoking status",
        ylab = "HDL cholesterol (mmol/l)", col = "lightblue")
dev.off()

# your output directory should now contain ex5_hdl_violin.pdf. pdf is a vector
# format, so it stays perfectly sharp however far you enlarge it.


## ----Q13, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=9-----------
plot_vars <- c("age", "systolic", "diastolic", "tchol", "hdlchol",
               "triglyceride", "bmi")

pairs(cardiac[, plot_vars])

# systolic and diastolic are the most strongly related pair, which is no
# surprise given that they are the same measurement taken at two points in the
# heartbeat. hdlchol against triglyceride is the next most obvious, sloping the
# other way.

# age is related to almost nothing here, and that's worth thinking about.
# Everyone in this study is between 55 and 75, so there isn't much spread in
# age for a relationship to show itself in. A variable can look unimportant
# just because of who was recruited.

