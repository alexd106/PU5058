## ----Q4, echo=SOLUTIONS, eval=SOLUTIONS---------------------------------------
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


## ----Q5, echo=SOLUTIONS, eval=SOLUTIONS---------------------------------------
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


## ----Q7, echo=SOLUTIONS, eval=SOLUTIONS---------------------------------------
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

# Now the bmi plot is readable, and you can see the distribution properly:
# most patients between about 20 and 30, thinning out to four patients above
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


## ----Q8, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=8, fig.height=6------------
par(mfrow = c(1, 2))
hist(cardiac$bmi, main = "", xlab = "bmi")
hist(cardiac$alcohol, main = "", xlab = "alcohol (units/week)")

# bmi is roughly symmetric, a recognisable hump with a modest tail to the
# right. alcohol is nothing of the sort: a huge spike at zero and a long thin
# tail stretching out to 82 units. Hold on to that difference, you come back
# to it in Q10.

# wide bins and narrow bins, drawn from exactly the same 163 numbers
par(mfrow = c(2, 1))
hist(cardiac$systolic, xlab = "systolic", main = "bins of 20 mmHg",
     breaks = seq(from = 100, to = 240, by = 20))
hist(cardiac$systolic, xlab = "systolic", main = "bins of 2 mmHg",
     breaks = seq(from = 100, to = 240, by = 2))

# with 20 mmHg bins the distribution looks smooth and roughly symmetric; with
# 2 mmHg bins it looks spiky and full of gaps. Nothing about the patients has
# changed, only the picture. The 2 mmHg version cuts 163 patients into 70 bins,
# so a typical bin holds two people and one patient either way visibly changes
# its height. Narrow bins do not show you more detail, they show you noise.
# Treat the shape of a histogram as a rough guide, not as evidence.


## ----Q9, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=4.5----------
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


## ----Q10, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=4-----------
cardiac$alcohol_sqrt <- sqrt(cardiac$alcohol)
cardiac$alcohol_log  <- log(cardiac$alcohol)

par(mfrow = c(1, 3))
hist(cardiac$alcohol, main = "untransformed", xlab = "alcohol")
hist(cardiac$alcohol_sqrt, main = "square root", xlab = "sqrt(alcohol)")
hist(cardiac$alcohol_log, main = "natural log", xlab = "log(alcohol)")

sum(cardiac$alcohol == 0)   # 58

# The log fails. 58 of the 163 patients reported drinking no alcohol at all, and
# log(0) is -Inf, so R quietly drops more than a third of your data from the
# plot with nothing more than a warning. The square root works, because sqrt(0)
# is 0, and it pulls the long tail in nicely.

# Triglyceride logged cleanly in Exercise 4 only because you had already set
# its one zero to NA in Q1 of that exercise, and you did the same again in Q7
# above. The zeros in alcohol are quite different: they are real, and they are
# more than a third of the patients, so there is nothing to set to NA. Check
# the minimum before you take the log of anything.


## ----Q11, echo=SOLUTIONS, eval=SOLUTIONS--------------------------------------
# note: Fsmoking is the recoded smoking variable created in Q4
boxplot(hdlchol ~ Fsmoking, data = cardiac,
        xlab = "smoking status", ylab = "HDL cholesterol (mmol/l)")

boxplot(tchol ~ Fsmoking, data = cardiac,
        xlab = "smoking status", ylab = "total cholesterol (mmol/l)")

# HDL is the one with something in it. The never-smokers sit clearly above the
# other two, with a median of 1.60 against 1.31 for current smokers and 1.21
# for ex-smokers, and their median is above the upper quartile of both other
# groups. Total cholesterol shows nothing at all: three boxes at much the same
# height, medians of 6.91, 6.58 and 7.03.

# Which would you leave out? The honest answer is that the temptation is to
# report the HDL plot and let the cholesterol one quietly disappear. Do not.
# A plot that shows you there is nothing to see has still told you something,
# and keeping only the comparisons that worked is how a set of results stops
# being evidence.

# violin plot. install.packages("vioplot") first if you have not already
library(vioplot)
vioplot(hdlchol ~ Fsmoking, data = cardiac, xlab = "smoking status",
        ylab = "HDL cholesterol (mmol/l)", col = "lightblue")

# the boxplot gives you the quartiles, the violin gives you the shape as well.
# Here they agree, which is reassuring rather than dull: it means the medians
# are not being propped up by one odd cluster of patients.


## ----Q12, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=9, tidy = TRUE----
plot_vars <- c("age", "systolic", "diastolic", "tchol", "hdlchol", "triglyceride", "bmi")

# vanilla pairs plot
pairs(cardiac[, plot_vars])

# panel.smooth adds the wiggly line. Note that it keeps its dot: it is a base R
# function, not something you wrote
pairs(cardiac[, plot_vars], lower.panel = panel.smooth)

# systolic and diastolic are the most strongly related pair, which is no
# surprise given that they are the same measurement taken at two points in the
# heartbeat. hdlchol against triglyceride is the next most obvious, sloping the
# other way. age is related to almost nothing here, and that is worth
# thinking about: everyone in this study is between 55 and 75, so there simply
# is not enough spread in age for a relationship to show itself. A variable can
# look unimportant purely because of who was recruited.


## ----Q13, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=9, tidy = TRUE----
panel_cor <- function(x, y, digits = 2, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "pairwise.complete.obs")
  text(0.5, 0.5, format(r, digits = digits), cex = 1.4)
}

pairs(cardiac[, plot_vars], upper.panel = panel_cor, lower.panel = panel.smooth)

# systolic with diastolic is the strongest at r = 0.56, hdlchol with
# triglyceride the next at r = -0.51, and age never gets above 0.21 with
# anything. The eye had it right.

# That -0.51 is worth a second look. Before the cleaning in Q7 it was -0.43,
# and the whole of the difference is two patients recorded with an HDL of
# zero. Two impossible values out of 163 moved a correlation by 0.08.

