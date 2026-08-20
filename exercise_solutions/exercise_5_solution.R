## ----Q4, echo=SOLUTIONS-------------------------------------------------------
cardiac <- read.table('data/cardiacdata.txt', header = TRUE, sep = "\t",
                      na.strings = "NA", stringsAsFactors = TRUE)

str(cardiac)
summary(cardiac)

# recode the two categorical variables as factors, keeping the originals
cardiac$Fsex <- factor(cardiac$sex, levels = c(1, 2),
                       labels = c("Female", "Male"))

cardiac$Fsmoking <- factor(cardiac$smoking, levels = c(1, 2, 3),
                           labels = c("Current", "Ex", "Never"))

str(cardiac)

#  $ Fsex    : Factor w/ 2 levels "Female","Male": 2 2 2 2 2 2 2 1 1 2 ...
#  $ Fsmoking: Factor w/ 3 levels "Current","Ex",..: NA NA NA NA NA NA NA 1 1 1 ...


## ----Q5, echo=SOLUTIONS-------------------------------------------------------
table(cardiac$Fsmoking, cardiac$Fsex)

  #           Female Male
  # Current       26   24
  # Ex            15   37
  # Never         37   17

# the pattern is almost reversed between the sexes: most of the men are
# ex-smokers, most of the women have never smoked. The smallest cell has 15
# patients, which is small but not unusable.

xtabs(~ Fsmoking + Fsex, data = cardiac)

# and don't forget the 7 patients with no smoking status recorded
table(cardiac$Fsmoking, cardiac$Fsex, useNA = "ifany")


## ----Q6, echo=SOLUTIONS-------------------------------------------------------
pdf('output/ex5_dotplots.pdf')
par(mfrow = c(2, 2))
dotchart(cardiac$bmi, main = "bmi")
dotchart(cardiac$systolic, main = "systolic")
dotchart(cardiac$tchol, main = "total cholesterol")
dotchart(cardiac$alcohol, main = "alcohol")
dev.off()

# the bmi plot is the striking one: a single point sits so far to the right
# that every other patient is squashed into a narrow strip on the left. You
# cannot see the shape of the bmi distribution at all, because one value is
# setting the scale for all 163.


## ----Q7, echo=SOLUTIONS-------------------------------------------------------
which(cardiac$bmi > 100)
# [1] 161
cardiac$bmi[161]
# [1] 514.6
cardiac$bmi[cardiac$bmi > 100] <- NA

par(mfrow = c(2, 2))
dotchart(cardiac$bmi, main = "bmi")
dotchart(cardiac$systolic, main = "systolic")
dotchart(cardiac$tchol, main = "total cholesterol")
dotchart(cardiac$alcohol, main = "alcohol")

# Now the bmi plot is readable, and you can see the distribution properly:
# most patients between about 20 and 30, thinning out to a handful above 35,
# with one patient at 51.46.

# What else stands out? One patient with a bmi of 51.46, one with a systolic
# pressure of 230 mmHg, and in the alcohol plot a handful of patients reporting
# 41, 64 and 82 units in a week when the median is 2.

# What should you do about them? NOTHING. Every one of those values is
# perfectly possible. A BMI of 51 is severe obesity, a systolic pressure of 230
# is a hypertensive crisis, and 82 units a week is a great deal of alcohol but
# people really do drink that much. These are not errors, they are patients.
# The difference between this question and the last one is the difference
# between a value that CANNOT be right and a value you did not expect, and only
# the first of those is yours to change. Deleting inconvenient data because it
# looks untidy is scientific fraud.


## ----Q8, echo=SOLUTIONS-------------------------------------------------------
pdf('output/ex5_hist.pdf')
par(mfrow = c(2, 2))
hist(cardiac$bmi, main = "", xlab = "bmi")
hist(cardiac$systolic, main = "", xlab = "systolic")
hist(cardiac$tchol, main = "", xlab = "total cholesterol")
hist(cardiac$alcohol, main = "", xlab = "alcohol (units/week)")
dev.off()

# need the min and max of systolic to work out the limits for the breaks
summary(cardiac$systolic)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  104     127     140   142.9     156     230 

# experimenting with different breaks
par(mfrow = c(2, 2))
brk1 <- seq(from = 100, to = 240, by = 20)
hist(cardiac$systolic, xlab = "systolic", breaks = brk1, main = "brk: 20")

brk2 <- seq(from = 100, to = 240, by = 10)
hist(cardiac$systolic, xlab = "systolic", breaks = brk2, main = "brk: 10")

brk3 <- seq(from = 100, to = 240, by = 5)
hist(cardiac$systolic, xlab = "systolic", breaks = brk3, main = "brk: 5")

brk4 <- seq(from = 100, to = 240, by = 2)
hist(cardiac$systolic, xlab = "systolic", breaks = brk4, main = "brk: 2")

# with 20 mmHg bins the distribution looks smooth and roughly symmetric; with
# 2 mmHg bins it looks spiky and full of gaps, because blood pressure is
# recorded in whole even numbers. The data have not changed, only the picture.


## ----Q9, echo=SOLUTIONS-------------------------------------------------------
# a clear positive relationship, as you would expect: the two numbers are
# measurements of the same thing at different points in the heartbeat
plot(cardiac$systolic, cardiac$diastolic,
     xlab = "systolic (mmHg)", ylab = "diastolic (mmHg)")

# this one slopes the other way: patients with high HDL cholesterol tend to
# have low triglyceride. That is a real and well known pattern
plot(cardiac$hdlchol, cardiac$triglyceride,
     xlab = "HDL cholesterol (mmol/l)", ylab = "triglyceride (mmol/l)")

# transforming alcohol
cardiac$alcohol.sqrt <- sqrt(cardiac$alcohol)
cardiac$alcohol.log  <- log(cardiac$alcohol)

par(mfrow = c(1, 3))
hist(cardiac$alcohol, main = "untransformed", xlab = "alcohol")
hist(cardiac$alcohol.sqrt, main = "square root", xlab = "sqrt(alcohol)")
hist(cardiac$alcohol.log, main = "natural log", xlab = "log(alcohol)")

# The log transformation fails. 58 of the 163 patients reported drinking no
# alcohol at all, and log(0) is -Inf, not a number. R does not stop, it just
# quietly drops those 58 patients from the plot - more than a third of your
# data gone, with nothing more than a warning about non-finite values.
min(cardiac$alcohol)          # 0
log(0)                        # -Inf
sum(is.infinite(cardiac$alcohol.log))   # 58

# The square root works, because sqrt(0) is 0, and it pulls the long tail in
# nicely. If you did need a log, the usual dodge is log(x + 1).

jpeg('output/ex5_alcohol.jpeg')
hist(cardiac$alcohol.sqrt, main = "", xlab = "sqrt(alcohol)")
dev.off()


## ----Q10, echo=SOLUTIONS, tidy = TRUE-----------------------------------------
# note: Fsmoking is the recoded smoking variable created in Q4
boxplot(tchol ~ Fsmoking, data = cardiac, xlab = "smoking status", ylab = "total cholesterol (mmol/l)")

# violin plot
library(vioplot)
vioplot(tchol ~ Fsmoking, data = cardiac, xlab = "smoking status", ylab = "total cholesterol (mmol/l)", col = "lightblue")

# the three groups look remarkably similar. That is a perfectly good result:
# a plot that shows you there is nothing much to see has still told you
# something, and is a lot more honest than hunting for a variable that does
# show a difference.


## ----Q11, echo=SOLUTIONS, tidy = TRUE-----------------------------------------
# vanilla pairs plot
pairs(cardiac[, c("age", "systolic", "diastolic", "tchol", "hdlchol", "triglyceride", "bmi")])

# customise the plot. You need to define the panel.hist and panel.cor functions
# first - these are taken from the ?pairs help file
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey85", ...)
}

panel.cor <- function(x, y, digits = 2, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  text(0.5, 0.5, txt, cex = 1.2)
}

pairs(cardiac[, c("age", "systolic", "diastolic", "tchol", "hdlchol", "triglyceride", "bmi")],
      diag.panel = panel.hist, upper.panel = panel.cor, lower.panel = panel.smooth)

# systolic and diastolic are the most strongly related pair (r = 0.56), which
# is no surprise. hdlchol and triglyceride are the next strongest (r = -0.43)
# and negative. age is related to almost nothing here, and that is worth
# thinking about: everyone in this study is between 55 and 75, so there simply
# is not enough spread in age for a relationship to show itself. A variable can
# look unimportant purely because of who was recruited.

