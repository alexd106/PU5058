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

# and don't forget the 7 patients with no smoking status recorded
table(cardiac$Fsmoking, cardiac$Fsex, useNA = "ifany")


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
cardiac$bmi[cardiac$bmi > 100] <- NA

dotchart(cardiac$bmi, main = "bmi")

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


## ----Q8, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=8, fig.height=7------------
par(mfrow = c(2, 2))
hist(cardiac$bmi, main = "", xlab = "bmi")
hist(cardiac$systolic, main = "", xlab = "systolic")
hist(cardiac$tchol, main = "", xlab = "total cholesterol")
hist(cardiac$alcohol, main = "", xlab = "alcohol (units/week)")

# wide bins and narrow bins, drawn from exactly the same 163 numbers
par(mfrow = c(2, 1))
hist(cardiac$systolic, xlab = "systolic", main = "bins of 20 mmHg",
     breaks = seq(from = 100, to = 240, by = 20))
hist(cardiac$systolic, xlab = "systolic", main = "bins of 2 mmHg",
     breaks = seq(from = 100, to = 240, by = 2))

# with 20 mmHg bins the distribution looks smooth and roughly symmetric; with
# 2 mmHg bins it looks spiky and full of gaps, because blood pressure is
# recorded in whole even numbers. The data have not changed, only the picture.
# Treat the shape of a histogram as a rough guide, not as evidence.


## ----Q9, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=4------------
par(mfrow = c(1, 2))

# systolic and diastolic: a clear positive relationship, which is no surprise
# because the two numbers are the same measurement taken at two points in the
# heartbeat. Neither one explains the other, so there is no response variable
# here and the choice of axes really is arbitrary. The honest description is
# 'these two go together', not 'this one drives that one'.
plot(cardiac$systolic, cardiac$diastolic,
     xlab = "systolic (mmHg)", ylab = "diastolic (mmHg)")

# triglyceride and HDL cholesterol: negative, and a well known pattern. If you
# are going to treat one as explanatory it should be triglyceride, because a
# raised triglyceride is generally understood to drive HDL down rather than the
# other way round, so triglyceride belongs on the x axis. Do not lean on that
# too hard though. This is a cross sectional study, both are markers of the same
# underlying metabolic state, and no scatterplot can tell you which came first.
plot(cardiac$triglyceride, cardiac$hdlchol,
     xlab = "triglyceride (mmol/l)", ylab = "HDL cholesterol (mmol/l)")

# transforming a skewed variable
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

# Triglyceride logged cleanly in Exercise 4 because it has no zeros in it.
# Alcohol does. Check the minimum before you take the log of anything.


## ----Q10, echo=SOLUTIONS, eval=SOLUTIONS, tidy = TRUE-------------------------
# note: Fsmoking is the recoded smoking variable created in Q4
boxplot(tchol ~ Fsmoking, data = cardiac, xlab = "smoking status", ylab = "total cholesterol (mmol/l)")

# violin plot. install.packages("vioplot") first if you have not already
library(vioplot)
vioplot(tchol ~ Fsmoking, data = cardiac, xlab = "smoking status", ylab = "total cholesterol (mmol/l)", col = "lightblue")

# the three groups look remarkably similar. That is a perfectly good result:
# a plot that shows you there is nothing much to see has still told you
# something, and is a lot more honest than hunting for a variable that does
# show a difference.


## ----Q11, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=9, tidy = TRUE----
plot_vars <- c("age", "systolic", "diastolic", "tchol", "hdlchol", "triglyceride", "bmi")

# vanilla pairs plot
pairs(cardiac[, plot_vars])

# panel.smooth adds the wiggly line. Note that it keeps its dot: it is a base R
# function, not something you wrote
pairs(cardiac[, plot_vars], lower.panel = panel.smooth)

# systolic and diastolic are the most strongly related pair, which is no
# surprise, and hdlchol against triglyceride is the next most obvious, sloping
# the other way. age is related to almost nothing here, and that is worth
# thinking about: everyone in this study is between 55 and 75, so there simply
# is not enough spread in age for a relationship to show itself. A variable can
# look unimportant purely because of who was recruited.


## ----Q11b, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=9, tidy = TRUE----
panel_cor <- function(x, y, digits = 2, ...) {
  usr <- par("usr"); on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "pairwise.complete.obs")
  text(0.5, 0.5, format(r, digits = digits), cex = 1.4)
}

pairs(cardiac[, plot_vars], upper.panel = panel_cor, lower.panel = panel.smooth)

# systolic with diastolic is the strongest at r = 0.56, hdlchol with
# triglyceride the next at r = -0.43, and age never gets above 0.21 with
# anything. The eye had it right.


## ----Q12, echo=SOLUTIONS, eval=SOLUTIONS, results='hide'----------------------
# pdf is a vector format: the file stores the instructions for drawing the plot,
# so it stays perfectly sharp however far you enlarge it. Sizes are in inches.
pdf('output/ex5_cholesterol.pdf', width = 7, height = 5)
boxplot(tchol ~ Fsmoking, data = cardiac,
        xlab = "smoking status", ylab = "total cholesterol (mmol/l)")
dev.off()

# png is a bitmap: a grid of pixels, which goes blurry when enlarged. Word and
# PowerPoint handle it happily though. Always set res, because the default of
# 72 dpi looks fine on screen and disappointing on a slide or on paper.
png('output/ex5_cholesterol.png', width = 7, height = 5, units = "in", res = 300)
boxplot(tchol ~ Fsmoking, data = cardiac,
        xlab = "smoking status", ylab = "total cholesterol (mmol/l)")
dev.off()

# check they are really there. Your output directory should now contain
# ex5_cholesterol.pdf and ex5_cholesterol.png
list.files('output')

# tiff() works in exactly the same way and is what journals usually ask for.

# Rule of thumb: pdf if it is going to be printed or enlarged, png at 300 dpi
# if it is going into Word or PowerPoint, tiff if a journal asks for it.
# Whichever you choose, save it from R. Never screenshot the plot pane, and
# never paste a plot in at 72 dpi and hope nobody notices.

