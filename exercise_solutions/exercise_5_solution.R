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


## ----Q8, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=8, fig.height=6------------
par(mfrow = c(1, 2))
hist(cardiac$bmi, main = "", xlab = "bmi")
hist(cardiac$alcohol, main = "", xlab = "alcohol (units/week)")

# bmi is roughly symmetric, a nice recognisable hump with a modest tail to the
# right. alcohol isn't anything like it: a big spike at zero and a long thin
# tail stretching out to 82 units. Hang on to that, you come straight back to
# it in Q9.

# wide bins and narrow bins, drawn from exactly the same 163 numbers
par(mfrow = c(2, 1))
hist(cardiac$systolic, xlab = "systolic", main = "bins of 20 mmHg",
     breaks = seq(from = 100, to = 240, by = 20))
hist(cardiac$systolic, xlab = "systolic", main = "bins of 2 mmHg",
     breaks = seq(from = 100, to = 240, by = 2))

# with 20 mmHg bins the distribution looks smooth and roughly symmetric; with
# 2 mmHg bins it looks spiky and full of gaps. Nothing about the patients has
# changed, only the picture. The narrow version cuts 163 patients into 70 bins,
# so a typical bin holds about two people and one patient either way visibly
# changes its height. You aren't seeing more detail there, you're seeing noise,
# so it's worth treating the shape of a histogram as a rough guide rather than
# as evidence.


## ----Q9, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=8, fig.height=7, results='hide'----
cardiac$alcohol_sqrt <- sqrt(cardiac$alcohol)
cardiac$alcohol_log  <- log(cardiac$alcohol)
cardiac$alcohol_log1 <- log(cardiac$alcohol + 1)

par(mfrow = c(2, 2))
hist(cardiac$alcohol, main = "untransformed", xlab = "alcohol")
hist(cardiac$alcohol_sqrt, main = "square root", xlab = "sqrt(alcohol)")
hist(cardiac$alcohol_log, main = "natural log", xlab = "log(alcohol)")
hist(cardiac$alcohol_log1, main = "natural log of alcohol + 1",
     xlab = "log(alcohol + 1)")

sum(cardiac$alcohol == 0)   # 58

# the check, exactly the one you used in Exercise 4 Q6: how big is the gap
# between the mean and the median?
mean(cardiac$alcohol)        # 6.91
median(cardiac$alcohol)      # 2.00,  gap 4.91

mean(cardiac$alcohol_sqrt)   # 1.86
median(cardiac$alcohol_sqrt) # 1.41,  gap 0.44

mean(cardiac$alcohol_log1)   # 1.33
median(cardiac$alcohol_log1) # 1.10,  gap 0.23

# The plain log is the one that fails. 58 of the 163 patients reported drinking
# no alcohol at all, log(0) is -Inf, and R drops more than a third of your data
# from the plot with nothing more than a warning.

# Adding 1 first is the usual way round it, and it costs you almost nothing.
# log(0 + 1) is 0, an ordinary number, so nobody is thrown away. For the
# patients who did drink, adding 1 barely moves them: the heaviest drinker
# becomes log(83) = 4.419 instead of log(82) = 4.407. 1 is the natural choice
# here because it sends the zeros to zero and because alcohol is recorded in
# whole units, so 1 is the smallest step the variable can actually take.

# Both survivors work, and the log of alcohol plus 1 works better. The gap
# between mean and median falls from 4.91 to 0.44 under the square root and to
# 0.23 under the log. Look at the histograms and you can see the same thing.

# Two things to be honest about. First, the constant is your choice, not the
# data's, and it matters: adding 0.5 instead of 1 leaves a gap of 0.10, and
# adding 0.1 overshoots so far that the mean drops below the median and the
# skew tips the other way. Whichever you use, say so. Second, no transformation
# is going to make 58 patients stop being zero. They are still a third of your
# data sitting on a single value. A transformation can pull a tail in; it
# cannot turn a variable into something it is not.

# Triglyceride logged cleanly in Exercise 4 with no constant needed, because
# you had already set its one zero to NA in Q1 of that exercise. The zeros in
# alcohol are quite different: they are real readings, so there is nothing to
# clean and you have to deal with them instead. Check the minimum before you
# take the log of anything.


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


## ----Q14, echo=SOLUTIONS, eval=SOLUTIONS, fig.width=9, fig.height=9-----------
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

