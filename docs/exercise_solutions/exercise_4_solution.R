## ----Q0, echo=TRUE------------------------------------------------------------
cardiac <- read.table('data/cardiacdata.txt', header = TRUE, sep = "\t",
                      na.strings = "NA", stringsAsFactors = TRUE)

cardiac$Fsex <- factor(cardiac$sex, levels = c(1, 2),
                       labels = c("Female", "Male"))


## ----Q1, echo=SOLUTIONS, tidy = TRUE------------------------------------------
summary(cardiac)

# hdlchol has a minimum of 0.000, triglyceride has a minimum of 0.000, and
# bmi has a maximum of 514.60. A blood concentration of zero is not a low
# reading, it is impossible; you cannot have no cholesterol in your blood and
# still be alive to take part in a study. And a body mass index of 514 is not
# a very large person, it is a decimal point in the wrong place: 51.46 typed
# as 514.6.

# which patient has the impossible bmi?
cardiac[cardiac$bmi > 100, ]     # patient 1630L

# set the impossible values to NA. Putting the condition inside [ ] on the left
# of the arrow means 'the elements that match this condition become NA', and
# every other value is left alone.
cardiac$hdlchol[cardiac$hdlchol == 0] <- NA           # 2 patients
cardiac$triglyceride[cardiac$triglyceride == 0] <- NA # 1 patient
cardiac$bmi[cardiac$bmi > 100] <- NA                  # 1 patient

summary(cardiac)   # check: the minima and maximum are now sensible

#      tchol           hdlchol       triglyceride        bmi       
#  Min.   : 4.120   Min.   :0.720   Min.   :0.520   Min.   :17.57  
#  Median : 6.830   Median :1.360   Median :1.380   Median :25.20  
#  Mean   : 6.978   Mean   :1.421   Mean   :1.556   Mean   :25.72  
#  Max.   :11.660   Max.   :3.000   Max.   :4.670   Max.   :44.44  
#  NA's   :2        NA's   :4       NA's   :3       NA's   :1      

# Notice what that one bmi value was doing to the mean: 28.72 before, 25.72
# after. The median barely moved. One wrong number in 163 shifted the mean by
# three units.

# Why NA and not something else? Deleting the whole record throws away all the
# other measurements for that patient, which are perfectly good. Guessing the
# value - even a very reasonable guess like 51.46 - means inventing data, and
# nobody reading your results afterwards could tell which numbers you measured
# and which you made up. NA says exactly what you know: there should be a value
# here, and it isn't usable. Every R function that matters has an na.rm
# argument to cope with it.

# Note that you have changed the dataframe in your R session, not the file on
# disk. data/cardiacdata.txt still contains the original values, which is
# exactly as it should be: your raw data stays raw, and your script is the
# record of what you changed.


## ----Q2, echo=SOLUTIONS-------------------------------------------------------
# ordering on one variable first, to see what order() is doing
cardiac_sys_sort <- cardiac[order(cardiac$systolic), ]

# now systolic within smoking status. smoking comes first because it is the
# grouping you want, systolic second because it sorts within each group
cardiac_sorted <- cardiac[order(cardiac$smoking, cardiac$systolic), ]

# order() puts NAs last by default, so the 7 patients with no smoking status
# are all at the bottom rather than mixed in with the ones you can interpret.
tail(cardiac_sorted)


## ----Q3a, echo=TRUE-----------------------------------------------------------
mean(cardiac$age)          # mean age
median(cardiac$systolic)   # median systolic blood pressure
length(cardiac$tchol)      # number of observations


## ----Q3b, echo=SOLUTIONS, tidy = TRUE-----------------------------------------
# run it plainly first and see what the missing values do
aggregate(cardiac[, c(2, 4, 5, 6)], by = list(smoking = cardiac$smoking), FUN = mean)

#   smoking      age systolic diastolic tchol
# 1       1 63.10060 143.5400  76.40000    NA
# 2       2 66.40769 145.0385  77.48077  6.82
# 3       3 64.82019 139.2778  77.50000    NA

# two missing values in 163 have wiped out two of the three means. na.rm = TRUE
# is passed straight through to mean()
aggregate(cardiac[, c(2, 4, 5, 6)], by = list(smoking = cardiac$smoking), FUN = mean, na.rm = TRUE)

#   smoking      age systolic diastolic    tchol
# 1       1 63.10060 143.5400  76.40000 6.927143
# 2       2 66.40769 145.0385  77.48077 6.820000
# 3       3 64.82019 139.2778  77.50000 7.157170

# two grouping variables, both named
aggregate(cardiac[, c(2, 4, 5, 6)], by = list(smoking = cardiac$smoking, sex = cardiac$Fsex), FUN = mean, na.rm = TRUE)

# note that the 7 patients with no smoking status are dropped from every one of
# these summaries. aggregate() has no group to put them in.


## ----Q4, echo=SOLUTIONS-------------------------------------------------------
# using table
table(cardiac$smoking)
table(cardiac$smoking, cardiac$Fsex)

#     Female Male
#   1     26   24
#   2     15   37
#   3     37   17

# by default table() silently drops the missing values - 50 + 52 + 54 = 156,
# not 163. Ask for them explicitly:
table(cardiac$smoking, useNA = "ifany")


## ----Q5, echo=SOLUTIONS, tidy = TRUE------------------------------------------
cardiac$bmi_z <- (cardiac$bmi - mean(cardiac$bmi, na.rm = TRUE)) / sd(cardiac$bmi, na.rm = TRUE)

cardiac$systolic_z <- (cardiac$systolic - mean(cardiac$systolic, na.rm = TRUE)) / sd(cardiac$systolic, na.rm = TRUE)

# check: mean 0, standard deviation 1
mean(cardiac$bmi_z, na.rm = TRUE)   # 0
sd(cardiac$bmi_z, na.rm = TRUE)     # 1

# R also has scale() built in, which does exactly this arithmetic. It returns a
# matrix rather than a vector, hence the [, 1]
cardiac$bmi_z <- scale(cardiac$bmi)[, 1]

# b) the patient furthest from the average bmi, in either direction
i <- which.max(abs(cardiac$bmi_z))
cardiac[i, c("patno", "bmi", "bmi_z", "systolic", "systolic_z")]

#     patno   bmi    bmi_z systolic systolic_z
# 135 1199K 44.44 4.641475      188   2.089224

# patient 1199K is 4.6 standard deviations above the mean bmi and 2.1 above the
# mean systolic. Both are high, but the bmi is far more extreme, and you can
# only say that because the two are now on the same scale. This is a real
# patient with a real, if unusual, bmi of 44.44, not a data error.

# c) Standardising uses the mean and the standard deviation, and both of those
# are wrecked by a single impossible value. With the 514.6 still in place:
#
#   mean 28.72 and sd 38.50, instead of 25.72 and 4.03
#
# The bad value would score z = 12.62 and every one of the other 162 patients
# would be squashed into the range -0.29 to 0.41. Patient 1199K, who genuinely
# is unusual, would look utterly ordinary at z = 0.41. Cleaning is not tidying
# up before the real work starts. It IS the work, and doing it in the wrong
# order gives you a result that is wrong without ever looking wrong.


## ----Q6, echo=SOLUTIONS, tidy = TRUE------------------------------------------
followup <- read.table('data/cardiac_followup.txt', header = TRUE, sep = "\t",
                       na.strings = "NA", stringsAsFactors = TRUE)

nrow(cardiac)     # 163 patients at the start of the study
nrow(followup)    # 108 patients with follow-up measurements

cardiac_fu <- merge(cardiac, followup, by = "patno")
nrow(cardiac_fu)  # 108

# By default merge() keeps only the patients who appear in BOTH dataframes,
# which is called an inner join. 55 of the original patients have no follow-up
# measurements, so they have quietly disappeared. Nothing warned you about this.
# ALWAYS check the number of rows before and after a linkage.


## ----Q7, echo=SOLUTIONS, tidy = TRUE------------------------------------------
cardiac_all <- merge(cardiac, followup, by = "patno", all.x = TRUE)
nrow(cardiac_all)                      # 163 - everybody is kept

sum(is.na(cardiac_all$systolic10))     # 55 patients have no follow-up

# all.x = TRUE keeps every row of x (the first dataframe) and fills the missing
# columns with NA. This is a left join.

# Without 'by', merge() silently joins on every column name the two dataframes
# happen to share. Here that is just patno, so you get the right answer by luck.
# If both files also had a column called, say, 'systolic', merge() would try to
# match on that too and you would get almost no rows back - with no error, no
# warning, and no clue as to why. Always name your key.


## ----Q8, echo=SOLUTIONS, tidy = TRUE------------------------------------------
write.table(cardiac_all, "output/cardiac_clean.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

# Decision log - the sort of thing that belongs at the top of your script:
#
# Data: data/cardiacdata.txt, 163 patients, imported unchanged.
# 1. Fsex created as a factor version of sex, labelled Female (1) and Male (2).
#    The original sex column was left untouched.
# 2. hdlchol of 0.00 set to NA (2 patients). A HDL cholesterol of zero is
#    physiologically impossible and is almost certainly a missing value that
#    was recorded as 0.
# 3. triglyceride of 0.00 set to NA (1 patient). Same reasoning.
# 4. bmi of 514.60 set to NA (patient 1630L). Almost certainly 51.46 with the
#    decimal point misplaced, but as we cannot confirm that, NA rather than
#    a correction.
# 5. bmi_z and systolic_z added, standardised after the cleaning in step 4.
# 6. Ten year follow-up measurements linked on from data/cardiac_followup.txt,
#    keeping all 163 patients; 55 have no follow-up data.


## ----Qopt, echo=SOLUTIONS, tidy = TRUE----------------------------------------
smoke_codes <- read.table('data/smoking_lookup.txt', header = TRUE, sep = "\t",
                          na.strings = "NA", stringsAsFactors = TRUE)
smoke_codes

#   code smoking_status
# 1    1        Current
# 2    2             Ex
# 3    3          Never

cardiac_all <- merge(cardiac_all, smoke_codes, by.x = "smoking", by.y = "code", all.x = TRUE)

nrow(cardiac_all)    # still 163 - check every time!

table(cardiac_all$smoking_status, useNA = "ifany")

# Current      Ex   Never    <NA> 
#      50      52      54       7 

# The 7 patients with a missing smoking code get a missing label, which is
# right. This is how coded data is usually handled: the codes stay in the data,
# the meanings live in a lookup file, and anyone can see how one maps to the
# other. If a code is ever added or changed you edit one small file rather than
# hunting through your scripts for hard coded labels.

# and re-export, now with the labels attached. Add a line to the decision log
# in your script recording that you did this, exactly as in Q8.
write.table(cardiac_all, "output/cardiac_clean.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

