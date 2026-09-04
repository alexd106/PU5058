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
cardiac$hdlchol[cardiac$hdlchol == 0] <- NA          # 2 patients
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
cardiac_sys_sort <- cardiac[order(cardiac$systolic), ]


## ----Q3, echo=SOLUTIONS-------------------------------------------------------
cardiac_sorted <- cardiac[order(cardiac$smoking, cardiac$systolic), ]

# order() puts NAs last by default, so the 7 patients with no smoking status
# are all at the bottom rather than mixed in with the ones you can interpret.
tail(cardiac_sorted)


## ----Q4a, echo=TRUE-----------------------------------------------------------
mean(cardiac$age)          # mean age
median(cardiac$systolic)   # median systolic blood pressure
length(cardiac$tchol)      # number of observations


## ----Q4b, echo=SOLUTIONS------------------------------------------------------
tapply(cardiac$tchol, cardiac$smoking, mean)      # notice the NAs?

# use the na.rm argument again
tapply(cardiac$tchol, cardiac$smoking, mean, na.rm = TRUE)    

#        1        2        3 
# 6.927143 6.820000 7.157170 

# alternative method using the with() function. see ?with
with(cardiac, tapply(tchol, smoking, mean, na.rm = TRUE))   

# when using multiple grouping variables these need to be supplied as a list
tapply(cardiac$tchol, list(cardiac$smoking, cardiac$Fsex), median, na.rm = TRUE)

#   Female Male
# 1  7.090 6.46
# 2  7.560 6.28
# 3  7.025 7.03


## ----Q5, echo=SOLUTIONS, tidy = TRUE------------------------------------------
aggregate(cardiac[, c(2, 4, 5, 6)], by = list(smoking = cardiac$smoking), mean, na.rm = TRUE)

#   smoking      age systolic diastolic    tchol
# 1       1 63.10060 143.5400  76.40000 6.927143
# 2       2 66.40769 145.0385  77.48077 6.820000
# 3       3 64.82019 139.2778  77.50000 7.157170

aggregate(cardiac[, c(2, 4, 5, 6)], by = list(smoking = cardiac$smoking, sex = cardiac$Fsex), mean, na.rm = TRUE)


## ----Q6, echo=SOLUTIONS-------------------------------------------------------
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


## ----Q7, echo=SOLUTIONS, tidy = TRUE------------------------------------------
followup <- read.table('data/cardiac_followup.txt', header = TRUE, sep = "\t",
                       na.strings = "NA", stringsAsFactors = TRUE)

nrow(cardiac)     # 163 patients at the start of the study
nrow(followup)    # 108 patients with follow-up measurements

cardiac_fu <- merge(cardiac, followup, by = "patno")
nrow(cardiac_fu)  # 108

# By default merge() keeps only the rows that appear in BOTH dataframes, which
# is called an inner join. 55 of the original patients have no follow-up
# measurements, so they have quietly disappeared. Nothing warned you about this.
# ALWAYS check the number of rows before and after a join.


## ----Q8, echo=SOLUTIONS, tidy = TRUE------------------------------------------
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


## ----Q9, echo=SOLUTIONS, tidy = TRUE------------------------------------------
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


## ----Q10, echo=SOLUTIONS, tidy = TRUE-----------------------------------------
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
# 5. smoking labels joined on from data/smoking_lookup.txt.
# 6. Ten year follow-up measurements joined on from data/cardiac_followup.txt,
#    keeping all 163 patients; 55 have no follow-up data.

