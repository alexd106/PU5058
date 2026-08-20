## ----Q0, echo=TRUE------------------------------------------------------------
cardiac <- read.table('data/cardiacdata.txt', header = TRUE, sep = "\t",
                      na.strings = "NA", stringsAsFactors = TRUE)

# 1 = Female and 2 = Male. Writing the labels out means you never have to
# remember the codes again, and your output reads properly.
cardiac$sex <- factor(cardiac$sex, levels = c(1, 2), labels = c("Female", "Male"))


## ----Q1, echo=SOLUTIONS, tidy = TRUE------------------------------------------
cardiac.sys160 <- cardiac[cardiac$systolic > 160, ]

cardiac.dia90 <- cardiac[cardiac$diastolic > 90, ]

cardiac.never <- cardiac[cardiac$smoking == 3, ]

cardiac.f.current <- cardiac[cardiac$smoking == 1 & cardiac$sex == "Female", ]

cardiac.subset <- cardiac[cardiac$sex == "Male" & cardiac$smoking == 3 & cardiac$diastolic > 76, ]

cardiac.bmi.trig <- cardiac[cardiac$bmi > 25 & cardiac$bmi < 30 & cardiac$triglyceride > 1 & cardiac$triglyceride < 2, ]

cardiac.notex <- cardiac[cardiac$smoking != 2, ]


## ----Q2, echo=SOLUTIONS, tidy = TRUE------------------------------------------
cardiac.subset <- cardiac[cardiac$sex == "Male" & cardiac$smoking == 3 & cardiac$diastolic > median(cardiac$diastolic), ]


## ----Q3, echo=SOLUTIONS, tidy = TRUE------------------------------------------
# results in a dataframe filled with NAs. 
cardiac.new <- cardiac[cardiac$systolic > 160 & cardiac$tchol > mean(cardiac$tchol), ]

# the variable tchol contains 2 NA values. By default the mean function will return an NA.
# use the na.rm argument to ignore NAs
cardiac.new <- cardiac[cardiac$systolic > 160 & cardiac$tchol > mean(cardiac$tchol, na.rm = TRUE), ]  


## ----Q4, echo=SOLUTIONS, tidy = TRUE------------------------------------------
subset(cardiac, sex == "Female" & age < 65 & systolic > 140)

subset(cardiac, sex == "Male" & bmi > 30, select = c("patno", "sex", "bmi", "systolic", "tchol"))


## ----Q5, echo=SOLUTIONS, tidy = TRUE------------------------------------------
summary(cardiac)

# hdlchol has a minimum of 0.000, triglyceride has a minimum of 0.000, and
# bmi has a maximum of 514.60. A blood concentration of zero is not a low
# reading, it is impossible; you cannot have no cholesterol in your blood and
# still be alive to take part in a study. And a body mass index of 514 is not
# a very large person, it is a decimal point in the wrong place: 51.46 typed
# as 514.6.

# which patients are they?
cardiac[cardiac$hdlchol == 0 & !is.na(cardiac$hdlchol), ]
cardiac[cardiac$triglyceride == 0 & !is.na(cardiac$triglyceride), ]
cardiac[cardiac$bmi > 100, ]

# set the impossible values to NA
cardiac$hdlchol[cardiac$hdlchol == 0] <- NA
cardiac$triglyceride[cardiac$triglyceride == 0] <- NA
cardiac$bmi[cardiac$bmi > 100] <- NA

summary(cardiac)   # check: the minima and maximum are now sensible

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


## ----Q6, echo=SOLUTIONS-------------------------------------------------------
cardiac.sys.sort <- cardiac[order(cardiac$systolic), ]


## ----Q7, echo=SOLUTIONS-------------------------------------------------------
# notice where the patients with a missing smoking status end up - and why
cardiac.sorted <- cardiac[order(cardiac$smoking, cardiac$systolic), ]        

# use '-' to reverse the order of systolic
cardiac.rev.sorted <- cardiac[order(cardiac$smoking, -cardiac$systolic), ]   


## ----Q8a, echo=SOLUTIONS------------------------------------------------------
mean(cardiac$age)          # mean age
median(cardiac$systolic)   # median systolic blood pressure
length(cardiac$tchol)      # number of observations


## ----Q8b, echo=SOLUTIONS------------------------------------------------------
tapply(cardiac$tchol, cardiac$smoking, mean)      # notice the NAs?

# use the na.rm argument again
tapply(cardiac$tchol, cardiac$smoking, mean, na.rm = TRUE)    

# alternative method using the with() function. see ?with
with(cardiac, tapply(tchol, smoking, mean, na.rm = TRUE))   

# when using multiple grouping variables these need to be supplied as a list
tapply(cardiac$tchol, list(cardiac$smoking, cardiac$sex), median, na.rm = TRUE)


## ----Q9, echo=SOLUTIONS, tidy = TRUE------------------------------------------
aggregate(cardiac[, c(2, 4, 5, 6)], by = list(smoking = cardiac$smoking), mean, na.rm = TRUE)

aggregate(cardiac[, c(2, 4, 5, 6)], by = list(smoking = cardiac$smoking, sex = cardiac$sex), mean, na.rm = TRUE)

# optional question. Need to specify a function 'on the fly' using function(x){}
aggregate(cardiac[, c(2, 4, 5, 6)], by = list(smoking = cardiac$smoking, sex = cardiac$sex), function(x){round(mean(x, na.rm = TRUE), digits = 2)})


## ----Q10, echo=SOLUTIONS------------------------------------------------------
# using table
table(cardiac$smoking)
table(cardiac$smoking, cardiac$sex)

# by default table() silently drops the missing values - 50 + 52 + 54 = 156,
# not 163. Ask for them explicitly:
table(cardiac$smoking, useNA = "ifany")

# using xtabs
xtabs(~ smoking, data = cardiac)
xtabs(~ sex + smoking, data = cardiac)


## ----Q11, echo=SOLUTIONS, tidy = TRUE-----------------------------------------
followup <- read.table('data/cardiac_followup.txt', header = TRUE, sep = "\t",
                       na.strings = "NA", stringsAsFactors = TRUE)

nrow(cardiac)     # 163 patients at the start of the study
nrow(followup)    # 108 patients with follow-up measurements

cardiac.fu <- merge(cardiac, followup, by = "patno")
nrow(cardiac.fu)  # 108

# By default merge() keeps only the rows that appear in BOTH dataframes, which
# is called an inner join. 55 of the original patients have no follow-up
# measurements, so they have quietly disappeared. Nothing warned you about this.
# ALWAYS check the number of rows before and after a join.


## ----Q12, echo=SOLUTIONS, tidy = TRUE-----------------------------------------
cardiac.all <- merge(cardiac, followup, by = "patno", all.x = TRUE)
nrow(cardiac.all)                      # 163 - everybody is kept

sum(is.na(cardiac.all$systolic10))     # 55 patients have no follow-up

# all.x = TRUE keeps every row of x (the first dataframe) and fills the missing
# columns with NA. This is a left join.

# Without 'by', merge() silently joins on every column name the two dataframes
# happen to share. Here that is just patno, so you get the right answer by luck.
# If both files also had a column called, say, 'systolic', merge() would try to
# match on that too and you would get almost no rows back - with no error, no
# warning, and no clue as to why. Always name your key.


## ----Q13, echo=SOLUTIONS, tidy = TRUE-----------------------------------------
smoke.codes <- read.table('data/smoking_lookup.txt', header = TRUE, sep = "\t",
                          na.strings = "NA", stringsAsFactors = TRUE)
smoke.codes

#   code smoking_status
# 1    1        Current
# 2    2             Ex
# 3    3          Never

cardiac.all <- merge(cardiac.all, smoke.codes, by.x = "smoking", by.y = "code", all.x = TRUE)

nrow(cardiac.all)    # still 163 - check every time!

table(cardiac.all$smoking_status, useNA = "ifany")

# Current      Ex   Never    <NA> 
#      50      52      54       7 

# The 7 patients with a missing smoking code get a missing label, which is
# right. This is how coded data is usually handled: the codes stay in the data,
# the meanings live in a lookup file, and anyone can see how one maps to the
# other. If a code is ever added or changed you edit one small file rather than
# hunting through your scripts for hard coded labels.


## ----Q14, echo=SOLUTIONS, tidy = TRUE-----------------------------------------
write.table(cardiac.all, "output/cardiac_clean.txt", col.names = TRUE, row.names = FALSE, sep = "\t")

# Decision log - the sort of thing that belongs at the top of your script:
#
# Data: data/cardiacdata.txt, 163 patients, imported unchanged.
# 1. hdlchol of 0.00 set to NA (2 patients). A HDL cholesterol of zero is
#    physiologically impossible and is almost certainly a missing value that
#    was recorded as 0.
# 2. triglyceride of 0.00 set to NA (1 patient). Same reasoning.
# 3. bmi of 514.60 set to NA (1 patient). Almost certainly 51.46 with the
#    decimal point misplaced, but as we cannot confirm that, NA rather than
#    a correction.
# 4. sex converted to a factor with labels Female (1) and Male (2).
# 5. smoking labels joined on from data/smoking_lookup.txt.
# 6. Ten year follow-up measurements joined on from data/cardiac_followup.txt,
#    keeping all 163 patients; 55 have no follow-up data.

