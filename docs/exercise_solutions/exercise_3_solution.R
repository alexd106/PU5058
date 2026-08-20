## ----Q5, echo=SOLUTIONS-------------------------------------------------------
cardiac <- read.table('data/cardiacdata.txt', header = TRUE, sep = "\t",
                      na.strings = "NA", stringsAsFactors = TRUE)


## ----Q6, echo=SOLUTIONS-------------------------------------------------------
head(cardiac)         # display the first few rows 
names(cardiac)        # display the variable names
str(cardiac)          # display the structure of the dataframe cardiac

# 'data.frame':	163 obs. of  11 variables:
#  $ patno       : Factor w/ 163 levels "0049B","0052H",..: 6 54 65 69 ...
#  $ age         : num  71 68.2 62.9 69.9 65 ...
#  $ sex         : int  2 2 2 2 2 2 2 1 1 2 ...
#  $ systolic    : int  142 140 156 154 187 123 147 140 131 128 ...
#  $ diastolic   : int  63 78 82 102 131 63 66 67 73 62 ...
#  $ tchol       : num  7.03 5.32 9.34 7.19 8.84 6.17 6.2 6.96 7.02 7.21 ...
#  $ hdlchol     : num  1.4 0.88 0.92 1.31 1.83 1.58 1.65 1.68 1.57 1.08 ...
#  $ triglyceride: num  0.81 3.4 4.67 2.53 1.76 0.73 1.11 0.69 1.29 1.16 ...
#  $ bmi         : num  24.7 26 26.6 26.2 26.1 ...
#  $ alcohol     : int  9 2 7 24 14 8 0 2 0 0 ...
#  $ smoking     : int  NA NA NA NA NA NA NA 1 1 1 ...

# 163 observations and 11 variables.

# R thinks sex and smoking are integers, because that is how they are stored
# in the file: sex is coded 1 and 2, smoking is coded 1, 2 and 3. But they are
# not really numbers, they are categories, and R has no way of knowing that.
# Nothing stops you calculating mean(cardiac$sex) - it returns 1.52 - and that
# number is meaningless. You will convert these to factors in Exercise 4.

# patno is a factor with 163 levels, one for every row. A factor with as many
# levels as there are rows is a good sign that you are looking at an identifier
# rather than a variable.


## ----Q7, echo=SOLUTIONS-------------------------------------------------------
summary(cardiac)

# NOTE: only some of the columns are shown here, to save space

 #     tchol           hdlchol       triglyceride        bmi        
 #  Min.   : 4.120   Min.   :0.000   Min.   :0.000   Min.   : 17.57  
 #  1st Qu.: 6.140   1st Qu.:1.080   1st Qu.:1.040   1st Qu.: 22.88  
 #  Median : 6.830   Median :1.360   Median :1.380   Median : 25.20  
 #  Mean   : 6.978   Mean   :1.404   Mean   :1.547   Mean   : 28.72  
 #  3rd Qu.: 7.720   3rd Qu.:1.700   3rd Qu.:1.870   3rd Qu.: 28.24  
 #  Max.   :11.660   Max.   :3.000   Max.   :4.670   Max.   :514.60  
 #  NA's   :2        NA's   :2       NA's   :2                       

# Four variables have missing values: tchol, hdlchol and triglyceride have 2
# each, and smoking has 7.

# The maximum bmi is 514.60. A body mass index of 514 is not possible; the
# heaviest person ever recorded had a BMI of around 200. Notice that nothing
# went wrong when you imported the file, and R will happily calculate a mean
# from it (28.72, when the median is 25.20). It is simply a wrong number sitting
# quietly in a column of right ones. You will deal with it in Exercise 4.


## ----Q8, echo=SOLUTIONS-------------------------------------------------------
# first 10 rows and first 4 columns
cardiac_sub <- cardiac[1:10, 1:4]                                      

# all rows and the columns patno, sex, smoking and tchol
cardiac_risk <- cardiac[, c(1, 3, 11, 6)] 
# alternative way of indexing columns with named indexes - much easier to read,
# and it still works if the column order changes
cardiac_risk <- cardiac[, c("patno", "sex", "smoking", "tchol")]    

# first 50 rows and all columns
cardiac_50 <- cardiac[1:50, ]  

# excluding first 10 rows and last column using negative indexing
cardiac_last <- cardiac[-c(1:10), -11]  
# more general way if you have lots of columns
cardiac_last <- cardiac[-c(1:10), -c(ncol(cardiac))] 
# NOTE: negative indexing does NOT work with column names. Uncomment the line
# below and run it to see the error for yourself:
# cardiac_last <- cardiac[-c(1:10), -c("smoking")]

