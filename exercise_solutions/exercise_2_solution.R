## ----Q2, echo=SOLUTIONS-------------------------------------------------------
log(12.43)              # natural log
log10(12.43)            # log to base 10
log2(12.43)             # log to base 2
log(12.43, base = 2)    # alternative log to base 2
sqrt(12.43)             # square root
exp(12.43)              # exponent


## ----Q3, echo=SOLUTIONS-------------------------------------------------------
area_circle <- pi * (20/2)^2


## ----Q4, echo=SOLUTIONS-------------------------------------------------------
weight <- c(69, 62, 57, 59, 59, 64, 56, 66, 67, 66)


## ----Q5, echo=SOLUTIONS-------------------------------------------------------
mean(weight)                                # calculate mean 
var(weight)                                 # calculate variance
sd(weight)                                  # calculate standard deviation
range(weight)                               # range of weight values
length(weight)                              # number of observations

first_five <- weight[1:5]                  # extract first 5 weight values
first_five <- weight[c(1, 2, 3, 4, 5)]     # alternative method


## ----Q6, echo=SOLUTIONS-------------------------------------------------------
height <- c(112, 102, 83, 84, 99, 90, 77, 112, 133, 112)

summary(height)   # summary statistics of height variable

some_child <- height[c(2, 3, 9, 10)]      # extract the 2nd, 3rd, 9th, 10th height

shorter_child <- height[height <= 99]     # extract all heights less than or equal to 99


## ----Q7,echo=SOLUTIONS--------------------------------------------------------
bmi <- weight/(height/100)^2    # don't forget to convert height to meters


## ----Q8,echo=SOLUTIONS--------------------------------------------------------
seq1 <- seq(from = 0, to = 1, by = 0.1)

seq2 <- rev(seq(from = 1, to = 10, by = 0.5))


## ----Q9, echo=SOLUTIONS-------------------------------------------------------
rep(1:3, times = 3)                        # times repeats the whole vector
rep(c("a", "c", "e", "g"), each = 3)       # each repeats every element in turn


## ----Q10,echo=SOLUTIONS-------------------------------------------------------
height_sorted <- sort(height)

height_rev <- rev(sort(height))



## ----Q11,echo=SOLUTIONS-------------------------------------------------------
child_names <- c("Alfred", "Barbara", "James", "Jane", "John", "Judy", "Louise", "Mary", "Ronald", "William")


## ----Q12,echo=SOLUTIONS-------------------------------------------------------
height_ord <- order(height)   # get the indexes of the heights, smallest to tallest
names_sort <- child_names[height_ord]     # Louise is shortest, Ronald is tallest


## ----Q13,echo=SOLUTIONS-------------------------------------------------------
mydata <- c(2, 4, 1, 6, 8, 5, NA, 4, 7)
mean(mydata)    # returns NA!

mean(mydata, na.rm = TRUE)    # returns 4.625


## ----Q14,echo=SOLUTIONS-------------------------------------------------------
ls()          # list all variables in workspace
rm(seq1)      # remove variable seq1 from the workspace
ls()          # check seq1 has been removed

