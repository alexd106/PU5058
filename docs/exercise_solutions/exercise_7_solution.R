## ````md
## # Cardiac study report
## 
## ## Background
## 
## These data come from a **cohort study** of the risk factors for cardiovascular disease.
## Each row is one patient, and each column is something that was *measured* at the start
## of the study.
## 
## The variables we will look at are:
## 
## - age, in years
## - systolic blood pressure, in mmHg
## - body mass index
## - smoking status
## ````

## ````md
## ```{r import}
## cardiac <- read.table('data/cardiacdata.txt', header = TRUE, sep = "\t", stringsAsFactors = TRUE)
## 
## cardiac$Fsex <- factor(cardiac$sex, levels = c(1, 2),
##                        labels = c("Female", "Male"))
## 
## cardiac$Fsmoking <- factor(cardiac$smoking, levels = c(1, 2, 3),
##                            labels = c("Current", "Ex", "Never"))
## ```
## 
## ```{r structure}
## str(cardiac)
## ```
## ````

## ````md
## ## Data validation
## 
## ```{r validation, echo = TRUE}
## # count the impossible values before changing them, so we can report how many
## n_hdl  <- sum(cardiac$hdlchol == 0, na.rm = TRUE)
## n_trig <- sum(cardiac$triglyceride == 0, na.rm = TRUE)
## 
## cardiac$bmi[cardiac$bmi > 100] <- NA
## cardiac$triglyceride[cardiac$triglyceride == 0] <- NA
## cardiac$hdlchol[cardiac$hdlchol == 0] <- NA
## ```
## 
## Three variables contained values that are not biologically possible. `r n_hdl` patients
## had an HDL cholesterol of 0 mmol/l and `r n_trig` had a triglyceride of 0 mmol/l, and
## nobody can have no fat at all in their blood. One patient had a body mass index of 514.6,
## which is almost certainly 51.46 with the decimal point in the wrong place. All four values
## were set to `NA` rather than deleted or corrected. Deleting the patients would throw away
## their other measurements, which are perfectly good, and correcting the body mass index
## would mean putting a number into the data that nobody measured.
## 
## ```{r alcohol-transform, fig.cap = 'Weekly alcohol consumption before and after square root transformation.'}
## cardiac$alcohol_sqrt <- sqrt(cardiac$alcohol)
## 
## par(mfrow = c(1, 2))
## hist(cardiac$alcohol, main = "", xlab = "alcohol (units/week)")
## hist(cardiac$alcohol_sqrt, main = "", xlab = "sqrt(alcohol)")
## ```
## 
## A small number of patients drink a great deal more than the rest, which leaves the
## distribution of weekly units with a long tail to the right. Alcohol is recorded as a
## count of units, so the square root is the usual transformation for it, and it spreads
## the values out much more evenly.
## ````
## 
## Note the two things this paragraph does that the rest of the report does not. It says what
## was done, and it says why, which is what makes the difference between a set of results and
## a piece of work somebody else can rely on.

## ````md
## ```{r chol-plot, fig.cap = 'HDL cholesterol by smoking status.', fig.width = 4}
## boxplot(hdlchol ~ Fsmoking, data = cardiac,
##         xlab = "Smoking status", ylab = "HDL cholesterol (mmol/l)")
## ```
## ````

## The setup chunk, which sets the defaults for every chunk in the document:
## 
## ````md
## ```{r setup, include = FALSE}
## knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
## ```
## ````
## 
## And one chunk overriding that default:
## 
## ````md
## ```{r structure, echo = TRUE}
## str(cardiac)
## ```
## ````
## 
## Note that the setup chunk itself uses `include = FALSE`, which is stronger than
## `echo = FALSE`: it hides the code *and* anything the chunk produces, so the chunk runs
## completely invisibly.
## 
## A word of caution about `warning = FALSE`. You are hiding warnings from your reader, not
## from yourself. Keep an eye on the ones that appear in the console while you work, because
## some of them matter.

## ````md
## The study included `r nrow(cardiac)` patients, with a mean age of
## `r round(mean(cardiac$age), 1)` years.
## ````
## 
## Which knits to:
## 
## > The study included 163 patients, with a mean age of 65 years.
## 
## The validation paragraph from Q6 does the same thing, using the `n_hdl` and `n_trig`
## objects the validation chunk made:
## 
## ````md
## `r n_hdl` patients had an HDL cholesterol of 0 mmol/l and `r n_trig` had a
## triglyceride of 0 mmol/l.
## ````
## 
## Notice why the chunk counts those *before* it sets anything to `NA`. It is tempting to
## write `sum(is.na(cardiac$hdlchol))` in your sentence afterwards and be done with it, but
## that returns 4 rather than 2: `hdlchol` already had two genuinely missing values in the
## file before you touched it. If you want to report what you changed, you have to count it
## before you change it.

## ````md
## ```{r summary-table}
## smoke_summary <- aggregate(cardiac[, c("age", "systolic", "tchol")],
##                            by = list(Smoking = cardiac$Fsmoking),
##                            mean, na.rm = TRUE)
## 
## knitr::kable(smoke_summary, digits = 1,
##              caption = "Mean age, systolic blood pressure and total cholesterol by smoking status.")
## ```
## ````

## Typed straight into the text:
## 
## ````md
## ![Alcohol-related hospital admissions by council area.](output/ex6_admissions.png)
## ````
## 
## There is also an R way, inside a chunk, which gives you control over the size with
## `out.width`. You do not need it today, but it is there when you do:
## 
## ````md
## ```{r admissions-figure, out.width = "80%", fig.cap = 'Alcohol-related hospital admissions by council area.'}
## knitr::include_graphics("output/ex6_admissions.png")
## ```
## ````
