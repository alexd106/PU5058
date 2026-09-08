## ````{.md .foldable}
## # Introduction
## 
## ## Data description
## 
## These data come from a **cohort study** of the risk factors for cardiovascular disease.
## A group of adults, aged between 55 and 75, were examined at the start of the study and a
## range of *measurements* was taken on each of them. Each row of the dataset is one patient,
## identified by a unique patient number (`patno`), and each column is one of those
## measurements.
## 
## The four measurements used in this report are:
## 
## - age, in years
## - systolic blood pressure, in mmHg
## - HDL cholesterol, in mmol/l
## - smoking status
## ````

## The setup chunk, with the package loaded at the top of the document:
## 
## ````{.md .foldable}
## ```{r setup, include = FALSE}
## knitr::opts_chunk$set(echo = TRUE)
## 
## library(knitr)
## library(ggplot2)
## ```
## ````
## 
## And the import chunk:
## 
## ````{.md .foldable}
## ```{r import}
## cardiac <- read.table('data/cardiacdata.txt', header = TRUE, sep = "\t", stringsAsFactors = TRUE)
## 
## cardiac$Fsex <- factor(cardiac$sex, levels = c(1, 2),
##                        labels = c("Female", "Male"))
## 
## cardiac$Fsmoking <- factor(cardiac$smoking, levels = c(1, 2, 3),
##                            labels = c("Current", "Ex", "Never"))
## ```
## ````

## ````{.md .foldable}
## ## Data validation
## 
## The dataset contains 163 patients.
## 
## ```{r validation}
## cardiac$bmi[cardiac$bmi > 100] <- NA
## cardiac$triglyceride[cardiac$triglyceride == 0] <- NA
## cardiac$hdlchol[cardiac$hdlchol == 0] <- NA
## ```
## 
## Three variables contained values that are not biologically possible. Two patients had an
## HDL cholesterol of 0 mmol/l and one had a triglyceride of 0 mmol/l, and nobody can have no
## fat at all in their blood. One patient had a body mass index of 514.6, which is almost
## certainly 51.46 with the decimal point in the wrong place. All four values were set to
## `NA` rather than deleted or corrected. Deleting the patients would throw away their other
## measurements, which are perfectly good, and correcting the body mass index would mean
## putting a number into the data that nobody measured.
## 
## ```{r alcohol-transform, fig.show = 'hold', out.width = '50%'}
## cardiac$alcohol_sqrt <- sqrt(cardiac$alcohol)
## 
## ggplot(cardiac, aes(x = alcohol)) +
##   geom_histogram(bins = 15) +
##   labs(x = "Alcohol (units per week)", y = "Number of patients") +
##   theme_minimal()
## 
## ggplot(cardiac, aes(x = alcohol_sqrt)) +
##   geom_histogram(bins = 15) +
##   labs(x = "Square root of alcohol units", y = "Number of patients") +
##   theme_minimal()
## ```
## 
## A small number of patients drink a great deal more than the rest, which leaves weekly
## alcohol units with a long tail to the right. Alcohol is recorded as a count of units, so
## the square root is the usual transformation for it, and it spreads the values out much
## more evenly.
## ````
## 
## Note the two things these paragraphs do that a list of results would not. They say what was
## done, and they say why, which is the difference between a set of numbers and a piece of
## work somebody else can rely on.

## ````{.md .foldable}
## ```{r chol-plot, fig.cap = 'HDL cholesterol by smoking status.', fig.width = 4}
## ggplot(cardiac, aes(x = Fsmoking, y = hdlchol)) +
##   geom_boxplot() +
##   scale_x_discrete(na.translate = FALSE) +
##   labs(x = "Smoking status", y = "HDL cholesterol (mmol/l)") +
##   theme_minimal()
## ```
## ````
## 
## And the first line of the `alcohol-transform` chunk from Q6, with its caption added:
## 
## ````{.md .foldable}
## ```{r alcohol-transform, fig.cap = 'Weekly alcohol consumption before and after square root transformation.'}
## ```
## ````

## The first version, which prints the raw console output:
## 
## ````{.md .foldable}
## ```{r summary-table}
## smoke_summary <- aggregate(cardiac[, c("age", "systolic", "tchol")],
##                            by = list(Smoking = cardiac$Fsmoking),
##                            mean, na.rm = TRUE)
## 
## smoke_summary
## ```
## ````
## 
## And the finished version:
## 
## ````{.md .foldable}
## ```{r summary-table}
## smoke_summary <- aggregate(cardiac[, c("age", "systolic", "tchol")],
##                            by = list(Smoking = cardiac$Fsmoking),
##                            mean, na.rm = TRUE)
## 
## kable(smoke_summary, digits = 1,
##       caption = "Mean age, systolic blood pressure and total cholesterol by smoking status.")
## ```
## ````

## ````{.md .foldable}
## The dataset contains `r nrow(cardiac)` patients, with a mean age of
## `r round(mean(cardiac$age), 1)` years. The youngest patient was
## `r round(min(cardiac$age))` and the oldest was `r round(max(cardiac$age))`.
## ````
## 
## Which knits to:
## 
## > The dataset contains 163 patients, with a mean age of 65 years. The youngest patient
## > was 55 and the oldest was 75.
## 
## Anything you can calculate in a chunk you can put in a sentence this way, so it's worth
## using for any number in your writing that comes out of your data.

## Hiding the code for a single chunk:
## 
## ````{.md .foldable}
## ```{r chol-plot, fig.cap = 'HDL cholesterol by smoking status.', echo = FALSE}
## ```
## ````
## 
## And setting a default for every chunk in the document, in the setup chunk at the top:
## 
## ````{.md .foldable}
## ```{r setup, include = FALSE}
## knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
## ```
## ````
## 
## Note that the setup chunk itself uses `include = FALSE`, which is stronger than
## `echo = FALSE`. It hides the code *and* anything the chunk produces, so the chunk runs
## completely invisibly.
## 
## You may also be wondering about the `knitr::` that RStudio put in front of
## `opts_chunk$set()` when it made the document. That is just another way of reaching a
## single function without loading the whole package first, and you can read it as 'the
## `opts_chunk` from `knitr`'. Now that you have `library(knitr)` in the same chunk you could
## drop it, but there is no need to.

## Typed straight into your text:
## 
## ````{.md .foldable}
## ![Alcohol-related hospital admissions by council area.](output/ex6_admissions.png)
## ````
## 
## And the same figure in a chunk, with a caption and set to the full width of the page:
## 
## ````{.md .foldable}
## ```{r admissions-figure, out.width = "100%", fig.cap = 'Alcohol-related hospital admissions by council area.'}
## include_graphics("output/ex6_admissions.png")
## ```
## ````
## 
## This one is a wide figure, so it's worth giving it the whole width of the page. Something narrower, a portrait shaped figure especially, usually sits better at 60 or 70%.
## 
## With a few sentences to go under it:
## 
## ````{.md .foldable}
## Alcohol-related hospital admissions vary enormously between council areas. Glasgow City
## has consistently the highest rate in Scotland over this period and Aberdeenshire one of
## the lowest, with the national average falling somewhere between the two. The figure was
## produced from the ScotPHO data in Exercise 6 and exported as a png at 300 dpi.
## ````

## ### The finished report
## 
## \
## 
## Here is the whole thing in one piece: **[cardiac_report.Rmd](exercise_solutions/cardiac_report.Rmd)**, the document this exercise builds, with every question's answer already in it. Right click and choose 'save link as' if it opens in your browser rather than downloading.
## 
## It needs `data/cardiacdata.txt` and `output/ex6_admissions.png` alongside it in a Project, exactly as the exercise describes, and then it will knit to PDF as it stands. Use it to check your own against, or keep it as a template to start your next report from.
