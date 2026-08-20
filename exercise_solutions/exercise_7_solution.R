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
## cardiac <- read.table('data/cardiacdata.txt', header = TRUE, sep = "\t",
##                       na.strings = "NA", stringsAsFactors = TRUE)
## ```
## 
## ```{r structure}
## str(cardiac)
## ```
## ````

## ````md
## ```{r chol-plot, fig.cap = 'Total cholesterol by smoking status.', fig.width = 4}
## cardiac$Fsmoking <- factor(cardiac$smoking, levels = c(1, 2, 3),
##                            labels = c("Current", "Ex", "Never"))
## 
## boxplot(tchol ~ Fsmoking, data = cardiac,
##         xlab = "Smoking status", ylab = "Total cholesterol (mmol/l)")
## ```
## ````

## The setup chunk, which sets the default for every chunk in the document:
## 
## ````md
## ```{r setup, include = FALSE}
## knitr::opts_chunk$set(echo = FALSE)
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

## ````md
## The study included `r nrow(cardiac)` patients, with a mean age of
## `r round(mean(cardiac$age), 1)` years.
## ````
## 
## Which knits to:
## 
## > The study included 163 patients, with a mean age of 65 years.

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
