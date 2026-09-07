## ----Q1, echo=TRUE------------------------------------------------------------
usethis::git_sitrep()


## ----Q4, echo=SOLUTIONS-------------------------------------------------------
cardiac <- read.table('data/cardiacdata.txt', header = TRUE, sep = "\t",
                      stringsAsFactors = TRUE)

cardiac$Fsmoking <- factor(cardiac$smoking, levels = c(1, 2, 3),
                           labels = c("Current", "Ex", "Never"))

boxplot(hdlchol ~ Fsmoking, data = cardiac,
        xlab = "Smoking status", ylab = "HDL cholesterol (mmol/l)")


## ````{.md .foldable}
## output/
## ````
