# Generates data/cardiac_admissions.txt for Exercise 4.
#
# THE ADMISSIONS DATA ARE SIMULATED. The cardiac cohort itself is real, but no
# hospital episode data came with it, so this file is invented to give Exercise 4
# a linkage worth doing. Re-running this script with the seed below reproduces
# the published file exactly.
#
# What is built in, and why:
#   - one row per admission, so a patient may have none, one or several. This is
#     the one-to-many join the follow-up file cannot demonstrate.
#   - the number of admissions rises mildly with age and with smoking, so the
#     closing question of the exercise has a real answer rather than noise. The
#     effect sizes are deliberately modest and are NOT a finding about the real
#     cohort.
#   - about a quarter of patients have no admissions at all, so a left join
#     produces NAs that mean zero rather than unknown.
#   - the file is deliberately minimal: one row per patient and a single count
#     column. Bed days and out-of-cohort records were both cut because each was a
#     second lesson competing with the one the question is for.

set.seed(20260904)

cardiac <- read.table("_data_raw/cardiacdata_published.txt", header = TRUE,
                      sep = "\t", stringsAsFactors = FALSE)

# expected number of admissions per patient over the ten year follow up
smoke_effect <- c("1" = 0.35, "2" = 0.20, "3" = 0.00)
b_smoke <- unname(smoke_effect[as.character(cardiac$smoking)])
b_smoke[is.na(b_smoke)] <- 0.18          # the 7 with no smoking status recorded

lambda <- exp(-0.10 + 0.25 * (cardiac$age - 65) / 10 + b_smoke)
n_adm  <- rpois(nrow(cardiac), lambda)

adm <- data.frame(
  patno = rep(cardiac$patno, n_adm),
  stringsAsFactors = FALSE
)

n <- nrow(adm)
adm$admission_year <- sample(2001:2010, n, replace = TRUE)
adm$specialty <- sample(c("Cardiology", "General medicine", "Surgery",
                          "Respiratory", "Other"),
                        n, replace = TRUE, prob = c(0.30, 0.28, 0.20, 0.12, 0.10))
adm$los <- pmax(1, round(exp(rnorm(n, mean = 1.1, sd = 0.9))))

# NOTE: records for patients outside the cohort used to be added here. They were
# removed on 4 September 2026: checking the key in both directions is a second
# lesson, and Exercise 4 Q8 now teaches one.

# Summarise to ONE ROW PER PATIENT before publishing. A per-admission file would
# force students to aggregate before linking, and to use %in% to check the key,
# which is several new functions in service of one idea. The published file is a
# summarised extract of the kind a data linkage team would actually hand you.
out <- aggregate(cbind(n_admissions = los) ~ patno, data = adm, FUN = length)
out <- out[order(out$patno), ]

write.table(out, "data/cardiac_admissions.txt", sep = "\t",
            row.names = FALSE, quote = FALSE)

cat("rows in published file:", nrow(out), "\n")
cat("cohort patients absent from it:", nrow(cardiac) - nrow(out), "\n")
