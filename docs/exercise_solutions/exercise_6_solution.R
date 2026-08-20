## ----Q2, echo=SOLUTIONS-------------------------------------------------------
scotpho <- read.table('data/scotpho_alcohol_admissions.txt', header = TRUE, sep = "\t",
                      na.strings = "NA", stringsAsFactors = TRUE)


## ----Q4, echo=SOLUTIONS-------------------------------------------------------
str(scotpho)
summary(scotpho)

nrow(scotpho)            # 330

table(scotpho$area_type)
# Council area     Scotland 
#          320           10 

# 33 areas, 10 years each. Every area is complete:
table(scotpho$area_name)
unique(table(scotpho$area_name))   # 10 - so every area has all 10 years

range(scotpho$year)      # 2010 2019

# If an area were missing a year, lines() would simply join the points either
# side of the gap and draw a straight line straight through it. Nothing would
# warn you, and the plot would imply data you do not have.


## ----Q5, echo=SOLUTIONS-------------------------------------------------------
scot <- scotpho[scotpho$area_name == "Scotland", ]

plot(scot$year, scot$measure, type = "b")

# What is wrong with this plot for anyone other than you?
#  - the x axis is labelled scot$year and the y axis scot$measure
#  - there are no units anywhere, so 673 could be anything
#  - there is no title, so the reader does not know what is being counted
#  - the y axis does not start at zero, which exaggerates the decline
#  - there is nothing to say where the data came from
#
# The plot is not wrong. It is just useless to anybody who is not already
# holding the dataset.


## ----Q6, echo=SOLUTIONS, tidy = TRUE------------------------------------------
areas <- c("Scotland", "Glasgow City", "Aberdeen City", "Aberdeenshire")
sub <- scotpho[scotpho$area_name %in% areas, ]

# the factor still thinks it has 33 levels, even though only 4 are left
levels(sub$area_name)          # all 33
sub$area_name <- droplevels(sub$area_name)
levels(sub$area_name)          # now 4

# set up an empty plot with limits that fit every area
plot(range(sub$year), range(sub$measure), type = "n",
     xlab = "Year", ylab = "Admissions per 100,000")

cols <- c("black", "red", "blue", "darkgreen")

for (i in seq_along(areas)) {
  a <- sub[sub$area_name == areas[i], ]
  a <- a[order(a$year), ]        # never assume the rows are in order
  lines(a$year, a$measure, col = cols[i], lwd = 2)
}

legend("topright", legend = areas, col = cols, lwd = 2)

# Glasgow City is roughly twice the Scottish average and Aberdeenshire is
# roughly half of it. The national line, on its own, describes almost nobody.


## ----Q7, echo=SOLUTIONS, tidy = TRUE------------------------------------------
cols <- palette.colors(4, palette = "Okabe-Ito")
cols
# "#000000" "#E69F00" "#56B4E9" "#009E73"

# a greyscale test: convert each colour to the grey of the same luminance
grey.cols <- grey(apply(col2rgb(cols), 2, function(x) sum(x * c(0.299, 0.587, 0.114)) / 255))

# draw both versions side by side and compare
par(mfrow = c(1, 2))
for (palette.in.use in list(cols, grey.cols)) {
  plot(range(sub$year), range(sub$measure), type = "n",
       xlab = "Year", ylab = "Admissions per 100,000")
  for (i in seq_along(areas)) {
    a <- sub[sub$area_name == areas[i], ]
    a <- a[order(a$year), ]
    lines(a$year, a$measure, col = palette.in.use[i], lwd = 2)
  }
  legend("topright", legend = areas, col = palette.in.use, lwd = 2)
}

# In grey, two of the four lines become hard to separate. Colour alone is never
# enough. The fix is to give the reader a second, redundant cue - here, a
# different line type for each area - so the plot still works with no colour
# at all.
par(mfrow = c(1, 1))
ltys <- c(1, 2, 3, 4)
plot(range(sub$year), range(sub$measure), type = "n",
     xlab = "Year", ylab = "Admissions per 100,000")
for (i in seq_along(areas)) {
  a <- sub[sub$area_name == areas[i], ]
  a <- a[order(a$year), ]
  lines(a$year, a$measure, col = cols[i], lwd = 2, lty = ltys[i])
}
legend("topright", legend = areas, col = cols, lwd = 2, lty = ltys)


## ----Q8, echo=SOLUTIONS, tidy = TRUE------------------------------------------
plot(range(sub$year), c(0, max(sub$measure)), type = "n",
     xlab = "Year",
     ylab = "Hospital admissions per 100,000 people",
     main = "Alcohol-related hospital admissions, 2010 to 2019",
     las = 1)

for (i in seq_along(areas)) {
  a <- sub[sub$area_name == areas[i], ]
  a <- a[order(a$year), ]
  lines(a$year, a$measure, col = cols[i], lwd = 2, lty = ltys[i])
}

legend("bottomleft", legend = areas, col = cols, lwd = 2, lty = ltys, bty = "n")

mtext("Source: Scottish Public Health Observatory. Rates are age-sex standardised.",
      side = 1, line = 4, cex = 0.7, adj = 0)

# Should the y axis start at zero?
# Starting at zero, as here, shows the true relative size of the difference
# between areas: Glasgow really is about four times Aberdeenshire. Starting at
# the minimum instead would fill the panel with the year to year wiggles and
# make a modest national decline look dramatic. For a rate like this, where
# zero is meaningful and the comparison between areas is the point, starting at
# zero is the honest choice. For something like average age, where zero is
# nowhere near the data, it would be absurd. Decide, and be able to say why.


## ----Q9, echo=SOLUTIONS, tidy = TRUE------------------------------------------
# a small function so you don't have to repeat the plotting code
plot.admissions <- function() {
  plot(range(sub$year), c(0, max(sub$measure)), type = "n",
       xlab = "Year", ylab = "Hospital admissions per 100,000 people",
       main = "Alcohol-related hospital admissions, 2010 to 2019", las = 1)
  for (i in seq_along(areas)) {
    a <- sub[sub$area_name == areas[i], ]
    a <- a[order(a$year), ]
    lines(a$year, a$measure, col = cols[i], lwd = 2, lty = ltys[i])
  }
  legend("bottomleft", legend = areas, col = cols, lwd = 2, lty = ltys, bty = "n")
  mtext("Source: Scottish Public Health Observatory. Rates are age-sex standardised.",
        side = 1, line = 4, cex = 0.7, adj = 0)
}

# pdf: vector, so it stays sharp at any size. width and height are in inches,
# and 10 x 5.625 is 16:9
pdf('output/ex6_admissions.pdf', width = 10, height = 5.625, pointsize = 12)
plot.admissions()
dev.off()

# png: pixels, so the resolution matters. units = "in" plus res = 300 gives you
# a 3000 x 1688 pixel image that will print cleanly
png('output/ex6_admissions.png', width = 10, height = 5.625, units = "in",
    res = 300, pointsize = 12)
plot.admissions()
dev.off()

# Use the pdf for anything going to a printer, and the png for anything going
# into PowerPoint or a web page. If your poster template asks for a specific
# figure size, set width and height to that size here rather than resizing the
# image afterwards, which is what makes text look squashed or fuzzy.

