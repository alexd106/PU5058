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
# one dataframe per area
scotland <- scotpho[scotpho$area_name == "Scotland", ]
glasgow <- scotpho[scotpho$area_name == "Glasgow City", ]
aberdeenshire <- scotpho[scotpho$area_name == "Aberdeenshire", ]

# Glasgow reaches about 1500, so leave room for it
plot(scotland$year, scotland$measure, type = "l", lwd = 2, col = "black",
     ylim = c(0, 1600), xlab = "Year", ylab = "Admissions per 100,000")

lines(glasgow$year, glasgow$measure, lwd = 2, col = "red")
lines(aberdeenshire$year, aberdeenshire$measure, lwd = 2, col = "blue")

legend("topright", legend = c("Scotland", "Glasgow City", "Aberdeenshire"),
       col = c("black", "red", "blue"), lwd = 2)

# Glasgow City is roughly twice the Scottish average and Aberdeenshire is
# roughly half of it. The national line, on its own, describes almost nobody.

# Note: with only three areas it is perfectly reasonable to write out three
# lines() calls. If you had all 32 council areas you would want a loop instead,
# which you can meet in the optional programming exercise.


## ----Q7, echo=SOLUTIONS, tidy = TRUE------------------------------------------
cols <- palette.colors(3, palette = "Okabe-Ito")
cols
# "#000000" "#E69F00" "#56B4E9"   black, orange, sky blue

plot(scotland$year, scotland$measure, type = "l", lwd = 2, col = cols[1],
     ylim = c(0, 1600), xlab = "Year", ylab = "Admissions per 100,000")
lines(glasgow$year, glasgow$measure, lwd = 2, col = cols[2])
lines(aberdeenshire$year, aberdeenshire$measure, lwd = 2, col = cols[3])
legend("topright", legend = c("Scotland", "Glasgow City", "Aberdeenshire"),
       col = cols, lwd = 2)

# now the same plot in greyscale
greys <- grey(c(0, 0.64, 0.62))

plot(scotland$year, scotland$measure, type = "l", lwd = 2, col = greys[1],
     ylim = c(0, 1600), xlab = "Year", ylab = "Admissions per 100,000")
lines(glasgow$year, glasgow$measure, lwd = 2, col = greys[2])
lines(aberdeenshire$year, aberdeenshire$measure, lwd = 2, col = greys[3])
legend("topright", legend = c("Scotland", "Glasgow City", "Aberdeenshire"),
       col = greys, lwd = 2)

# The orange and the sky blue are different enough on screen, but in grey they
# come out at 0.64 and 0.62 - practically the same. Anyone printing your poster
# in black and white cannot tell Glasgow from Aberdeenshire.

# Colour alone is never enough. The fix is to give the reader a second,
# redundant cue - here a different line type for each area, set with lty - so
# the plot still works with no colour at all.
plot(scotland$year, scotland$measure, type = "l", lwd = 2, col = cols[1], lty = 1,
     ylim = c(0, 1600), xlab = "Year", ylab = "Admissions per 100,000")
lines(glasgow$year, glasgow$measure, lwd = 2, col = cols[2], lty = 2)
lines(aberdeenshire$year, aberdeenshire$measure, lwd = 2, col = cols[3], lty = 3)
legend("topright", legend = c("Scotland", "Glasgow City", "Aberdeenshire"),
       col = cols, lwd = 2, lty = c(1, 2, 3))


## ----Q8, echo=SOLUTIONS, tidy = TRUE------------------------------------------
plot(scotland$year, scotland$measure, type = "l", lwd = 2, col = cols[1], lty = 1,
     ylim = c(0, 1600), las = 1,
     xlab = "Year",
     ylab = "Hospital admissions per 100,000 people",
     main = "Alcohol-related hospital admissions, 2010 to 2019")

lines(glasgow$year, glasgow$measure, lwd = 2, col = cols[2], lty = 2)
lines(aberdeenshire$year, aberdeenshire$measure, lwd = 2, col = cols[3], lty = 3)

legend("bottomleft", legend = c("Scotland", "Glasgow City", "Aberdeenshire"),
       col = cols, lwd = 2, lty = c(1, 2, 3), bty = "n")

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
# pdf: vector, so it stays sharp at any size. width and height are in inches,
# and 10 x 5.625 is 16:9
pdf('output/ex6_admissions.pdf', width = 10, height = 5.625, pointsize = 12)

plot(scotland$year, scotland$measure, type = "l", lwd = 2, col = cols[1], lty = 1,
     ylim = c(0, 1600), las = 1, xlab = "Year",
     ylab = "Hospital admissions per 100,000 people",
     main = "Alcohol-related hospital admissions, 2010 to 2019")
lines(glasgow$year, glasgow$measure, lwd = 2, col = cols[2], lty = 2)
lines(aberdeenshire$year, aberdeenshire$measure, lwd = 2, col = cols[3], lty = 3)
legend("bottomleft", legend = c("Scotland", "Glasgow City", "Aberdeenshire"),
       col = cols, lwd = 2, lty = c(1, 2, 3), bty = "n")
mtext("Source: Scottish Public Health Observatory. Rates are age-sex standardised.",
      side = 1, line = 4, cex = 0.7, adj = 0)

dev.off()

# png: pixels, so the resolution matters. units = "in" plus res = 300 gives you
# a 3000 x 1688 pixel image that will print cleanly. It is the same plotting
# code again - copy and paste it, or better, keep it in your script once and
# run it twice.
png('output/ex6_admissions.png', width = 10, height = 5.625, units = "in",
    res = 300, pointsize = 12)

plot(scotland$year, scotland$measure, type = "l", lwd = 2, col = cols[1], lty = 1,
     ylim = c(0, 1600), las = 1, xlab = "Year",
     ylab = "Hospital admissions per 100,000 people",
     main = "Alcohol-related hospital admissions, 2010 to 2019")
lines(glasgow$year, glasgow$measure, lwd = 2, col = cols[2], lty = 2)
lines(aberdeenshire$year, aberdeenshire$measure, lwd = 2, col = cols[3], lty = 3)
legend("bottomleft", legend = c("Scotland", "Glasgow City", "Aberdeenshire"),
       col = cols, lwd = 2, lty = c(1, 2, 3), bty = "n")
mtext("Source: Scottish Public Health Observatory. Rates are age-sex standardised.",
      side = 1, line = 4, cex = 0.7, adj = 0)

dev.off()

# Use the pdf for anything going to a printer, and the png for anything going
# into PowerPoint or a web page. If your poster template asks for a specific
# figure size, set width and height to that size here rather than resizing the
# image afterwards, which is what makes text look squashed or fuzzy.


## ----Q10, echo=SOLUTIONS, tidy = TRUE-----------------------------------------
library(ggplot2)

areas <- c("Scotland", "Glasgow City", "Aberdeenshire")
sub <- scotpho[scotpho$area_name %in% areas, ]

# keep only the three areas as factor levels, in the order we want them
sub$area_name <- factor(sub$area_name, levels = areas)

ggplot(data = sub, aes(x = year, y = measure,
                       colour = area_name, linetype = area_name)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = as.vector(cols)) +
  scale_y_continuous(limits = c(0, 1600)) +
  # without this ggplot labels the axis 2010.0, 2012.5, 2015.0 - decimal years,
  # which is nonsense on a plot anyone else has to read
  scale_x_continuous(breaks = seq(2010, 2018, by = 2)) +
  labs(x = "Year",
       y = "Hospital admissions per 100,000 people",
       title = "Alcohol-related hospital admissions, 2010 to 2019",
       caption = "Source: Scottish Public Health Observatory. Rates are age-sex standardised.",
       colour = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))

ggsave('output/ex6_admissions_ggplot.png', width = 10, height = 5.625, dpi = 300)

