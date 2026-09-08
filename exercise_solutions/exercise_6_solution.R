## ----Q2, echo=SOLUTIONS-------------------------------------------------------
library(ggplot2)

scotpho <- read.table('data/scotpho_alcohol_admissions.txt', header = TRUE, sep = "\t", stringsAsFactors = TRUE)


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

# Why it's worth checking: if an area were missing a year, the line would be
# drawn straight through the gap, joining the points either side of it.
# Nothing would warn you, and the plot would imply data you don't have.


## ----Q5, echo=SOLUTIONS-------------------------------------------------------
scot <- scotpho[scotpho$area_name == "Scotland", ]

ggplot(data = scot, aes(x = year, y = measure)) +
  geom_line() +
  geom_point()

# What is wrong with this plot for anyone other than you?
#  - the axes are labelled year and measure, which are column names rather
#    than English
#  - there are no units anywhere, so 673 could be anything
#  - there is no title, so the reader does not know what is being counted
#  - the x axis is broken at 2010, 2012.5, 2015 and 2017.5, and there is no
#    such year as 2012.5
#  - the y axis runs from about 665 to 764 rather than from zero, which
#    exaggerates the decline
#  - there is nothing to say where the data came from
#
# The plot is not wrong. It is just not very useful to anybody who doesn't already
# have the dataset.


## ----Q6, echo=SOLUTIONS-------------------------------------------------------
# a) the three areas in one dataframe
areas <- c("Scotland", "Glasgow City", "Aberdeenshire")

three_areas <- scotpho[scotpho$area_name %in% areas, ]

nrow(three_areas)   # 30 - three areas, ten years each

# b) one geom_line(), three lines, and a legend you didn't have to write
ggplot(data = three_areas, aes(x = year, y = measure, colour = area_name)) +
  geom_line(linewidth = 1)

# Glasgow City is roughly twice the Scottish average and Aberdeenshire is
# roughly half of it. In 2019 the rates were 1169, 673 and 314 admissions per
# 100,000. The national line, on its own, describes almost nobody.


## ----Q7, echo=SOLUTIONS-------------------------------------------------------
# a) a colour-blind friendly palette
cols <- palette.colors(3, palette = "Okabe-Ito")
cols
# "#000000" "#E69F00" "#56B4E9"   black, orange, sky blue

ggplot(data = three_areas, aes(x = year, y = measure, colour = area_name)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = cols)

# b) the same plot in greyscale
greys <- grey(c(0, 0.64, 0.62))

ggplot(data = three_areas, aes(x = year, y = measure, colour = area_name)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = greys)

# The orange and the sky blue are different enough on screen, but in grey they
# come out at 0.64 and 0.62, which is practically the same. Anyone printing
# your poster in black and white can't tell Glasgow from Scotland.

# c) a second cue, so the plot still works with no colour at all
ggplot(data = three_areas,
       aes(x = year, y = measure, colour = area_name, linetype = area_name)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = cols)

# mapping both colour and linetype to the same variable gives you a single
# legend showing both, which is what you want. Try it with greys instead of
# cols and you'll find the plot still reads perfectly well.


## ----Q8, echo=SOLUTIONS-------------------------------------------------------
ggplot(data = three_areas,
       aes(x = year, y = measure, colour = area_name, linetype = area_name)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = seq(2010, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 1600)) +
  labs(x = "Year",
       y = "Hospital admissions per 100,000 people",
       title = "Alcohol-related hospital admissions, 2010 to 2019",
       caption = "Source: Scottish Public Health Observatory. Rates are age-sex standardised.",
       colour = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))

# plot.caption = element_text(hjust = 0) pushes the source note over to the
# left. ggplot right aligns it by default, which doesn't look quite right.

# Should the y axis start at zero?
# Starting at zero, as here, shows the true relative size of the difference
# between the areas, and Glasgow really is about four times Aberdeenshire.
# Starting at the minimum instead would fill the panel with the year to year
# wiggles and make a fairly modest national decline look dramatic. For a rate
# like this, where zero is meaningful and the comparison between areas is the
# point, starting at zero is the honest choice. For something like average age,
# where zero is nowhere near the data, it wouldn't make much sense. Whichever
# you go for, just be able to say why.


## ----Q9, echo=SOLUTIONS-------------------------------------------------------
# give the plot a name. Nothing is drawn until you ask for it by name
admissions_plot <- ggplot(data = three_areas,
       aes(x = year, y = measure, colour = area_name, linetype = area_name)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = seq(2010, 2019, by = 2)) +
  scale_y_continuous(limits = c(0, 1600)) +
  labs(x = "Year",
       y = "Hospital admissions per 100,000 people",
       title = "Alcohol-related hospital admissions, 2010 to 2019",
       caption = "Source: Scottish Public Health Observatory. Rates are age-sex standardised.",
       colour = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))

admissions_plot     # draw it in the plot pane

# pdf: a vector format, so it stays sharp at any size. Sizes are in inches.
ggsave('output/ex6_admissions.pdf', plot = admissions_plot,
       width = 10, height = 5.625, units = "in")

# png: pixels, so the resolution matters. 10 x 5.625 inches at 300 dpi gives
# you a 3000 x 1687 pixel image that will print cleanly.
ggsave('output/ex6_admissions.png', plot = admissions_plot,
       width = 10, height = 5.625, units = "in", dpi = 300)

# Use the pdf for anything going to a printer and the png for anything going
# into a Word document, PowerPoint or a web page. If your poster template asks
# for a particular figure size, set width and height to that size here rather
# than resizing the image afterwards, which is what makes text look squashed
# or fuzzy.

# If you leave out plot = admissions_plot, ggsave() saves the last plot you
# drew. That's usually the one you wanted, but not always, so naming it is
# safer.

