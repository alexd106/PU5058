# PU5058 Introduction to Health Data Science

Course website source for **PU5058 Introduction to Health Data Science**, MSc Health Data Science, University of Aberdeen.

Rendered site: https://alexd106.github.io/PU5058/

The site hosts the practical exercises, exercise solutions, data files and how-to
videos for the course. It is built with `rmarkdown::render_site()` from the `.Rmd`
sources in this repository; see `_site.yml` for the site configuration.

Rendered files are written to `docs/`. GitHub Pages should be configured to
deploy from the `master` branch and `/docs` folder.

## Prerequisites

Rendering the full site requires R plus the packages used by the website and
exercise pages:

```r
install.packages(c(
  "rmarkdown",
  "knitr",
  "dplyr",
  "stringr",
  "htmltools",
  "lattice",
  "vioplot",
  "scales"
))
```

The `exercises.Rmd` build also renders PDF versions of the exercises, so a LaTeX
installation is required. If LaTeX is not already installed, the smallest usual
setup from R is:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

The optional ggplot solution page uses additional packages:

```r
install.packages(c("ggplot2", "gridExtra", "GGally"))
```

Build the site from the repository root with:

```r
rmarkdown::render_site()
```
