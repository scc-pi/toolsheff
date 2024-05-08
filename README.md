# toolsheff

Collection of general use functions.

## Use

To use the functions add the following at the top of your R script (or
in your `setup` R Markdown or Quarto chunk).

``` r
source("https://raw.githubusercontent.com/scc-pi/toolsheff/main/R/_scc-tools.R")
```

## Database connections

Even with hidden authorisation credentials we don’t want to store
database addresses and port numbers. So, this is something that’ll have
to wait until we have secure private repos available via Azure DevOps.

## Plans

Data analysis at Sheffield Council is set to switch its default Git
repository hosting from GitHub to Azure DevOps. When it does, we’ll look
at creating R packages for internal use via Continuous Integration.
Until then this is a workaround.

## `{box}` R package

We’ve briefly tried the `{box}` R package to support modular code:
[klmr.me/box](https://klmr.me/box/). We may review our use of it after
we’ve established the convenience (or otherwise) of internal packages
via DevOps Continuous Integration.
