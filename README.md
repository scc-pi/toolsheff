# toolsheff

Collection of general use functions.

## Use

To use the functions, add the following at the top of your R script (or
in your `setup` R Markdown or Quarto chunk):

``` r
source("https://raw.githubusercontent.com/scc-pi/toolsheff/main/R/_scc-tools.R")
```

## Development

If you’ve branched from `main` and are developing the functions, and
want to use (or test) them, you could source from your local repo, for
example:

``` r
# source("https://raw.githubusercontent.com/scc-pi/toolsheff/main/R/_scc-tools.R")
source("C:/repo/toolsheff/R/_scc-tools.R")
```

## Project specific

You may have functions that are useful across different scripts but only
within the context of a single project (and project specific repo). For
example, within a `housing` local repo:

``` r
# source("https://raw.githubusercontent.com/scc-pi/toolsheff/main/R/_scc-tools.R")
source("C:/repo/toolsheff/R/_scc-tools.R")
source(here::here("R", "_scc-tools-housing.R"))
```

## Database connections

Even with hidden authorisation credentials, we don’t want to store
database addresses and port numbers. So, this is something that will
have to wait until we have secure private repos available via Azure
DevOps.

## Plans

Data analysis at Sheffield Council is set to switch its default Git
repository hosting from GitHub to Azure DevOps. When it does, we’ll look
at creating R packages for internal use via Continuous Integration.
Until then this is a workaround.

## `{box}` R package

We’ve briefly tried the `{box}` R package to support modular code:
[klmr.me/box](https://klmr.me/box/). We may review our use of it after
we’ve established the convenience (or otherwise) of internal packages
via Continuous Integration in Azure DevOps.
