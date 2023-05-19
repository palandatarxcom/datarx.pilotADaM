
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

## Instruction

In 2007, the [original pilot project submission
package](https://bitbucket.cdisc.org/projects/CED/repos/sdtm-adam-pilot-project/browse)
was finalized and released following a review by FDA Staff, where the
CDISC data and metadata contained within the package were evaluated for
its suitability in meeting the needs and expectations of medical and
statistical reviewers. In 2019, the [PHUSE Test Data
Factory](https://advance.phuse.global/display/WEL/Test+Dataset+Factory)
took on the goal of replicating the SDTM and ADaM data within the CDISC
pilot package to match more modern data standards, bringing the ADaM
data up to version 1.1. Atorus Research had regenerated the table
outputs within the CDISC Pilot Project using the PHUSE Test Data Factory
project’s data and the R Programming language.

Our motivation behind this new project is to: - Migrate Atorus project
to a R package, convert the script to R functions. - Pilot data from
PHUSE Test Data Factory are convert and stored in this R package. -
Demonstrate that we are able to obtain matching outputs using R - Create
a Shiny app to demostrate the tables. - Demonstrate Atorus’s R package,
`pharmaRTF`, in action.(You can find our package pharmaRTF right
[here](https://github.com/atorus-research/pharmaRTF))

## The original data

To obtain the data for this repository, you can download the data from
the PHUSE Github Repository, using [this
link](https://github.com/phuse-org/phuse-scripts/blob/master/data/adam/TDF_ADaM_v1.0.zip)
for ADaM data and [this
link](https://github.com/phuse-org/phuse-scripts/blob/master/data/sdtm/TDF_SDTM_v1.0%20.zip)
for the SDTM.

## Notes from Atorus project

- The ADaMs we used to regenerate the CDISC Pilot displays were the
  PHUSE CDISC Pilot replication ADaMs following ADaM V1.1. Since the
  CDISC Pilot displays were not regenerated using the PHUSE CDISC Pilot
  replication data there are likely discrepancies between the original
  CDISC Pilot analysis data and the PHUSE CDISC Pilot replication ADaMs.
- SAS and R round differently. While SAS rounds up if the value is 5 or
  greater, R rounds to the nearest even number.
- In some circumstances, R packages will not produce a p-value if the
  the counts within the data are not high enough to make it
  statistically meaningful.

## Notes on R Packages

As many programmers in the R community do, we relied on the
[tidyverse](https://www.tidyverse.org/packages/) for much of our data
processing. There are a few addition libraries that we used worth
mentioning: - For the CMH test where testing for the alternate
hypothesis that row means differ, the package vcdExtra was used, which
is not included in the base distribution of R. We additionally had to
make a slight modification to this library, which is available in [this
fork](https://github.com/mstackhouse/vcdExtra) of the package. The
update is due to the fact that the `solve` function in R will throw an
error when processing large, sparse tables. By replacing `solve` with
`MASS::ginv`, the error is bypassed. We as an organization plan to
perform further testing of this update and submit the update back to the
original author. - For Mixed Models, the `lme4` package was used - For
ANCOVA models, `car` was used and the `emmeans` package was used to do
LSMEANS.

## Installation

You can install the development version of datarx.pilotADaM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("palandatarxcom/datarx.pilotADaM")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(datarx.pilotADaM)
## basic example code
```
