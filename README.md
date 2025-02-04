<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/dkgaraujo/brazilianbanks/branch/main/graph/badge.svg)](https://app.codecov.io/gh/dkgaraujo/brazilianbanks?branch=main)
<!-- badges: end -->

`brazilianbanks` is an R package to download publicly-available data on Brazilian banks and other regulated financial institutions. This is a rich panel dataset that includes information ranging from total assets to number of physical branches to the growth in credit to specific economic sectors according to a maturity bucket. 

The package should be functional but has not been officially released yet. Its alpha version will be released once the appropriate tests are included and the documentation is written more extensively. The plan is to eventually submit the package to CRAN.

## Source of the data
The data is obtained from the [Central Bank of Brazil's "IF.Data"](https://www3.bcb.gov.br/ifdata) page. The value added compared to accessing data from the IF.Data website is that `brazilianbanks` enables you to download the data across multiple periods as a panel dataset, and in a tidy format that you can readily use in your data science or econometric pipeline.

## Install
In your R console, type:
```
devtools::install_github("dkgaraujo/brazilianbanks")
```

## Usage

### Bank-level information

The `brazilianbanks` package already includes off-the-shelf two datasets of banks and other financial institutions' accounting, risk and other information. One such dataset includes all information from 2017 Q1 up to the most recent quarter: `brazilian_banks_201703_onwards`. A shorter version of this dataset covers the period of 2017 Q1 to 2021 Q4: `brazilian_banks_201703_202112`. 

```
library(brazilianbanks)

brazilian_banks_201703_onwards
```

Using these pre-loaded datasets is usually a quicker alternative to `get_bank_stats()`, the main function to retrieve data in a more customised way.

The function `all_available_quarters()` returns a vector with all the quarters for which there is available data. I suggest you run this function first, in particular to check what the most recent quarter is.

If you are interested in bank-level variables (eg, accounting or credit risk figures), the main function is `get_bank_stats()`. It returns a tibble data frame with the panel data:

```
library(brazilianbanks)

quarters <- all_available_quarters()
bank_df <- get_bank_stats(yyyymm_start = 201903, yyyymm_end = max(quarters))
```

The initial and final quarters (arguments `yyyymm_start` and `yyyymm_end` respectively) are defined by the user according to the YYYYMM format, ie March 2014 is 201403. Note that the code chunk above uses the most recent available quarter as the final quarter for data download.

### Banking statistics at the municipality level

If your focus is on municipality-level banking statistics, the main function is `get_municipal_stats()`. It returns a tibble data frame with bank balance sheet and financial statement variables at the municipal level, at a monthly frequency:

```
library(brazilianbanks)

municipal_banking_data <- get_municipal_stats(yyyymm_start = 201111, yyyymm_end = 201808)
```

## Definitions
The result of `get_bank_stats()` represents only data at the consolidated levels, equivalent to "bank holding company" level in other jurisdictions. Brazilian regulation differentiates between "prudential" and "financial" conglomerate consolidation perspectives; both are considered to be the same bank.

## Speed
Note that the first download of the data from the BCB servers may be slow (due to server-side speed limits I believe). To mitigate that, you are encouraged to always use `cache_json = TRUE`, as set by default. This will ensure your downloaded files are stored locally, saving you time in future uses. Also, processing the information from the original JSON files is not optimised for speed. To bypass these speed issues, released versions will put processed files available as part of the package, which should be good enough for users that do not mind not using the data directly from the BCB source.

## Comments, bugs, issues, feature requests and suggestions for improvement
Please write them [here](https://github.com/dkgaraujo/brazilianbanks/issues). Pull requests are welcome after launch of the alpha version and its posterior versions (before that, unit tests will not be available to check PRs for consistency). Contributors are encouraged but not required to describe the issue first in this previous link.

Thank you very much for your interest!
