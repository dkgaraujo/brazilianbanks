`brazilianbanks` is an R package to download publicly-available data on Brazilian banks and other regulated financial institutions. It should be functional but has not been released yet. Its alpha version will be released once the appropriate tests are included and the documentation is written.

## Source of the data
The data is obtained from the [Central Bank of Brazil's "IF.Data"](https://www3.bcb.gov.br/ifdata) page. The value added compared to accessing data from the IF.Data website is that `brazilianbanks` enables you to download the data across multiple periods as a panel dataset, and in a tidy format.

## Usage
The main function is `get_bank_stats()`. It returns a tibble data frame with the panel data.

The initial and final quarters (arguments `yyyymm_start` and `yyyymm_end` respectively) are defined by the user according to the YYYYMM format, ie March 2014 is 201403. 

## Definitions
The result of `get_bank_stats()` represents only data at the consolidated levels. Both "prudential" and "financial" conglomerate perspectives are considered to be the same bank.

## Comments, bugs, issues and suggestions
Please write them [here](https://github.com/dkgaraujo/brazilianbanks/issues).

Thank you very much for your interest!
