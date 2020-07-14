
# tm.plugin.proquest

<!-- badges: start -->
<!-- badges: end -->

tm.plugin.proquest provides a `tm` Source function for creating corpora from 
ProQuest database content. Specifically, it reads emails sent from ProQuest
containing full text articles, in the form of .eml files.

## Installation

You can install the development version of tm.plugin.proquest from
[Github](https://github.com/pmyteh/tm.plugin.proquest) with:

``` r
devtools::install_github("pmyteh/tm.plugin.proquest")
```

## Example

This example shows you how to create a `tm::VCorpus` object using this package's
source function:

``` r
library(tm.plugin.proquest)
s <- ProQuestSource('input.eml')
corp <- VCorpus(s)
```

