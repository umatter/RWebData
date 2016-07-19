# README #


### RWebData: An R package to easily access and download data from REST APIs ###

The rise of the programmable web offers new opportunities for the empirically driven social sciences. The access, compilation and preparation of data from the programmable web for statistical analysis can, however, involve substantial up-front costs for the practical researcher. The R-package RWebData provides a high-level framework that allows data to be easily collected from the programmable web in a format that can directly be used for statistical analysis in R without bothering about the data's initial format and nesting structure. It was developed specifically for users who have no experience with web technologies and merely use R as a statistical software. The core idea and methodological contribution of the package are the disentangling of parsing web data and mapping them with a generic algorithm (independent of the initial data structure) to a flat table-like representation. This paper provides an overview of the high-level functions for R-users, explains the basic architecture of the package, and illustrates the implemented data mapping algorithm. 


* Current version: 0.1
* See the current working paper/manual on [arXiv](http://arxiv.org/abs/1603.00293) for a short introduction 

### How to get started? ###

* Install via the devtools package: install_bitbucket("ulrich-matter/rwebdata")
* Dependencies: see DESCRIPTION-File in Source-directory

### Contribution guidelines ###

* Writing tests: suggest APIs to run tests
* Code review
* Other guidelines: suggestions for extensions etc.