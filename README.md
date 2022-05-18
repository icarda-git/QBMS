# QBMS <img src='man/figures/logo.png' align="right" height="140" />
<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/QBMS)](https://cran.r-project.org/package=QBMS)
[![CRAN checks](https://cranchecks.info/badges/summary/QBMS)](https://cran.r-project.org/web/checks/check_results_QBMS.html)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/QBMS)](https://cran.r-project.org/package=QBMS)
[![CRAN license](https://img.shields.io/github/license/icarda-git/QBMS)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![GitHub commits](https://img.shields.io/github/last-commit/icarda-git/QBMS)](https://github.com/icarda-git/QBMS/commits/master)
[![GitHub issues](https://img.shields.io/github/issues-raw/icarda-git/QBMS)](https://github.com/icarda-git/QBMS/issues)
[![R badge](https://img.shields.io/badge/Build%20with-♥%20and%20R-blue)](https://github.com/icarda-git/QBMS)
<!-- badges: end -->

## Overview
Linking data management systems to analytics is an important step in breeding digitalization. Breeders can use this R package to Query the Breeding Management System(s) like [BMS](https://bmspro.io/), [BreeBase](https://breedbase.org), and [GIGWA](https://southgreen.fr/content/gigwa) (using [BrAPI](https://brapi.org/) calls) and help them to retrieve phenotypic and genotypic data directly into their analyzing pipelines developed in R statistical environment.

>___Author and Maintainer:__ [Khaled Al-Shamaa](https://github.com/khaled-alshamaa) <k.el-shamaa (at) cgiar (dot) org>_
>
>___Contributor:__ [Mariano Omar CRIMI](https://github.com/mcrimi) <m.crimi (at) cgiar (dot) org>_
>
>___Contributor:__ Zakaria Kehel <z.kehel (at) cgiar (dot) org>_
>
>___Copyright Holder:__ [International Center for Agricultural Research in the Dry Areas (ICARDA)](https://www.icarda.org/)_

## Breeding Management System (BMS)
Breeding Management System ([BMS](https://bmspro.io/)) is an information management system developed by the Integrated Breeding Platform to help breeders manage the breeding process, from programme planning to decision-making. The BMS is customizable for most crop breeding programs, and comes pre-loaded with curated ontology terms for many crops (bean, cassava, chickpea, cowpea, groundnut, maize, rice, sorghum, soybean, wheat, and others). The BMS is available as a cloud application, which can be installed on local or remote servers and accessed by multiple users.

## BreedBase
[Breedbase](https://breedbase.org/) is a comprehensive breeding management and analysis software. It can be used to design field layouts, collect phenotypic information using tablets, support the collection of genotyping samples in a field, store large amounts of high density genotypic information, and provide Genomic Selection related analyses and predictions.

## GIGWA
[GIGWA](https://southgreen.fr/content/gigwa) is a web-based tool which provides an easy and intuitive way to explore large amounts of genotyping data by filtering the latter based not only on variant features, including functional annotations, but also on genotype patterns. The data storage relies on MongoDB, which offers good scalability perspectives. GIGWA can handle multiple databases and may be deployed in either single or multi-user mode. Finally, it provides a wide range of popular export formats.

## BrAPI
The Breeding API ([BrAPI](https://brapi.org/)) project is an effort to enable interoperability among plant breeding databases. BrAPI is a standardized RESTful web service API specification for communicating plant breeding data. This community driven standard is free to be used by anyone interested in plant breeding data management.

## Installation
```r
install.packages("QBMS")
```

### _Development version_

To get a bug fix or to use a feature from the development version, you can install the development version of QBMS from GitHub.

```r
install.packages("remotes")
remotes::install_github("icarda-git/QBMS")
```

## _Troubleshooting the installation_

1. If the installation of QBMS generates errors saying that some of the existing packages cannot be removed, you can try to quit any R session, and try to start R in administrator (Windows) or SUDO mode (Linux/Ubuntu) then try installing again.

2. If you get an error related to packages built under a current version of R, and updating your packages doesn’t help, you can consider overriding the error with the following code. _Note: This might help you install QBMS but may result in other problems. If possible, it’s best to resolve the errors rather than ignoring them._

```r
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = TRUE)

remotes::install_github("icarda-git/QBMS", upgrade = "always")
```
