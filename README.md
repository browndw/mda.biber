# mda.biber

[![R-CMD-check](https://github.com/browndw/mda.biber/workflows/R-CMD-check/badge.svg)](https://github.com/browndw/mda.biber/actions)
[![Tests](https://github.com/browndw/mda.biber/workflows/Tests/badge.svg)](https://github.com/browndw/mda.biber/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/mda.biber)](https://CRAN.R-project.org/package=mda.biber)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/mda.biber)](https://cran.r-project.org/package=mda.biber)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

An R package for conducting Multi-Dimensional Analysis (MDA), a statistical procedure developed by [Douglas Biber](https://link.springer.com/article/10.1007/BF00136979) for analyzing linguistic variation across genres, registers, and text types.

## About Multi-Dimensional Analysis

Multi-Dimensional Analysis (MDA) is a specialized application of factor analysis that identifies co-occurring patterns in linguistic features. It is based on the principle that some linguistic variables co-occur (e.g., nouns and adjectives) while others inversely co-occur (e.g., nouns and pronouns).

MDA typically involves:

1. **Feature extraction**: Measuring syntactic categories (passive constructions), lexical classes (nouns), or functional categories (hedges)
2. **Factor analysis**: Identifying underlying dimensions of linguistic variation
3. **Dimension scoring**: Computing scores for texts along each dimension
4. **Interpretation**: Understanding what linguistic patterns each dimension represents

The linguistic features used in this package are based on Biber's original research, documented in the [**corpora**](https://www.rdocumentation.org/packages/corpora/versions/0.5/topics/BNCbiber) and [**pseudobibeR**](https://github.com/browndw/pseudobibeR) packages.

## Key Features

- **Core MDA function**: `mda_loadings()` performs factor analysis and calculates dimension scores
- **Visualization tools**: Multiple plotting functions for different aspects of MDA results
- **Built-in dataset**: MICUSP corpus data tagged with 67 linguistic features
- **Flexible parameters**: Customizable correlation thresholds and factor loadings
- **Comprehensive output**: Dimension scores, group means, and factor loadings

## Installation

Install the development version from GitHub:

```r
# Install from GitHub
devtools::install_github("browndw/mda.biber")

# Load the package
library(mda.biber)
```

## Quick Start

```r
library(mda.biber)
library(dplyr)

# Load and prepare the built-in MICUSP dataset
data(micusp_biber)

# Extract discipline codes and convert to factor
d <- micusp_biber %>%
  mutate(doc_id = stringr::str_extract(doc_id, "^[A-Z]+")) %>%
  mutate(doc_id = as.factor(doc_id)) %>%
  select(where(~ any(. != 0)))  # Remove zero-only columns

# Determine optimal number of factors
screeplot_mda(d)

# Perform MDA with 2 factors
mda_result <- mda_loadings(d, n_factors = 2)

# View dimension scores
head(mda_result)

# Access group means and factor loadings
attributes(mda_result)$group_means
attributes(mda_result)$loadings
```

## Visualization

The package provides several visualization functions:

### Stick Plot

```r
# Show category means along a dimension
stickplot_mda(mda_result, n_factor = 1)
```

### Heatmap

```r
# Combine stick plot with factor loadings heatmap
heatmap_mda(mda_result, n_factor = 1)
```

### Boxplot

```r
# Show dimension scores with contributing variables
boxplot_mda(mda_result, n_factor = 1)
```

## Dataset

The package includes `micusp_biber`, a dataset containing:

- **828 texts** from the Michigan Corpus of Upper-Level Student Papers (MICUSP)
- **67 linguistic features** tagged using the pseudobibeR package
- **Multiple disciplines** represented (Biology, Engineering, Philosophy, etc.)
- **Standardized rates** per 1,000 tokens for most features

## Functions Overview

| Function | Purpose |
|----------|---------|
| `mda_loadings()` | Core MDA analysis with factor analysis and dimension scoring |
| `screeplot_mda()` | Generate scree plots to determine optimal number of factors |
| `stickplot_mda()` | Create stick plots showing category means along dimensions |
| `heatmap_mda()` | Combine stick plots with factor loading heatmaps |
| `boxplot_mda()` | Display dimension scores with contributing variable vectors |

## Documentation

- [Package documentation](https://cmu-textstat-docs.readthedocs.io/en/latest/mda.biber/mda.biber.html)
- Full methodology description: [Multi-dimensional analysis tutorial](https://www.uni-bamberg.de/fileadmin/eng-ling/fs/Chapter_21/Index.html?21Summary.html)

## Citation

If you use this package in your research, please cite:

```bibtex
Brown, D. (2024). mda.biber: Functions for Multi-Dimensional Analysis. 
R package version 1.0.1. https://github.com/browndw/mda.biber
```

## License

This package is licensed under the MIT License.
