## mda.biber

Multi-Dimensional Analysis (MDA) is a complex statistical procedure developed by [Douglas Biber](https://link.springer.com/article/10.1007/BF00136979). It is largely used to describe language as it varies by genre, register, and use.

MDA is a specific application of factor analysis. Thus, it rests on the co-occurence of linguistic features -- a full description of the procedure [can be found here](https://www.uni-bamberg.de/fileadmin/eng-ling/fs/Chapter_21/Index.html?21Summary.html). Typically, the features that are measured are syntactic categories (like passive contructions), lexical classes (like nouns), or functional categories (like hedges). The categories Biber used in his research are documented in the [**corpora** package](https://www.rdocumentation.org/packages/corpora/versions/0.5/topics/BNCbiber) and the [**pseudobibeR** package](https://github.com/browndw/pseudobibeR)

The **mda.biber** package contains functions for carrying out the calculations needed to describe and plot MDA results: dimension scores, dimension means, and factor loadings.

## Installing mda.biber

Use devtools to install the package.

```r
devtools::install_github("browndw/mda.biber")
```
## Running mda.biber

The main function, **mda_loadings( )**, requires a data.frame with 1 categorical variable (formatted as a factor) and multiple continous (numeric) variables.

The categorical variable is typically a text-type or register (like spoken, academic, news, magazine, and fiction) and the other variables are normalized feature frequencies.

The rule of thumb is that there should be 5 times as many observations (rows) than variables (columns). Therefore, the feature frequencies are generally not simple token counts, as there would likely be tens-of-thousands of columns.

The package comes with sample data tagged with **pseudobibeR**, which counts 66 features.

The **mda_loadings( )** functions requires an argument for specifying the number of factors to extract. This would be determined [just as one would for any factor analysis](https://www.statmethods.net/advstats/factor.html).

The function also takes arguments for the minimum at which a variable must correlate with another or be dropped, and the threshold factor loading (in absolute value) for calculating dimension scores. The defaults are **0.20** and **0.35** respectively.

```r
m <- mda_loadings(d, n_factors = 3,  cor_min = .20, threshold = .35)
```

The function outputs a data.frame of dimension scores for each observation (text or document) with the loadings and means accessible as attributes.

```r
attributes(m)$loadings
attributes(m)$group_means
```
For more details, consult the [vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/browndw/mda.biber/main/vignettes/introduction.html)
