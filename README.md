## mda.biber

Multi-Dimensional Analysis (MDA) is a complex statistical procedure developed by [Douglas Biber](https://link.springer.com/article/10.1007/BF00136979). It is largely used to describe language as it varies by genre, register, and use.

MDA is a specific application of factor analysis. Thus, it rests on the co-occurence of linguistic features -- a full description of the procedure [can be found here](https://www.uni-bamberg.de/fileadmin/eng-ling/fs/Chapter_21/Index.html?21Summary.html). Typically, the features that are measured are syntactic categories (like passive contructions), lexical classes (like nouns), or functional categories (like hedges). The categories Biber used in his research are documented in the [**corpora** package](https://www.rdocumentation.org/packages/corpora/versions/0.5/topics/BNCbiber) and the [**pseudobibeR** package](https://github.com/browndw/pseudobibeR)

The **mda.biber** package contains functions for carrying out the calculations needed to describe and plot MDA results: dimension scores, dimension means, and factor loadings.

## Installing mda.biber

Use devtools to install the package.

```r
devtools::install_github("browndw/mda.biber")
```

The package documentation is available on [readthedocs](https://cmu-textstat-docs.readthedocs.io/en/latest/mda.biber/mda.biber.html).

