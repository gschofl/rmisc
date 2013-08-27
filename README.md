# rmisc

Various utility functions that I reuse in different packages or on which I rely when using R interactively.

The easiest way to install `rmisc` is with devtools:

```R
library(devtools)
install_github("rmisc", "gschofl")
```

Note that this packages makes use of the C++11 standard. For it to compile `Sys.setenv("PKG_CXXFLAGS"="-std=c++11")` must be set before attempting instalation.


## Tools

Available function include:

* Tests and assertions: `is.empty()`, `are_empty()`, `all_empty()`
* Various string utilities: `wrap()`, `trim()`, `dup()`, `pad()`, `split_path()`
* `strsplitN()`: Extract the Nth substring from a vector of strings.
* `strip_ext()` and `replace_ext()` to handle file extensions.
* Partial function evaluation and function composition: `Partial()`, `Compose()`
* 'default' operators: `%||%` and `%|%` (vectorised verison)
* `linebreak()`: format paragraphs.
* Apply XPath expressions to XML documents: `xvalue()`, `xname()`, `xattr()`, `xset()`
* Customized package installation: `install_packages()`, `update_packages()`
* Some convenience functions: `require.all()`, `load.all()`, `open_Rproj()`


