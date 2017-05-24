# rjdotchartr
This package contains a single function, `rjdotchart`, that produces a dotchart with right justified y-axis labels. This is a modified version of the `dotchart` function included in the `graphics` package.

You can install this package using [devtools](https://cran.r-project.org/web/packages/devtools/index.html).

```
devtools::install_github('jessicabeyer/rjdotchartr')
```

As an example, we can compare the results of dotchart and rjdotchart.
```
dotchart(VADeaths[1,], main = "Death Rates in Virginia - 1940\n Ages 50-54")
```
![dotchart example](https://github.com/jessicabeyer/rjdotchartr/blob/master/example%20plots/dotchartexample.png)
```
rjdotchart(VADeaths[1,], main = "Death Rates in Virginia - 1940\n Ages 50-54")
```
![rjdotchart example](https://github.com/jessicabeyer/rjdotchartr/blob/master/example%20plots/rjdotchartexample.png)
