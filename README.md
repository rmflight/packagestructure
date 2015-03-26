# packagestructure

Create a graph of the structure of an `R` package.

Currently only supports S4 classes and slots.

## Installation

```
devtools::install_github("rmflight/packagestructure")
```

## Example


```r
library(packagestructure)
example_package <- system.file("exampledata", package = "packagestructure")
e_g <- class_graph(example_package)
plot(e_g)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

In this example, classes *class_B*, *class_C* directly inherit from *class_A*,
while *class_D* inherits from *class_B*. Each of these has particular slots
of particular classes. You can see the set of class definitions [here](https://github.com/rmflight/packagestructure/blob/master/inst/exampledata/R/s4_slotandclass.R).

## TODO

* improve test coverage (classunion and no-classes)
* add travis-ci
* allow specification of classes to show and classes to exclude
* add *methods*?
* add *function call* dependency?
