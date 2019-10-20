Choose which method(s) to use for estimation of growth curve parameters.



AUDIT supports fitting a smooth spline, gompertz, logisitc, and/or richards to the data from each well without any user input. Each fitted curve is generated via self-starting nonlinear (weighted) least-squares estimation provided via the [growr][] package. 

Custom models can be provided via the 'manual' estimation method. Specify your model in formula form (`y ~ x`). For example, to fit a straight line model to each well specify: `y ~ m * x + b`. Then, provide starting estimates for estimating the coefficients in a comma separated list (`m = 0.05, b = 0`). Many model methods are have been gathered within the [growr][] package.

For more information see `?stats::smooth.spline`, `?stats::nls`, `?stats::selfStart`, `?stats::formula`in R.

[growr]: https://github.com/npjc/growr
