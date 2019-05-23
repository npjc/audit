Choose which method(s) to use for estimation of growth curve parameters.

Derived values: estimating the maximum growth rate (mu), the length of lag phase (lambda), the area under the curve (integral) and asymptote or maximum value reached (A).

Interactively, AUDIT supports fitting a smooth spline, gompertz, logisitc, and/or richards to the data from each well without any user input. Each fitted curve is generated via self-starting nonlinear (weighted) least-squares estimation provided via the [growr][] package. 

Many more model methods are available in the package for which self-starting methods are not available. See `?stats::selfStart`, `?stats::nls` in R for further information on how to create your own self start methods.

[growr]: https://github.com/npjc/growr