Data and code to reproduce analyses and figures in:
>Conservation risks and portfolio effects in mixed-stock fisheries. In preparation. Moore J.W, B. Connors, and E. Hodgson. 

This projec consists of two main components:
1. A simulation model for an age-structured, multi-stock fish population complex that is used to explore how different aspects of diversity (e.g., number and eveness of populations, asynchrony among populations, differences in productivity among populations) and fishery management control (e.g., the ability of the managers to control and allocate harvest) influence the fishery outcomes.
2. Bayesian spawner-recruitment models fit to data from three large mixed-stock sockeye salmon fisheries to charactize change in among population status over time.

## Files
- `load.R`: Loads packages and scripts necessary for analysis. This file should be
  sourced prior to running other scripts.

- `functions.R`: All functions written for the analysis should be placed in this
  file. `functions.R` should be sourced by `load.R`. The one exception is the harvest control rule function  (`MSY_hcr_function.R`) which needs to be called multiple times during simulations (it is clunky but good enough for government work, at least for now).

  



