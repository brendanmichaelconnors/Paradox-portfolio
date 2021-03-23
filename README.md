Data and code to reproduce analyses and figures in:
>Conservation risks and portfolio effects in mixed-stock fisheries. In preparation. Moore J.W, B. Connors, and E. Hodgson. 

This project has two main components:
1. A simulation model for an age-structured, multi-stock fish population complex that is used to explore how different aspects of diversity (e.g., number and evenness of populations, asynchrony among populations, differences in productivity among populations) and fishery management control (e.g., the ability of the managers to control and allocate harvest) influence fishery outcomes.
2. Bayesian spawner-recruitment models fit to data from three large mixed-stock sockeye salmon fisheries to characterize population status (e.g., B/Bmsy, F/Fmsy) over time.

## Files
- `load.R`: Loads packages and functions necessary for analysis. This file should be sourced prior to running other scripts.
- `simulations.R`: Runs all mixed-stock fishery simulations and saves output as RDS files in the `output` sub-folder.
- `figures.R`: Generates all figures based on the mixed-stock fishery simulations and saves them in the `figures` sub-folder.
- `functions.R`: All required functions; sourced by `load.R`. The one exception is the harvest control rule function  (`MSY_hcr_function.R`) which needs to be called multiple times during simulations.

  



