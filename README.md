Work in progress version of code to simulate an age-structured, multi-stock fish population complex that experiences a mixed stock fishery with varying degrees of management control. This code is part of a set of analyses that are used to to explore how different aspects of diversity (e.g., number of populations, asynchrony among populations, differences in productivity among populations) and fishery management control (e.g., the ability of the managers to control and allocate harvest) influence the ability of fisheries to be 1) productive (maximize yield from the system), 2) sustainable (avoid accidental overharvest and extirpation of populations), and 3) stable over time. 

## Files
- `make.R`: All code required to reproduce current set of simulations and figures. Individual figures (named  `Figure_...`) are sourced from this code.

- `load.R`: Loads packages and scripts necessary for analysis. This file should be
  sourced prior to running other scripts.

- `functions.R`: All functions written for the analysis should be placed in this
  file. `functions.R` should be sourced by `load.R`. The one exception is the harvest control rule function  (`MSY_hcr_function.R`) which needs to be called multiple times during simulations (it is clunky but good enough for government work, at least for now).



