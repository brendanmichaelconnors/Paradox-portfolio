[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4724357.svg)](https://doi.org/10.5281/zenodo.4724357)

Data and code to reproduce analyses and figures in:
>Moore J.W, B. M. Connors, and E. E. Hodgson. 2021. Conservation risks and portfolio effects in mixed-stock fisheries. Fish and Fisheries. DOI: 10.1111/faf.12567

This project has two main components:
1. A simulation model for an age-structured, multi-stock fish population complex that is used to explore how different aspects of diversity (e.g., number and evenness of populations, asynchrony among populations, differences in productivity among populations) and fishery management control (e.g., the ability of the managers to control and allocate harvest) influence fishery outcomes.
2. Bayesian spawner-recruitment models fit to data from three large mixed-stock sockeye salmon fisheries to characterize population status (e.g., B/Bmsy, F/Fmsy) over time.

## Files
- `load.R`: Loads packages and functions necessary for analysis. This file should be sourced prior to running other scripts.
- `simulations.R`: Runs all mixed-stock fishery simulations and saves output as RDS files in the `output` sub-folder.
- `kobe_simulations.R`: Runs illustrative mixed-stock fishery simulations across 2 combinations of management control and over-fishing risk tolerance for Kobe plots. This is sourced when Figure 3 is generated in `figures.R`. 
- `figures.R`: Generates all figures based on the mixed-stock fishery simulations and saves them in the `figures` sub-folder. 
- `case_studies`: Folder with all code to run and visualize Bayesian spawner-recruitment analysis of case study systems to characterize population status over time. Figure 5 is partially generated with code in this folder but the final version with a map inset was generated outside of this repository.
- `functions.R`: All required functions; sourced by `load.R`. The one exception is the harvest control rule function  (`MSY_hcr_function.R`) which needs to be called multiple times during simulations.





