# Analysis Showcase

This repository includes three representative code examples from my empirical
research workflow. Together, they illustrate how I integrate spatial data,
econometric modeling, and simulation-based analysis in environmental and resource
economics.

## Repository Structure

### `01_spatial_extraction.R`
Assigns gridded climate variables (temperature) to agricultural field polygons.  
For each field and year, the script computes the area-weighted mean of raster
cells intersecting each field.

### `02_baseline_model.do` (or `.R`)
Constructs baseline econometric model used for downstream
analysis. This example demonstrates data preparation, model specification, and
estimation.

### `03_simulation_pipeline.R`
Generates counterfactual predictions using the estimated model.  
This script simulates outcomes under alternative climate or policy scenarios.

## Requirements
- R packages: `terra`, `sf`, `dplyr`, `tidyr`, `exactextractr`
- Stata (if the baseline model is written as a `.do` file)

## Purpose
These scripts highlight core components of my applied microeconometric workflow:
spatial data integration, model estimation, and simulation of policy-relevant
outcomes.
