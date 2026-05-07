# CDR_PM2.5_distribution_paper
Code for "Residual emissions may perpetuate community-scale inequalities of U.S. air pollution in net-zero scenarios", published at Nature Climate Change (DOI here) 

Data is located in Zenodo: input shapefiles, input data, and BenMAP results (DOI: 10.5281/zenodo.20045877). It has to be downloaded, unzipped, and placed in this folder to run the code. 

Additionally, folder "output_data" is in Zenodo as well and can be placed here to run code "paper_figures.R" without running previous code.

This branch should have the following folders in it:

- Folder "BenMAP_results" needs to be downloaded from Zenodo and it contains results from BenMAP runs.

- Folder "code" contains all the code used in this project:
  - 1_GCAM_data_analysis.R is used to analyze GCAM outputs
  - 2_downscaling_electricity.R is used to downscale electricity generation, and thus emissions, from GCAM at a state level to a point source level with data from eGRID
  - 3_downscaling_other_sectors.R is used to downscale other energy sectors from state level to a sector level with NEI data
  - 4_BenMAP_9km_grids_2019.R is used to prepare data for BenMAP for the contiguous U.S. at a 9 km resolution (row/column numbers, population, incidence rate). Code also reads and processes BenMAP mortality results
  - 5_BenMAP_1km_grids.R is used to prepare data for BenMAP for the 15 cities at a 1 km resolution (row/column numbers, population, incidence rate). Code also reads and processes BenMAP mortality results
  - 6_paper_figures.R is used to generate figures and data for the paper (based on outputs from previous code)
  - color_schemes.R has color palette

- Folder "data" needs to be downloaded from Zenodo and it contains input data.

- Folder "figures" gets populated by running the code.

- Folder "mappings" needs to be downloaded from Zenodo and has mapping input files.

- Folder "output_data" gets populated by running the code, and can also be downloaded from Zenodo to generate paper figures without running all the previous code.

- Folder "shapefiles" needs to be downloaded from Zenodo and has files with geographical domains.
