# CDR_PM2.5_distribution_paper
Code for "Carbon dioxide removal could perpetuate community-scale inequalities in health impacts of U.S. air pollution" paper. Data is located in Zenodo: input shapefiles, input data, and BenMAP results (10.5281/zenodo.13863764). It has to be downloaded, unzipped, and placed in this folder to run the code. Additionally, folder "output_data" is in Zenodo as well and can be placed here to run code "paper_figures.R" without running previous code.

This branch should have the following folders in it:

Folder "BenMAP_results" needs to be downloaded from Zenodo.

Folder "code" contains all the code used in this project:
1. GCAM_data_analysis.R is used to analyze GCAM outputs
2. downscaling_electricity.R is used to downscale electricity generation, and thus emissions, from GCAM at a state level to a point source level with data from eGRID
3. dowsncaling_other_sectors.R is used to downscale other energy sectors from s state level to a sector level with NEI data
4. BenMAP_9km_grids_2019.R is used to prepare data for BenMAP for the contiguous U.S. at a 9 km resolution (row/column numbers, population, incidence rate)
5. BenMAP_1km_grids.R is used to prepare data for BenMAP for the 15 cities at a 1 km resolution (row/column numbers, population, incidence rate)
6. paper_figures.R is used to generate figures and data for the paper
7. color_schemes.R color palette

Folder "data" needs to be downloaded from Zenodo.

Folder "figures" gets populated by running the code.

Folder "mappings" has mapping input files.

Folder "output_data" gets populated by running the code.

Folder "shapefiles" needs to be downloaded from Zenodo.
