# CDR_PM2.5_distribution_paper
Data and code for "Carbon dioxide removal could perpetuate community-scale inequalities in health impacts of U.S. air pollution" paper

Folder code contains all the code used in this project:
1. GCAM_data_analysis.R is used to analyze GCAM outputs
2. downscaling_electricity.R is used to downscale electricity generation, and thus emissions, from GCAM at a state level to a point source level with data from eGRID
3. dowsncaling_other_sectors.R is used to downscale other energy sectors from s state level to a sector level with NEI data
4. BenMAP_9km_grids_2019.R is used to prepare data for BenMAP for the contiguous U.S. at a 9 km resolution (row/column numbers, population, incidence rate)
5. BenMAP_1km_grids.R is used to prepare data for BenMAP for the 15 cities at a 1 km resolution (row/column numbers, population, incidence rate)
6. paper_figures.R is used to generate figures and data for the paper
7. color_schemes.R color palette
