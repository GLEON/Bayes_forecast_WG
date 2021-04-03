9_Data_publication

Scripts to compile hindcast .csvs into NetCDF format and create EML metadata for hindcasts, NLDAS, and PRISM data for publication at the Environmental Data Initiative (EDI).

Guide to files:

9A_rename_hindcast_files.R: renames hindcast .csv files to be compatible with 9B_create_netcdfs.R
9B_create_netcdfs.R: converts hindcasts .csv files to NetCDF format and generates an EML file for each NetCDF following Ecological Forecasting Initiative (EFI) standards
9C_MakeEMLHindcastsNLDASPRISM.R: script documenting workflow for creation of EML for data publication to EDI.
