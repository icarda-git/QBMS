<!--
# QBMS 0.9.0 _2022-10-??_
  * Add TerraClimate support to query and retrieve climate data (including the 19 BIOCLIM variables).
  * Add a new gigwa_get_metadata function to retrieve metadata from GIGWA database.
  * Speed up internal get_program_trials() function by caching the list. Thanks to Thanks to [Johan Steven Aparicio](https://github.com/AparicioJohan)
-->
# QBMS 0.8.0 _2022-05-18_ 
  * Add GIGWA support to query and retrieve SNPs data for GWAS and GS analysis pipelines.
  * Tiny usability enhancements (e.g., automate path parameter setting, show the server name in the login window, add progress bar to the get_program_studies function).
  * Fix mistakenly redundant location names/info returned by the get_program_studies function.

# QBMS 0.7.0 _2022-03-03_ 
  * Add BreedBase support using BrAPI v1 calls.
  * Add functionality to get the pedigree table starting from germplasm dataset.
  * Improve set_qbms_config to generalize the way of getting the server domain from the URL.
  * Default timeout become 120 sec instead of 10.
  * Set default encoding for HTTP content to UTF-8.

# QBMS 0.6.0 _2021-10-08_ 
  * Fix filter by year functionality in the list_trials function.
  * Fix get_germplasm_data by replaced the deprecated germplasm-search call.
  * Minimize package dependencies (rbindx replaced plyr::rbind.fill, rbindlistx replaced data.table::rbindlist, and use merge to replace dplyr::left_join).
  * Resolve compatibility issues with BrAPI changes in BMS version 19.
  * Enable to set the connection time_out in the set_qbms_config function.
  * Get entry type (test or check) in the get_germplasm_list returned data frame.

# QBMS 0.5.0 _2021-07-08_ 
  * Fix the issue of empty list in get_germplasm_data returned results.
  * Fix retrieving error when the study has no data!
  * Enhance returned info by the get_program_studies function to include study settings and number of test/check entries.

# QBMS 0.4.1 _2020-10-16_ 
  * Simplify configuration by required only the URL of the BMS login page.
  * Improve the performance of the internal get_program_trials function by passing the programDbId in the /trials GET call.
  * Add debug_qbms function to get the internal config/state object.

# QBMS 0.4.0 _2020-07-03_ 
  * Convert it into an R package.
  * Add set_qbms_config function to setup connection configuration variables.
  * Use the double colon approach for functions from external packages.
  * Fix the deprecated API call in the get_trial_obs_ontology function.

# QBMS 0.3.1 _2020-06-09_ 
  * Fix the "get_trial_data" function bug when you have more than one study in the same location. 
  * Function "list_studies" returns studyName also, and function "set_study" input is studyName now.
  * Simplify the "get_germplasm_list" function output by getting rid of nested lists.
  * Deprecate the "list_all_studies" function in favor of "get_program_studies" function.

# QBMS 0.3.0 _2020-06-02_ 
  * Call BrAPI directly (i.e. not required "CIP-RIU/brapi" from GitHub anymore).
  * Add a function to get all data of the current active trial (combined all studies).
  * Add a function to get a list of studies where given germplasm has been used.
  * Add a function to get a specific germplasm data from all program trials.
  * Handle BrAPI pagination in a proper way.

# QBMS 0.2.0 _2019-08-20_ 
  * Adopt tidyverse style guide https://style.tidyverse.org/
  * Add functions documentation using roxygen2 format.
  * Add basic error handling to the functions.
  * Add a function to retrieve the traits ontology of a trial.

# QBMS 0.1.0 _2019-07-24_ 
  * Initial version.
