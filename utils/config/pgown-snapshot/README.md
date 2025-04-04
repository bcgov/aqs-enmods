<!--
Copyright 2018 Province of British Columbia
&#10;Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
&#10;https://www.apache.org/licenses/LICENSE-2.0
&#10;Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, 
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->

# pgown-snapshot <a href='https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/groundwater-wells-aquifers/groundwater-observation-well-network'><img src='man/figures/BC_gov_logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/license/apache-2-0)
<!--[![R-CMD-check](https://github.com/bcgov/bcdata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bcgov/bcdata/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bcgov/bcdata/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bcgov/bcdata?branch=main)-->
<!-- badges: end -->

An R Coding pipeline for retrieving & summarizing data on the [Provincial Groundwater Observation Well Network (PGOWN)](https://www2.gov.bc.ca/gov/content?id=B03D0994BB5C4F98B6F7D4FD8610C836) in the [real-time water data](https://www2.gov.bc.ca/gov/content?id=39A675506AE54C4CB240849338B7C8D8) database used to prepare bi-annual snapshots of the network and its performance. This reporting too and data pipeline maintained by the Data Management unit of the LSDMR section (EMAB branch of the EPD division) in the Ministry of Env and Parks, Govt. of British Columbia.

### Code structure

The coding flow has three streams of data input/output. 

data: contains reference data needed to run the scripts (data_jb, data_rc, data_old) that need to be updated for each snapshot, and a folder for storing new data (data_new) generated on running this script.

generated: contains a copy of the figures and tables generated on running this script as well as their underlying data.

rcode: contains the R Markdown file that needs to be run to generate the output report "PGOWN-Snapshot.html". Also contains the script for API calls to the AQUARIUS database as well as an .Renviron that needs to be updated for each snapshot.Because the script pulls data from the production version of the databse it does take several minutes to complete. 

**Note:** The `pgown-snapshot` coding pipeline uses data from the AQUARIUS Time-Series API Client, utilizing the timeseries_client.R file written and maintained by [Aquatic Informatics](https://github.com/AquaticInformatics) which can be found in their repo [here](https://github.com/AquaticInformatics/examples/blob/fa417675042ea1f1d08358f2c42244e7c4baac23/TimeSeries/PublicApis/R/timeseries_client.R). Data on the AQTS server is password protected. If you do not have verified and functional credentials, you will not be able to use this coding pipeline. If you have credentials, update them in the .Renviron file in the folder rcode. Currently they are set to test values and will not work. If you have credentials but encounter errors in running this code, please file an
[issue](https://github.com/bcgov/PGOWN-snapshot/issues/).

### Installation

1. Download the entire folder structure from GitHub as a ZIP file to your local machine or use github desktop.
2. Unzip the file using the "Extract Here" option onto your parent directory. By default, the extracted folder is the root directory (PGOWN-snapshot-main).
3. Rename the folder titled "2025Feb". If you are generating a snapshot report due in July 2025, rename this folder to "2025July".
4. Next, within the folder renamed above (see Step 3), go to /coding_flow/data/ and get updated files from relevant stakeholders for folders data_jb and data_rc.
5. If you want to simply use the files already in this folder, open subfolder "data_rc" and unzip the file "PGOWN_Grades_Appr.zip" and "Extract Here" the csv file.
6. Snapshot reports are cumulative, and contain data that iteratively gets updated after every report. The updated data is stored in "data_new". For every new snapshot report, replace the files in "data_old" by copying files from "data_new".
7. Download the timeseries_client.R file written and maintained by [Aquatic Informatics](https://github.com/AquaticInformatics) which can be found in their repo [here](https://github.com/AquaticInformatics/examples/blob/fa417675042ea1f1d08358f2c42244e7c4baac23/TimeSeries/PublicApis/R/timeseries_client.R). Copy this file to the subfolder "rcode".
8. Update the .Renviron file in the root directory (PGOWN-snapshot main) with updated API credentials.
9. Open the file "app.R" currently in the root directory. This is an R Shiny app so you would need to have the "shiny" package installed before running it. Your R installation might call this package automatically to run this app.
10. Run the app using updated parameter values including due dates of current and previous reports, start and end dates for studying data gaps, as well as specific policy targets and the years in which they were announced.
11. If you encounter errors in running this code, please file an [issue](https://github.com/bcgov/PGOWN-snapshot/issues/).
12. An updated report called "PGOWN-Snapshot.html" should be placed in the folder renamed in Step 3.

### Reference

[pgown-snapshot 📦 home page and reference
guide](https://bcgov.github.io/pgown-snapshot/)

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/PGOWN-snapshot/issues/).

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](https://github.com/bcgov/PGOWN-snapshot/blob/master/CONTRIBUTING.md)
guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/bcgov/bcdata/blob/master/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

### License

Copyright 2018 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the “License”); you may
not use this file except in compliance with the License. You may obtain
a copy of the License at

<https://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

------------------------------------------------------------------------

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*
