# aqs-enmods
<!-- badges: start -->

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/license/apache-2-0)
<!--[![R-CMD-check](https://github.com/bcgov/bcdata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bcgov/bcdata/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bcgov/bcdata/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bcgov/bcdata?branch=main)-->
<!-- badges: end -->
 A collection of useful functions and tests to manage the BC Government's instance of Aquatic Informatics Samples. An API token is required.
 
## Code Style Standards

Naming conventions based on the reference value (SNAKE CASE)

  1. For file names/sheets, we follow the format "Snake_Case" 
  2. For file names coming from EMS, we follow the format "EMS_<Data_Type>_Date"
  3. For variable names in coding pipelines:
   - Use `snake_case` format if "case" is a version of "snake"
   - Use `snake.case` format if "case" is an element of the "snake"
   - Column names for tables also follow `snake_case` to avoid parsing issues
   - AQI uses `camelCase`, so following these styles helps distinguish BC Gov vs AQI-originated data

## Steps to configure an instance of EnMoDS - AQS

This pipeline sets up a clean configuration environment by systematically loading all relevant domain objects (profiles) into the system via API requests.

To start from a clean environment, first wipe the environment so it has no configuration (see how to wipe an environment below)

### 1. Run the Main Configuration Script

Run the master file `main_config_runner.R`. The source scripts, their generated tables, and function calls have been added in the right order already.
Details on these components are given below.

     A. Support Preprocessing Scripts and Tables
     B. Support Functions 

#### A. Support Preprocessing Scripts and Tables

These scripts read, clean, transform, and prepare the datasets required to configure EnMoDS:

- `preprocessing_units_unitsgroups.R`: 
  - Consolidates EMS unit data with internal references, classifies units, standardizes names, and prepares `units` and `unit_groups` datasets
  - Generates tables `units` and `unit_groups` needed for uploads that follow

- `preprocessing_observed_properties.R`: 
  - Merges EMS and custom observed property files, links them with unit/unit group IDs, standardizes labels, and prepares `observed_properties`
  - Generates table `observed_properties` 
  
- `preprocessing_others.R`: 
  - Prepares other core tables including `methods`, `labs`, `projects`, `taxonomy_levels`, `fish_taxonomy`, `detection_conditions`, `result_grades`, `result_statuses`, `mediums`, and `extended_attributes`
  - Generates all other tables except location-related
  
- `preprocessing_sampling_locations.R`: 
  - Consolidates non-cancelled sampling locations and joins them with permit data to prepare `locations` and `location_groups`.
  - Note that locations need to be uploaded manually before saved filters can be uploaded to AQS
  - Generates `locations` table needed to upload `location_groups`
  
- `preprocessing_saved_filters.R`: 
  - Prepares `filters` by joining saved filter records with location GUIDs and structuring data for API compatibility.
  - Generates the `saved_filters` table
  
#### B. Support Functions

Stylized along the lines of the underlying AQS API, these functions are used to:

- Post: The function `post_profiles` uploads a profile to the AQS database
- Put: The function `put_profiles` uploads a profile into the AQS database
- Get: The function `get_profiles` downloads an AQS profile
- Get: The function `get_profiles_for_url` downloads an AQS profile if the API URL is known
- Delete: The function `del_profiles` deletes an AQS profile
- Delete: The function `delete_all_profiles` deletes all AQS profiles. Note that 
some data cannot be deleted because they are marked as required by AQS. The current
code configuration accounts for such required data and does not delete them.

These functions are all stored in `api_functions.R`
  
### 2. Steps to wipe an instance of EnMoDS - AQS

Currently, every new setup comes with complete wipe prior to build. This is executed through 
the function `delete_all_profiles` stored in the source file `preprocessing_delete.R`.
  
### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an issue.

### How to Contribute

If you would like to contribute to the package, please see our
CONTRIBUTING guidelines.

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
