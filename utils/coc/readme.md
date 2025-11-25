# AQS - EnMoDS Chain of Custody From System

The chain of custody forms used in by AQS-EnMoDS are built on the BC Governments CHEFS platform. There are three forms for different types of sampling.
- [Water and General Chemistry](https://submit.digital.gov.bc.ca/app/form/submit?f=39c5e913-b03e-416c-b86e-bb3f25b54d76)
- [Air](https://submit.digital.gov.bc.ca/app/form/submit?f=db911513-ae9f-4dcc-a405-82838cd4fad5)
- [Biological](https://submit.digital.gov.bc.ca/app/form/submit?f=adabfbf4-62ae-4246-9a4d-23b5a2335361)

Data used to populate many of the dropdowns in these CoCs comes directly from AQS via a [github action](https://github.com/bcgov/aqs-enmods/actions/workflows/update_coc_json.yml) that pulls the data, assembles a json, and posts that json to BC Box where if can be pulled into CHEFS as needed.
- Project
- Sampling Agency
- Laboratory
- Locations
- Collection Methods
- QC Type
- Medium

Because the locations object is so big (~34,000 as of Nov 2025) it is impractical to read them all from AQS via API so instead the [spatial object](https://bcbox.nrs.gov.bc.ca/detail/objects?objectId=6fc7cb4c-dabf-4c41-bb69-f97045a1ed35) is used as the source of data - it is then further filtereed to get a list of stations that will load quickly in the CoC form. 

There are also json files that are manually updated as needed for
- Analytical Packages
- Preservatives

There are also print templates that are Microsoft Word Documents. These have been placed in the jsons sub-folder.

# Known Issues and Work Arounds
The following are known issues with the CHEFS based implementation of the CoC generating system.

## Not all locations in drop down
Because the locations list is so large (>34,000) records not all of them can be loaded into the form without causing unacceptable performance. To mitigate this only locations made in the last three years or sampled in the last five years display in the drop down. This should account for most commonly used locations. The sync is done nightly as a GitHub action. However, there are two failure states.
- A new location is made and the user whishes to make a CoC for it immediately. In this case the new location ID will not appear in the drop down until the following day when the update tasks runs overnight.
- An older location (>3 years since creation or >5 years since last sample) is sampled but does not appear in the drop down.

Both of these issue can be worked around by selecting 'Other - Enter below' in the location drop down which will cause a text box to open where the user can manually add the location ID they wish to sample.

Another issues that can appear from time to time is browser caching that can also cause what appear to be sync delays. To get around this use private windows or clear the browser cache.

## Project not appearing in drop down
Only projects classified as 'study' display in the CHEFS form this is done because authorization projects are not sampled by ministry staff and as such would needless clutter the dropdown. There is known use case for ministry staff to sample on behalf of a permit holder.

## Analytical group and or observed property not available in drop down
Although the list of analytical groups is extensive and based on consultation with the lab services unit. If there is an analytical group or test a user would like that isn't list they can select 'Other - List Below' as the analytical group and enter the test(s) as free text. If this is common the lab services unit should be consulted and consider adding the group to the analytical packages json file.
