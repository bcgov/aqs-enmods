# AQS - EnMoDS Chain of Custody From System

The chain of custody forms used in by AQS-EnMoDS are built on the BC Governments CHEFS platform. There are three forms for different types of sampling.
-[Water and General Chemistry](https://submit.digital.gov.bc.ca/app/form/submit?f=39c5e913-b03e-416c-b86e-bb3f25b54d76)
-[Air](https://submit.digital.gov.bc.ca/app/form/submit?f=db911513-ae9f-4dcc-a405-82838cd4fad5)
-[Biological](https://submit.digital.gov.bc.ca/app/form/submit?f=adabfbf4-62ae-4246-9a4d-23b5a2335361)

Data used to populate many of the dropdowns in these CoCs comes directly from AQS via a github action that pulls the data, assembles a json, and posts that json to BC Box where if can be pulled into CHEFS as needed.
-Project
-Sampling Agency
-Laboratory
-Locations
-Collection Methods
-QC Type
-Medium

There are also json files that are manually updated as needed for
-Analytical Packages
-Preservatives


