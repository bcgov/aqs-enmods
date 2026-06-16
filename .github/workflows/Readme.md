## EnMoDS Actions
GitHub actions are used to maintain reference lists for EDT users on our public web page and drop down menus on the chain of custody forms. If any of the actions stop working data will go stale, this isn't a significant issue unless they are offline for several weeks. There is also an action that is now turned off that was used to download performance logs provided by AQI.

```prod_update_reference_lists.yml``` downloads reference lists from the production instance of AQS and repost them as nicely formatted csv files in BC Box. These files are then linked with permalinks to the EnMoDS public web page for EDT users to access. This script requires tokens to access AQS and BC Box. The R script is located here ```utils/edt_reference_tables/make_reference_tables_prod.R```. This action runs once a day.

```update_coc_json.yml``` extracts and filters reference lists for use on the CoC forms. There is more aggressive filtering done to maintain high performance of the CoC and quick loading times for users. The exported json files are saved in BC Box. The R script is located here ```utils/coc/CoC_json_update.R```. This action runs once a day.

```AQS_performance_logs.yml``` downloaded weekly AQS performance logs from AQI's private S3 bucket and saved them to BC Box. The R script that did that is here ```utils/AQS_Performance_Logs.R```. When it was used it was run weekly by AQI stopped providing these reports once the Queue API end point was added to AQS.

