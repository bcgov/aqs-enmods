library(aws.s3)

readRenviron(paste0(getwd(), "./.Renviron"))

#set up account access for BC boc
Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("aws_access_key"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("aws_secret_access_key"),
           "AWS_S3_ENDPOINT" = "nrs.objectstore.gov.bc.ca")

#test connection and list buckets
bucketlist(region = "")

#list all files in the bucket
get_bucket("enmods", region = "")

#post a test file to a bucket, this can also be used to update a file
put_object(file = "./utils/coc/jsons/test_proj_data.json", 
           object = "CoC_Tables/project_json_from_R_test_workflow.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")

#sync the file to make it appear in the Bc Box interface

