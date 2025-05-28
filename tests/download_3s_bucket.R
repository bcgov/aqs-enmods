#Get stuff out of hidden s3 buckets


readRenviron(paste0(getwd(), "./.Renviron"))

Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("AWS_ACCESS_KEY"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("AWS_SECRET_ACCESS_KEY"),
           "AWS_S3_ENDPOINT" = "nrs.objectstore.gov.bc.ca")

objects <- get_bucket(bucket = "enmods-test", region = "")


for (obj in objects) {
  key <- obj$Key
  save_object(object = key, bucket = "enmods-test", region = "")
}
