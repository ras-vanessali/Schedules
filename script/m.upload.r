###### pick the bucket to upload file for different environment 
if(env == "test"){
  gcs_global_bucket('appraisals-data-dev-c55fa4-schedules-import-etl')
} else{
  gcs_global_bucket('appraisals-data-prod-707493-schedules-import-etl')
}

#################### Upload file to GCP bucket ########################
gar_auth(token = gargle::credentials_app_default(scopes = "https://www.googleapis.com/auth/cloud-platform"))
gcs_get_global_bucket()

uploadfunc <- function(input, output){
  write.csv(input, file = output,row.names = F)
}
upload_df=excel_out

gcs_upload(upload_df, name = paste("upload/schedule",CountryCode,publishDate,".csv"),object_function = uploadfunc)

setwd(file_path)  
write.csv(upload_df,paste('ClassificationModifySchedule',CountryCode,format(Sys.time(),format='%Y%m%d%H%M'),'VL.csv',sep=''),row.names = F)
