#load functions
source("fun.R")
#source("test.R") #checks to make sure that you're not deploying TEST data

#download data 1 time
download_ddh_data(app_data_dir = here::here("data"),
                  test = TRUE,
                  overwrite = FALSE)

#wait for message
message("waiting for messages...")
repeat {
  #check for message in sqs queue
  sqs_message <- get_message() #in fun.R
  
  #if no message, wait
  if(length(sqs_message$Messages) == 0){
    Sys.sleep(60)
  }
  #if message, then send email, log, and delete message
  if(length(sqs_message$Messages) == 1){
    #get json body attributes as df
    attributes <- jsonlite::fromJSON(sqs_message[["Messages"]][[1]][["Body"]])
    
    #send email, in fun.R
    send_email(first_name = attributes$first_name,
               email_address = attributes$email_address,
               input = list(type = attributes$type, 
                            subtype = attributes$subtype, 
                            query = attributes$query, 
                            content = attributes$content),
               private = as.logical(attributes$private))
    
    #log reports as csv
    csv_path <- tempfile(fileext = '.csv')
    write_csv(attributes, csv_path)
    good_csv_name <- glue::glue('{Sys.Date()}-{as.integer(Sys.time())}.csv') #gives it a unique name
    s3 <- paws::s3()
    s3$put_object(
      Bucket = Sys.getenv("AWS_SQS_BUCKET_ID"), 
      Body = csv_path,
      Key = good_csv_name
    )
    
    #consume message
    receipt_handle <- sqs_message$Messages[[1]]$ReceiptHandle
    delete_message(receipt_handle) #in fun.R
    
    #break #for testing only, send a message to break the loop
    Sys.sleep(1) #check for another message right away
  }
}

