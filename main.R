source("fun.R")
message("waiting for messages...")
repeat {
  #check for message in sqs queue
  sqs_message <- get_message()
  
  #if no message, wait
  if(length(sqs_message$Messages) == 0){
    Sys.sleep(60)
  }
  #if message, then send email, log, and delete message
  if(length(sqs_message$Messages) == 1){
    #get json body attributes
    attributes <- jsonlite::fromJSON(sqs_message[["Messages"]][[1]][["Body"]])
    
    #send email
    send_email(first_name = attributes$first_name,
               email_address = attributes$email_address,
               input = list(type = attributes$type, 
                            query = attributes$query, 
                            content = attributes$content),
               private = as.logical(attributes$private))
    
    #make json df
    json_df <- tibble(
      first_name = attributes$first_name, 
      last_name = attributes$last_name, 
      email_address = attributes$email_address, 
      type = attributes$type, 
      query = attributes$query, 
      content = attributes$content,
      private = attributes$private)

    #log reports as csv
    csv_path <- tempfile(fileext = '.csv')
    write_csv(json_df, csv_path)
    good_csv_name <- glue::glue('{Sys.Date()}-{as.integer(Sys.time())}.csv') #gives it a unique name
    s3 <- paws::s3()
    s3$put_object(
      Bucket = "ddh-sqs-logs", 
      Body = csv_path,
      Key = good_csv_name
    )
    
    #consume message
    receipt_handle <- sqs_message$Messages[[1]]$ReceiptHandle
    delete_message(receipt_handle)
    
    #break #for testing only, send a message to break the loop
    Sys.sleep(1) #check for another message right away
  }
}

