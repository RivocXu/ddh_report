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
    #get body attributes
    attributes <- jsonlite::fromJSON(sqs_message[["Messages"]][[1]][["Body"]])
    
    #send email
    send_email(first_name = attributes[1],
               email_address = attributes[2],
               input = list(type = attributes[3], query = attributes[4], content = attributes[5]),
               private = as.logical(attributes[6]))
    
    #log reports
    report_tibble <- tibble(
      first_name = attributes[1],
      email_address = attributes[2],
      type = attributes[3], 
      query = attributes[4], 
      content = attributes[5],
      private = as.logical(attributes[6])
    )
    
    #db connection
    #write to db
    
    #consume message
    receipt_handle <- sqs_message$Messages[[1]]$ReceiptHandle
    delete_message(receipt_handle)
    
    #break #for testing only, send a message to break the loop
    Sys.sleep(1) #check for another message right away
  }
}

