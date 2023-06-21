source("fun.R")

#load public reports as character vec
get_public_reports()

generate_and_email_report <- function(details) {
  message("generate email for ", details['email_address'], " content:", details['content'])
  send_email(first_name = details['first_name'],
             email_address = details['email_address'],
             input = list(type = details['type'], 
                          subtype = details['subtype'], 
                          query = details['query'], 
                          content = details['content']),
             private = as.logical(details['private']), 
             greeting = details['greeting'])
}

process_message_body <- function(message_body) {
  message("process_message_body")
  report_details_df <- jsonlite::fromJSON(message_body)
  apply(report_details_df, 1, generate_and_email_report)
}

process_sqs <- function(Records) {
  message("process_sqs")
  lapply(Records$body, process_message_body)
  "ok"
}

lambdr::start_lambda()
