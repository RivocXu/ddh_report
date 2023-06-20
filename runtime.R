library(blastula)

generate_and_email_report <- function(details) {
  message("generate email for ", details['email_address'])
  #message(details['email_address'])
  #message(details['type'])
  #message(details['subtype'])
  #message(details['query'])
  #message(details['content'])
  #message(details['private'])
  #message(details['greeting'])

  # Put this back in to test email integration
  email <- compose_email(body=md(paste0("Hello", details['first_name'], details['last_name'])))
  smtp_send(
      email = email,
      from =  Sys.getenv("SMTP_FROM")
      to = details['email_address'],
      subject = "DDH report",
      credentials = creds_envvar(
        user = Sys.getenv("SMTP_USER"),
        host = Sys.getenv("SMTP_HOST"),
        port = Sys.getenv("SMTP_PORT"),
      )
  )
  # SMTP_PASSWORD
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
