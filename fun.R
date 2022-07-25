library(tidyverse)

library(blastula)
library(glue)
library(jsonlite)
library(paws)

send_tmp_message <- function(){ #for teting only
  sqs <- paws::sqs()
  json <- '["there", "matthew@hirschey.org", "gene", "ROCK1", "ROCK1", "TRUE"]'
  sqs$send_message(
    QueueUrl = "https://sqs.us-east-1.amazonaws.com/344055253315/emailer",
    MessageBody = json
  )
}
# send_tmp_message()

get_message <- function(){
  #https://github.com/paws-r/paws/blob/main/examples/sqs.R
  sqs <- paws::sqs()
  sqs$receive_message(
    QueueUrl = "https://sqs.us-east-1.amazonaws.com/344055253315/emailer"
  )
  
}

delete_message <- function(receipt_handle){
  sqs <- paws::sqs()
  sqs$delete_message(
    QueueUrl = "https://sqs.us-east-1.amazonaws.com/344055253315/emailer",
    ReceiptHandle = receipt_handle
  )
  message(glue::glue('Message {str_sub(receipt_handle, 1, 6)} deleted'))
}

send_email <- function(first_name = "there",
                       email_address = "matthew@hirschey.org",
                       input = list(),
                       private = FALSE, 
                       path = NULL #not sure
){
  
  #logic for path
  if(is.null(path)){path <- getwd()}
  rmd_template <- "email_template.Rmd"
  #copy if file does not exist
  if(!file.exists(glue::glue("{path}/{rmd_template}"))){file.copy(from = rmd_template,
                                                                  to = glue::glue("{path}/{rmd_template}"),
                                                                  overwrite = TRUE)}
  setwd(path)
  
  #looks to see if report exists
  report_file <- NULL
  # s3 <- paws::s3()
  # report_file <- 
  #   s3$get_object(
  #     Bucket = "ddh-reports", 
  #     Key = glue::glue('{input$content}.zip')) #"ADCK3.zip"
  # if(is.null(report_file)){
  #   #if report doesn't exist, then report_file remains null and makes a report
  #   #report_file <- make_report(input = input, private = private)
  #   
  #   
  #   #Upload file to s3 so others can use it (i.e. we don't have to make it again)
  #   if(length(input$content) == 1){ #but only for single queries
  #     s3$put_object(
  #       Body = report_file,
  #       Bucket = "ddh-reports", 
  #       Key = glue::glue('{input$content}.zip')
  #     )
  #   }
  # }
  
  #make pretty https://pkgs.rstudio.com/blastula/reference/index.html#section-email-sending-through-smtp
  email_body <- 
    compose_email( #https://github.com/rstudio/blastula/issues/226
      header = add_image(file = "ddh YouTube banner.png", width = "100%"),
      body = render_email(input = rmd_template, 
                          envir = parent.frame(),
                          render_options = list(params = 
                                                  list(name = first_name, 
                                                       type = input$type,
                                                       query = input$query,
                                                       private = private)
                          ))$html_html,
      footer = blocks(
        #block_text("Thanks for reading! Find us here:"),
        block_social_links(
          social_link(
            service = "website",
            link = dplyr::if_else(private == TRUE, "https://www.datadrivenhypothesis.com", "https://www.datadrivenhypothesis.org"),
            variant = "bw"
          ),
          social_link(
            service = "twitter",
            link = "https://www.twitter.com/dd_hypothesis",
            variant = "color"
          ),
          social_link(
            service = "youtube",
            link = "https://www.youtube.com/channel/UCstAS6IpgYgZ9Cvcag_YlCQ",
            variant = "color"
          ),
          social_link(
            service = "github",
            link = "https://github.com/matthewhirschey",
            variant = "color"
          )
        )
      )
    )
  
  if(!is.null(report_file)){email_body <- add_attachment(email = email_body,
                                                         file = report_file) #figure out paths
  }
  
  # email #preview it
  email_body %>% 
    smtp_send(
      from = "matthew@hirschey.org",
      to = email_address,
      subject = "DDH report",
      credentials = creds_envvar(
        provider = "gmail",
        user = Sys.getenv("SMTP_USERNAME")
        #pass_envvar = Sys.getenv("SMTP_PASSWORD"), #use default
      )
    )
}

# send_email(first_name = "John", 
#            input = list(type = "gene", query = "ROCK2", content = "ROCK2"), 
#            private = TRUE)
