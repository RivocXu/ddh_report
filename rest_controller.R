#load the libraries
library(blastula)
library(glue)
library(plumber)
library(tidyverse)

#https://medium.com/@skyetetra/using-docker-to-deploy-an-r-plumber-api-863ccf91516d

#* Send a report email
#* @param first_name first name of email recipient
#* @param email_address email address of recipient
#* @param path gotta figure out this path business
#* @param report_file the attachment to sent
#* @param input query parameters if no file
#* @param private variable to determine which data to sent
#* @get /send_email

send_email <- function(first_name = "there",
                       email_address = "matthew@hirschey.org",
                       path = NULL, #not sure
                       report_file = NULL, #only needs to be internal
                       input = list(), #only include query, and figure out rest?
                       private = FALSE #deconvolute from where GET came from
                       ){
  
  #logic for path
  if(is.null(path)){path <- getwd()}
  rmd_template <- "email_template.Rmd"
  #copy if file does not exist
  if(!file.exists(glue::glue("{path}/{rmd_template}"))){file.copy(from = rmd_template,
                                                                  to = glue::glue("{path}/{rmd_template}"),
                                                                  overwrite = TRUE)}
  setwd(path)
  
  #if report file doesn't exist, make it (and copy it to wd)
  # if(!file.exists(report_file)){report_file <- make_report(input = input, 
  #                                                          private = private)}
  #consider writing the report file to db so others can use it (i.e. we don't have to make it again)
  
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
  message <- glue::glue("sent an email to {first_name} at {email_address}")
  return(message)
}

# send_email(first_name = "John", 
#            input = list(type = "gene", query = "ROCK2", content = "ROCK2"), 
#  
