source("fun_libraries.R") #used by fun.R too
Sys.setenv(VROOM_CONNECTION_SIZE = "1310720") #10x bigger; errors out otherwise

generate_methods <- function(email_address, 
                             email_zip = FALSE){
#generate methods via quarto
quarto::quarto_render(input = here::here("quarto"), #expecting a dir to render
                      output_format = "html", #output dir is set in _quarto.yml
                      cache_refresh = TRUE)

#ZIP
methods_files <- list.files(here::here("quarto", "methods"), 
                            recursive = TRUE, 
                            include.dirs = TRUE)

message(glue::glue_collapse(methods_files, sep = ", ")) #just for confirmation

#make tempdir our working directory
owd <- getwd()
setwd(here::here("quarto")) #for methods zipping
on.exit(setwd(owd))

#zip
final_zip_path <- glue::glue("{owd}/methods.zip")
zip(zipfile = final_zip_path, 
    files = glue::glue("methods/{methods_files}"))

#move to S3 as www/methods.zip
s3 <- paws::s3()
s3$put_object(
  Body = as.character(final_zip_path),
  Bucket = Sys.getenv("AWS_DATA_BUCKET_ID"),
  Key = "www/methods.zip"
)
complete_message <- "Hey, you just uploaded fresh methods to S3...nice work!"
message(complete_message)

#send email to spin down container
email_body <- compose_email(
  header = add_image(file = here::here("ddh-banner.png"), width = "100%"), 
  body = complete_message)

if(email_zip == TRUE){
email_body <- add_attachment(email = email_body,
                             file = final_zip_path)
}

email_body %>%
  smtp_send(
    from = "hey@datadrivenhypothesis.com",
    to = email_address,
    subject = "Methods completed",
    credentials = creds_envvar(
      user = Sys.getenv("SMTP_USERNAME"),
      #pass_envvar = Sys.getenv("SMTP_PASSWORD"), #use default
      host = "mail.privateemail.com",
      port = "465",
      use_ssl = TRUE
    )
  )
}

generate_methods(email_address = "matthew.hirschey@duke.edu") #"matthew@hirschey.org"


