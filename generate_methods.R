generate_methods <- function(){
  #render everything in quarto dir
  report_base_dir = glue::glue('{getwd()}/quarto') #here::here("quarto")
  
  #make tempdir our working directory
  owd <- getwd()
  setwd(report_base_dir) #for methods zipping
  on.exit(setwd(owd))
  
  #generate methods via quarto
  quarto::quarto_render(input = report_base_dir, #expecting a dir to render
                        output_format = "html", #output dir is set in _quarto.yml
                        cache_refresh = TRUE)
  
  #ZIP
  methods_files <- list.files(here::here("quarto", "methods"), 
                              recursive = TRUE, 
                              include.dirs = TRUE)
  
  message(glue::glue_collapse(methods_files, sep = ", ")) #just for confirmation
  
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
  compose_email(
    header = add_image(file = here::here("ddh-banner.png"), width = "100%"), 
    body = complete_message) %>%
    smtp_send(
      from = "hey@datadrivenhypothesis.com",
      to = "matthew@hirschey.org",
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



