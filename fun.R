#LOAD-----
source("fun_libraries.R") #used by generate_methods.R too

#MESSAGES-----
# if needed for testing, send_report_message() is located in ddh package

get_message <- function(){
  #https://github.com/paws-r/paws/blob/main/examples/sqs.R
  sqs <- paws::sqs()
  sqs$receive_message(
    QueueUrl = Sys.getenv("AWS_SQS_SERVICE_URL")
  )
}

delete_message <- function(receipt_handle){
  sqs <- paws::sqs()
  sqs$delete_message(
    QueueUrl = Sys.getenv("AWS_SQS_SERVICE_URL"),
    ReceiptHandle = receipt_handle
  )
  message(glue::glue('Message {stringr::str_sub(receipt_handle, 1, 6)} deleted'))
}

#REPORTS-----
# input = list(type = "gene", subtype = "gene", query = "ROCK1", content = "ROCK1")
render_report <- function(input = list(), 
                          private, 
                          report_dir = NULL){ #removed output_file
  #render everything in quarto dir
  report_base_dir = glue::glue('{getwd()}/quarto') #here::here("quarto")
  
  if(is.null(report_dir)){
    # create a temporary directory
    temp_dir <- tempfile(pattern="tmpdir", tmpdir=report_base_dir)
    dir.create(temp_dir)
  } else {
    temp_dir <- report_dir
  }
  #make a pretty folder for figures
  dir.create(glue::glue('{temp_dir}/figures')) 
  #make tempdir our working directory
  owd <- getwd()
  setwd(temp_dir)
  on.exit(setwd(owd))
  
  # copy the qmd file into our temporary(current) directory
  qmd_files_from <- list.files(path = report_base_dir, pattern = "\\.qmd", full.names = TRUE)
  qmd_files_to <- list.files(path = report_base_dir, pattern = "\\.qmd", full.names = FALSE)
  file.copy(qmd_files_from, qmd_files_to, overwrite = TRUE) #setting wd makes this happen
  
  # set the filename of the qmd file we will use for rendering
  report_template <- glue::glue("report_{input$type}.qmd") #"report_gene_dummy.qmd"
  private_var <- if_else(private == TRUE, "true", "false")
  
  #good file names
  good_file_name <- input$content
  if (input$subtype == "gene_list" || input$subtype == "compound_list" || input$subtype == "cell_list") {
    good_file_name <- paste0("custom_", str_extract(input$content, pattern = "^[:alnum:]+"), "_query")
  }
  
  output_html_filename <- glue::glue("{good_file_name}_REPORT.html")
  output_zip_filename <- glue::glue("{good_file_name}.zip")
  
  quarto::quarto_render(input = report_template, #glue::glue('{temp_dir}/{report_template}'),
                        execute_params = list(type = input$type, 
                                              subtype = input$subtype,
                                              query = input$query,
                                              content = input$content, 
                                              private = private_var),
                        cache_refresh = TRUE, 
                        output_file = output_html_filename)# glue::glue('{temp_dir}/{output_html_filename}'))
  
  #get report files dir
  report_image_dir <- glue::glue("report_{input$type}_files")
  
  #copy figures into better sub-dir
  image_files_from <- list.files(path = glue::glue('{temp_dir}/{report_image_dir}'), pattern = "\\.png", recursive = TRUE, full.names = TRUE)
  image_files_to <- 
    image_files_from %>% 
    purrr::map_chr(~ stringr::str_replace(string = .x, pattern = glue::glue("{report_image_dir}/figure-html"), replacement = "figures")) %>% 
    purrr::map_chr(~ stringr::str_replace(string = .x, pattern = "1\\.png", replacement = glue::glue('{good_file_name}.png')))
  file.copy(image_files_from, image_files_to, overwrite = TRUE)
  unlink(glue::glue('{temp_dir}/{report_image_dir}'), recursive = TRUE) #remove so list.files works below
  
  #match pngs
  images <- list.files(path = temp_dir,
                       pattern = "\\.png",
                       recursive = TRUE) #
  
  # to keep the quarto sub-directory structure we will zip from within /quarto
  zip_filenames <- c(output_html_filename)
  zip_filenames <- append(zip_filenames, images) #glue::glue('{report_image_dir}/{images})')
  message(glue::glue_collapse(zip_filenames, sep = ", "))
  
  #zip
  final_zip_path <- glue::glue("{temp_dir}/{output_zip_filename}")
  zip(zipfile = final_zip_path, 
      files = zip_filenames)
  
  return(final_zip_path)
}

#render_report(input = list(type = "gene", subtype = "gene", content = "ROCK2"), private = TRUE)
#render_report(input = list(type = "gene", subtype = "gene", content = "ROCK2"), private = FALSE)

make_report <- function(input = list(), 
                        private, 
                        report_dir = NULL){
  #report type
  report_type <- dplyr::if_else(private == TRUE, "private", "public")
  
  #make dir
  if(is.null(report_dir)){
    #make temp dir
    report_base_dir = here::here("quarto")
    temp_dir <- tempfile(pattern="tmpdir", tmpdir=report_base_dir)
    dir.create(temp_dir)
  } else {
    temp_dir <- report_dir
  }
  #check for public_reports, mostly for testing
  if(!exists("public_reports")) {
    get_public_reports()
  }
  
  #for single reports, do some checks for whitelist and for previous report
  if(length(input$content) == 1){
    #good file names
    good_file_name <- input$content
    
    #whitelist
    if(good_file_name %in% public_reports){report_type <- "private"}
    
    #name report
    report_filename <- glue::glue('{good_file_name}_{report_type}.zip') #"ROCK1_private.zip"
    
    #checks to see if report exists by trying to update report_file
    #reports for whitelist will always exist
    s3 <- paws::s3()
    report_file <- 
      tryCatch({
        s3$head_object(Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
                       Key = report_filename) %>% 
          purrr::pluck("ContentType")}, #"application/zip"
        error = function(e) {
          NULL #set report file to NULL
        })
    
    #overwrite if(overwrite == TRUE) {report_file <- NULL} 
    
    #if report exists on S3
    if(!is.null(report_file)){ 
      #download file into temp dir
      report_path <- glue::glue("{temp_dir}/{report_filename}")
      s3$download_file(Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
                       Key = report_filename, 
                       Filename = report_path)
      message(glue::glue("grabbed old report for {input$content}: way to be efficient"))
      return(report_path)
    }
    #for multi reports, skip checks
  } else { 
    good_file_name <- paste0("custom_", str_extract(input$content, pattern = "^[:alnum:]+"), "_query")
    report_filename <- glue::glue('{good_file_name}_{report_type}.zip')
  } 
  #if multi-query or report zip doesn't exist, make report and return path
  report_path <- 
    render_report(input = input, 
                  private = private, 
                  report_dir = report_dir)
  
  #Upload file to s3 so we don't have to make it again
  if(length(input$content) == 1){
    s3$put_object(
      Body = as.character(report_path),
      Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
      Key = as.character(report_filename) #need as.character because of glue::glue
    )
    message(glue::glue("uploaded fresh report for {input$content}: nice work"))}
  return(report_path)
} 

# make_report(input = list(type = "gene", subtype = "gene", content = "ROCK1"), private = TRUE)
# make_report(input = list(type = "gene", subtype = "gene", content = "ROCK2"), private = TRUE)
# make_report(input = list(type = "cell", subtype = "cell", content = "HEPG2"), private = TRUE)

send_email <- function(first_name,
                       email_address,
                       input = list(), 
                       private, 
                       greeting = "default"){
  
  #make temp dir
  report_base_dir = here::here("quarto")
  temp_dir <- tempfile(pattern="tmpdir", tmpdir=report_base_dir)
  dir.create(temp_dir)
  
  report_file <- make_report(input = input, 
                             private = private, 
                             report_dir = temp_dir)
  
  #make pretty https://pkgs.rstudio.com/blastula/reference/index.html#section-email-sending-through-smtp
  email_body <- 
    compose_email( #https://github.com/rstudio/blastula/issues/226
      header = add_image(file = here::here("ddh-banner.png"), width = "100%"),
      body = render_email(input = here::here("email_template.Rmd"), 
                          envir = parent.frame(),
                          render_options = list(params = 
                                                  list(name = first_name, 
                                                       type = input$type,
                                                       subtype = input$subtype,
                                                       query = input$query,
                                                       content = input$content,
                                                       private = private, 
                                                       greeting = greeting)
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
  
  #attach report
  email_body <- add_attachment(email = email_body,
                               file = report_file)
  
  # email_body #preview it
  email_body %>%
    blastula::smtp_send(
      from = Sys.getenv("SMTP_FROM"),
      to = email_address,
      subject = "DDH report",
      credentials = creds_envvar(
        user = Sys.getenv("SMTP_USER"),
        pass_envvar = "SMTP_PASSWORD", #The name of the environment variable that holds the value for an email account password
        host = Sys.getenv("SMTP_HOST"),
        port = Sys.getenv("SMTP_PORT"),
        use_ssl = TRUE
      )
    )
  
  #remove tempdirs
  on.exit(unlink(temp_dir, recursive = TRUE))
}

# send_email(first_name = "Matthew",
#            email_address = "matthew@hirschey.org",
#            input = list(type = "gene", subtype = "gene", query = "ROCK2", content = "ROCK2"),
#            private = TRUE,
#            greeting = "seminar")

# send_email(first_name = "Ope",
#            email_address = "opeyemi.olabisi@duke.edu",
#            input = list(type = "gene", subtype = "gene", query = "APOL1", content = "APOL1"),
#            private = TRUE,
#            greeting = "seminar")

get_sqs_report <- function(){
  s3 <- paws::s3()
  sqs_list <- 
    s3$list_objects(Bucket = Sys.getenv("AWS_SQS_BUCKET_ID")) %>% 
    purrr::pluck("Contents")
  
  query_table <- tibble()
  
  for (i in seq_along(sqs_list)) {
    id <- sqs_list[[i]][["Key"]]
    
    object <- 
      s3$get_object(
        Bucket = Sys.getenv("AWS_SQS_BUCKET_ID"), 
        Key = id
      ) 
    
    timestamp <- 
      object %>% 
      purrr::pluck("LastModified")
    
    csv_object <- 
      object %>% 
      purrr::pluck("Body") %>% 
      rawToChar() %>% 
      readr::read_csv(., show_col_types = FALSE) %>% 
      dplyr::bind_cols(timestamp = timestamp) %>% 
      dplyr::select(timestamp, everything())
    
    query_table <-
      query_table %>% 
      dplyr::bind_rows(csv_object)
    
    message(glue::glue("retreived query {i}"))
  }
  
  message("finished retreiving queries")
  return(query_table)
  
}
# query_table <- get_sqs_report()

purge_reports <- function(){
  s3 <- paws::s3()
  #initial list images
  object_list <-
    s3$list_objects_v2(Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID")) %>%
    purrr::pluck("Contents") %>%
    map(function(x) keep(x, str_detect(string = x, pattern = "zip")))
  
  while (length(object_list) > 0) { #loops until ALL images are deleted
    s3$delete_objects(
      Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
      Delete = list(
        Objects = object_list,
        Quiet = FALSE)
    )
    
    #get new object_list
    object_list <-
      s3$list_objects_v2(Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID")) %>%
      purrr::pluck("Contents") %>%
      map(function(x) keep(x, str_detect(string = x, pattern = "zip")))
  }
  message("all reports removed from bucket")
}
# purge_reports()

#PUBLIC----
generate_public_reports <- function(public_reports = c("ROCK1", "ROCK2")){ #define public report genes
  # create a temporary directory
  temp_dir <- tempfile(pattern="tmpdir", tmpdir=here::here())
  dir.create(temp_dir)
  
  #save temp Rds object
  public_report_filename <- "public_reports.Rds"
  public_report_path <- glue::glue("{temp_dir}/{public_report_filename}")
  saveRDS(public_reports, public_report_path)
  
  #upload to S3 data & test data
  s3 <- paws::s3()
  s3$put_object(
    Body = as.character(public_report_path),
    Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
    Key = as.character(public_report_filename) #need as.character because of glue::glue
  )
  message("uploaded public reports list to your reports dir: way to share")
  
  #render and upload reports for public_report_list genes
  for (i in public_reports) {
    #make report and return path
    report_path <- 
      render_report(input = list(
        type = "gene", 
        subtype = "gene", 
        query = i, 
        content = i
      ), 
      private = TRUE)
    
    #Upload file to s3 so others can use it (i.e. we don't have to make it again)
    s3$put_object(
      Body = as.character(report_path),
      Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
      Key = as.character(glue::glue("{i}_private.zip")) #need as.character because of glue::glue
    )
    message(glue::glue("uploaded {i} report"))
  }
  
  #delete temp_dir
  unlink(temp_dir, recursive = TRUE)
  
}
# generate_public_reports()

get_public_reports <- function(){
  #download data from public reports generation
  s3 <- paws::s3()
  s3_download <- s3$get_object(
    Bucket = Sys.getenv("AWS_REPORT_BUCKET_ID"),
    Key = "public_reports.Rds")
  
  public_reports <-
    s3_download$Body %>%
    rawConnection() %>%
    gzcon %>%
    readRDS
  
  assign("public_reports",
         public_reports,
         envir = .GlobalEnv)
  
  return(public_reports)
}
