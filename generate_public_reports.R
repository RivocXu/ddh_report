#load libraries
source("fun.R") 

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
