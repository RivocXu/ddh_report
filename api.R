library(plumber)

r <- plumb("rest_controller.R")
r$run(port=80, host="0.0.0.0")

#http://127.0.0.1/send_email?first_name=Matt&email_address=matthew.hirschey%40duke.edu
