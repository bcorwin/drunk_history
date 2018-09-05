library(dplyr)
library(glue)
library(mailR)

# Global functions --------------------------------------------------------
readRenviron('.Renviron')

user_emails <- c(
  "bscorwin+1@gmail.com",
  "bscorwin+2@gmail.com",
  "bscorwin+3@gmail.com",
  "bscorwin+4@gmail.com",
  "bscorwin+5@gmail.com"
)

SUBJECT  <- "Drunk History Submission Form"
EMAIL_BODY <- {"Please make your submission by September 10, 2018

Click <a href='www.drunkhistory.bscorwin.com/?group_code={group_code}&user_code={user_code}'>here</a> to make your submission.

If the above link does not work go here: www.drunkhistory.bscorwin.com
And use the following information:
Group code = {group_code}
User code = {user_code}"}
# Helper functions --------------------------------------------------------

gen_code <- function(n, len, prefix = "", use_digits = TRUE, use_chars = TRUE) {
  char_set <- NULL
  if(use_digits) {
    char_set <- c(char_set, 0:9)
  }
  if(use_chars) {
    char_set <- c(char_set, LETTERS)
  }
  out <- c(prefix, sample(char_set, n, replace = TRUE))
  paste0(out, collapse = "")
}

email_user <- function(email, group_code, user_code) {
  send.mail(
    from = Sys.getenv("EMAIL_FROM"),
    to   = email,
    subject = SUBJECT,
    body = glue(EMAIL_BODY),
    smtp = list(host.name = "smtp.gmail.com", port = 465,
                user.name = Sys.getenv("EMAIL_FROM"),
                passwd = Sys.getenv("EMAIL_PASS"), ssl = TRUE),
    authenticate = TRUE,
    html = TRUE,
    send = TRUE)
}

# Create group ------------------------------------------------------------

if(file.exists("users.Rdata")) {
  load("users.Rdata")
}

new_users <- data.frame(
  email = user_emails
) %>%
  mutate(
    group_code = gen_code(4, prefix = "G", use_digits = FALSE),
    user_code = sapply(1:n(), gen_code, n = 4, prefix = "U", use_chars = FALSE),
    created_at = Sys.time()
  )

if(exists("users")) {
  chk <- users %>%
    inner_join(new_users, by = c("group_code", "user_code"))
} else {
  chk <- data.frame()
  users <- new_users[0,]
}

if(nrow(chk) == 0) {
  # to do: error check
  apply(new_users, 1, function(x) {
    email_user(email      = x[["email"]],
               group_code = x[["group_code"]],
               user_code  = x[["user_code"]])
  })

  users <- users %>%
    bind_rows(new_users)
  save(users, file="users.Rdata")
} else {
  stop("Duplicate codes. Please rerun.")
}