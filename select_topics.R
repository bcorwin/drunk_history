library(dplyr)
library(glue)
library(mailR)

GROUP_CODE = "GPNRO"
FORCE_SEND = FALSE # Force a send even if not all users have submitted

email_user <- function(email, assigned_topic) {
  send.mail(
    from = Sys.getenv("EMAIL_FROM"),
    to   = email,
    subject = "Your assigned Drunk History topic",
    body = glue("Happy drinking! You have been assigned: {assigned_topic}"),
    smtp = list(host.name = "smtp.gmail.com", port = 465,
                user.name = Sys.getenv("EMAIL_FROM"),
                passwd = Sys.getenv("EMAIL_PASS"), ssl = TRUE),
    authenticate = TRUE,
    html = TRUE,
    send = TRUE)
}

load("submissions.Rdata")
load("users.Rdata")

assigned_topics <- users %>%
  filter(group_code == GROUP_CODE) %>%
  select(-created_at) %>%
  left_join(submissions, by = c("group_code", "user_code"))

if(any(is.na(assigned_topics$submission)) & FORCE_SEND != TRUE) {
  missing_users <- assigned_topics %>%
    filter(is.na(submission)) %>%
    select(email) %>%
    .[[1]] %>%
    paste0(collapse = ", ")
  stop(glue("Not all users have made a submission: {missing_users}"))
} else {
  assigned_topics <- assigned_topics %>%
    filter(!is.na(submission)) %>%
    group_by(user_code) %>%
    arrange(desc(created_at)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(rand_val = runif(n())) %>%
    arrange(rand_val) %>%
    mutate(assigned_topic = coalesce(lead(submission), first(submission))) %>%
    select(-created_at, -rand_val)

  apply(assigned_topics, 1, function(x) {
    email_user(email = x[["email"]],
               assigned_topic = x[["assigned_topic"]])
  })
}

