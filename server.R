library(shiny)
library(shinyjs)
library(dplyr)
library(glue)
library(mailR)

readRenviron(".Renviron")

shinyServer(function(input, output) {

  observeEvent(input$action_submit, {
    load("users.Rdata")

    cur_group_code <- toupper(input$group_code)
    cur_user_code  <- toupper(input$user_code)
    submission     <- input$user_submission

    all_groups <- unique(users$group_code)
    group_users <- users %>%
      filter(group_code == cur_group_code) %>%
      select(user_code) %>%
      .[[1]]

    if(!(cur_group_code %in% all_groups)) {
      alert("SUBMISSION NOT ACCEPTED\nInvalid group code.")
      return()
    }
    if(!(cur_user_code %in% group_users)) {
      alert("SUBMISSION NOT ACCEPTED\nInvliad user code.")
      return()
    }

    user_email <- users %>%
      filter(user_code == cur_user_code) %>%
      select(email) %>%
      .[[1]]

    new_submission <- data.frame(
      group_code = cur_group_code,
      user_code  = cur_user_code,
      submission = submission,
      created_at = Sys.time(),
      stringsAsFactors = FALSE
    )

    submissions <- tryCatch({
      load("submissions.Rdata")
      submissions %>%
        bind_rows(new_submission)
    }, error = function(x) {
      print(x)
      new_submission
    })

    save(submissions, file = "submissions.Rdata")
    print(user_email)
    print(submission)
    send.mail(
      from = Sys.getenv("EMAIL_FROM"),
      to   = user_email,
      subject = "Your Drunk History submission",
      body = glue("Thank you for your submission: {submission}"),
      smtp = list(host.name = "smtp.gmail.com", port = 465,
                  user.name = Sys.getenv("EMAIL_FROM"),
                  passwd = Sys.getenv("EMAIL_PASS"), ssl = TRUE),
      authenticate = TRUE,
      html = TRUE,
      send = TRUE)
    info("Thanks for your submission!")
  })

})
