#' send email in R.
#'
#' This function uses the `mailR` package to send an email from R. I normally use this function at the end of some analysis, where the body is the end result.
#' @param email.title Email subject.
#' @param from Email address being sent from. This will need to be configured to allow for emails to be sent from third parties.
#' @param to Email adress to send to.
#' @param body Body of email. Can be images.
#' @param from.passwd The password to the `from` email address.
#' @param host.name The host of the `from` email. The default here is for  Google email address.
#' @param host.port The host port. Default is 465.
#' @export
#' @examples
#' sendEmailFUN()


sendEmailFUN <- function(email.title, from, to, body, from.passwd, host.name = 'smtp.gmail.com', host.port = 465){
  
  # load packages
  library(mailR)

  send.mail(from = from,
            to = to,
            subject = email.title,
            body = body,
            smtp = list(host.name = host.name, port = host.port, user.name = from, passwd = from.passwd, ssl = T),
            authenticate = T, 
            send = T)
}	
