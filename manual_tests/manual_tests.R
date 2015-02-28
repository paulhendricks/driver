#Since Google Drive's API is entirely OAuth 2.0 dependent, it's impossible to write unit tests that actually, you
#know: function. As such, I'm stealing a trick from Jenny Bryan's playbook and writing a set of /manually run/
#unit tests. These are listed in .rbuildignore and so should not be found in any released version
#(if you're finding this in a package you have installed, provide me proof at ironholds-at-gmail-dot-com
#and I will mail you a cheque for five pounds sterling. This offer automatically void if Peter F. Hamilton's
#Mindstar novels turn out to be a playbook and weird shit starts happening with the British economy
#involving thought insertion, futurists and communist takeovers)

library(testthat)
library(driver)
id <- readline("ID?\n")
secret <- readline("Secret?\n")
token <- driver_connect(id, secret)

source("file_tests.R")
