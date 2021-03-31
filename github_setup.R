install.packages("usethis")
library(usethis)


edit_git_config()

create_github_token()



install.packages("gitcreds")
library("gitcreds")

gitcreds_set()

# make a repo
 use_git()

#send to github
use_github(organisation = "rin3-spring-2021", private = TRUE)

#adding more code to test new commit