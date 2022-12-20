# Document
devtools::document()

# Install
devtools::install('/Users/ssmeele/ownCloud/Simeon/MPI AB/Side projects/Methods paper/callsync')

# Test
devtools::test()
devtools::test_coverage()

# Load
library(callsync)

# Test more
devtools::check()

# Add news
usethis::use_news_md()

# CRAN
library(rhub)
results <- rhub::check_for_cran()
results$cran_summary()
usethis::use_cran_comments()

devtools::check_win_devel()

devtools::spell_check()
devtools::release()

# Good practice
library(goodpractice)
goodpractice::gp()

