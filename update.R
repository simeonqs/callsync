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

# CRAN
results <- rhub::check_for_cran()
results$cran_summary()
usethis::use_cran_comments()

devtools::check_win_devel()

devtools::spell_check()

# Good practice
goodpractice::gp()

# Final step before submission
devtools::release()
