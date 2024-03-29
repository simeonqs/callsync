# Document
devtools::document()

# Install
devtools::install('/home/au472091/OneDrive/au/projects/callsync')

# Test
devtools::test()
devtools::test_coverage()

# Load
library(callsync)
citation('callsync')

# Test more
devtools::check()
devtools::check_win_devel()
devtools::spell_check()

# CRAN
results <- rhub::check_for_cran()
results$cran_summary()

# Good practice
goodpractice::gp()

# Final step before submission
devtools::release()
