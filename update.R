# Document
devtools::document()

# Install
devtools::install('/Users/ssmeele/ownCloud/Simeon/MPI AB/Side projects/Methods paper/callsync')

# Test
library(devtools)
test()
test_coverage()

# Load
library(callsync)

# Test more
devtools::check()

library(rhub)
results <- rhub::check_for_cran()
results$cran_summary()

devtools::check_win_devel()
