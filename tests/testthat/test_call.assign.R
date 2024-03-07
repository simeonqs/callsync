all_files = list.files('files', '*chunk*', full.names = T)

detections = lapply(all_files, function(file){
  wave = load.wave(file, ffilter_from = 1100)
  detections = call.detect.multiple(wave, plot_it = F)
  return(detections)
})
names(detections) = basename(all_files)

test_that('Finding files.', {

  expect_true(length(all_files) != 0)

})

# test_that('Class and end > start.', {
#
#   ca = call.assign(all_files = all_files,
#                    detections = detections,
#                    quiet = TRUE,
#                    save_files = FALSE)
#
#   expect_true(class(ca) == 'data.frame')
#   expect_true(all(ca$end > ca$start))
#
# })

# test_that('Save files, a is null.', {
#
#   td = tempdir()
#
#   ca = call.assign(all_files = all_files,
#                    detections = detections,
#                    quiet = TRUE,
#                    save_files = TRUE,
#                    path_calls = td)
#
#   expect_null(ca)
#
#   unlink(td)
#
# })

