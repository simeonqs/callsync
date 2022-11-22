test_that('Class.', {

  wave = load.wave(path_audio_file = 'files/wave_1.wav')

  expect_true(as.character(class(wave)) == 'Wave')

})

