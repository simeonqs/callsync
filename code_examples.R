# align
require(callsync)
require(seewave)
require(tuneR)
files = system.file("extdata", "", package = "callsync")
all_files = list.files(files, '*chunk*', full.names = T)
a = align(chunk_size = 2,
          step_size = 0.1,
          all_files = all_files,
          keys_id = c('c', '$'),
          keys_rec = c('c', '$'),
          blank = 0,
          wing = 0,
          quiet = TRUE)

# better.spectro
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "wave_1.wav", package = "callsync")
wave = readWave(file)
better.spectro(wave)

# calc.am
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "wave_1.wav", package = "callsync")
wave = readWave(file)
result = calc.am(wave)

# calc.fm
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "wave_1.wav", package = "callsync")
wave = readWave(file)
trace = trace.fund(wave)
result = calc.fm(trace$fund)

# calc.perf
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata/audacity", "", package = "callsync")
d = load.selection.tables.audacity(path_selection_tables = file)
result = calc.perf(d, d)
