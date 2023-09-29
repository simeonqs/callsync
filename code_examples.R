# align
require(callsync)
require(seewave)
require(tuneR)
files = system.file("extdata", "", package = "callsync")
all_files = list.files(files, '*chunk*', full.names = T)
a = align(chunk_size = 2,
          step_size = 0.1,
          all_files = all_files,
          keys_id = c('c', '@'),
          keys_rec = c('c', '@'),
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

# call.assign
require(callsync)
require(seewave)
require(tuneR)
files = system.file("extdata", "", package = "callsync")
all_files = list.files(files, '*chunk*', full.names = T)
detections = lapply(all_files, function(file){
  wave = load.wave(file, ffilter_from = 1100)
  detections = call.detect.multiple(wave, plot_it = F)
  return(detections)
})
names(detections) = basename(all_files)
ca = call.assign(all_files = all_files,
                 detections = detections,
                 quiet = TRUE,
                 save_files = FALSE)

# call.detect.multiple
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "wave_1.wav", package = "callsync")
wave = readWave(file)
cd = call.detect.multiple(wave)

# call.detect
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "wave_1.wav", package = "callsync")
wave = readWave(file)
cd = call.detect(wave)

# create.spec.object
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "wave_1.wav", package = "callsync")
wave = readWave(file)
result = create.spec.object(wave, plot_it = FALSE)

# detect.and.assign
require(callsync)
require(seewave)
require(tuneR)
files = system.file("extdata", "", package = "callsync")
all_files = list.files(files, "*chunk*", full.names = T)
result = detect.and.assign(all_files = all_files,
                           quiet = TRUE,
                           save_files = FALSE)

# load.selection.table
require(callsync)
require(seewave)
require(tuneR)
files = system.file("extdata", "", package = "callsync")
st = load.selection.tables(path_selection_tables = files)

# load.selection.tables.audacity
require(callsync)
require(seewave)
require(tuneR)
files = system.file("extdata", "audacity", package = "callsync")
st = load.selection.tables.audacity(path_selection_tables = files)

# load.wave
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "wave_1.wav", package = "callsync")
wave = load.wave(file)

# measure.trace.multiple
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "", package = "callsync")
files = list.files(file, pattern = "wave_*", full.names = TRUE)
waves = lapply(files, load.wave)
new_waves = waves
detections = lapply(waves, call.detect)
traces = lapply(waves, trace.fund)
mt = measure.trace.multiple(traces = traces, waves = waves,
                            new_waves = new_waves, detections = detections)

# measure.trace
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "wave_1.wav", package = "callsync")
wave = readWave(file)
trace = trace.fund(wave)
result = measure.trace(trace)

# o.to.m
m = matrix(1:9, nrow = 3, ncol = 3)
o = m[lower.tri(m)]
m_new = o.to.m(o)

# run.spcc
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "", package = "callsync")
files = list.files(file, "wave_*", full.names = T)
waves = lapply(files, load.wave)
spcc_out = run.spcc(waves)

# simple.cc
require(callsync)
require(seewave)
require(tuneR)
s1 = c(0, 0, 0, 1, 1, 2, 0)
s2 = c(0, 0, 2, 2, 3, 0, 0, 0, 0)
offset = simple.cc(s1, s2) # -1
index_s1 = seq(1, length(s1)) + offset # align
plot(s2, type = 'b')
points(index_s1, s1, col = 2, type = 'b')

# sliding.pixel.comparison
require(callsync)
require(seewave)
require(tuneR)
file_1 = system.file("extdata", "wave_1.wav", package = "callsync")
file_2 = system.file("extdata", "wave_2.wav", package = "callsync")
wave_1 = readWave(file_1)
wave_2 = readWave(file_2)
so_1 = create.spec.object(wave = wave_1, plot_it = FALSE)
so_2 = create.spec.object(wave = wave_2, plot_it = FALSE)
out = sliding.pixel.comparison(so_1, so_2)

# trace.fund
require(callsync)
require(seewave)
require(tuneR)
file = system.file("extdata", "wave_1.wav", package = "callsync")
wave = readWave(file)
trace = trace.fund(wave)

