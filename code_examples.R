# align
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/chunk@1@1@1@1.wav'
file_2 = '/chunk@2@1@1@1.wav'
url_1 = paste0(path_git, path_repo, file_1)
url_2 = paste0(path_git, path_repo, file_2)
local_file_1 = paste(tempdir(), file_1, sep = '/')
local_file_2 = paste(tempdir(), file_2, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
if(!file.exists(local_file_2))
  download.file(url_2, destfile = local_file_2, mode = 'wb')
all_files = c(local_file_1, local_file_2)
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
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = readWave(local_file_1)
better.spectro(wave)

# calc.am
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = readWave(local_file_1)
result = calc.am(wave)

# calc.fm
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = readWave(local_file_1)
trace = trace.fund(wave)
result = calc.fm(trace$fund)

# calc.perf
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/audacity/chunk_15_ground_truth.txt'
url_1 = paste0(path_git, path_repo, file_1)
local_dir = paste(tempdir(), 'audacity', sep = '/')
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!dir.exists(local_dir)) dir.create(local_dir)
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
d = load.selection.tables.audacity(path_selection_tables = local_dir)
result = calc.perf(d, d)

# call.assign
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/chunk@1@1@1@1.wav'
file_2 = '/chunk@2@1@1@1.wav'
url_1 = paste0(path_git, path_repo, file_1)
url_2 = paste0(path_git, path_repo, file_2)
local_file_1 = paste(tempdir(), file_1, sep = '/')
local_file_2 = paste(tempdir(), file_2, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
if(!file.exists(local_file_2))
  download.file(url_2, destfile = local_file_2, mode = 'wb')
all_files = c(local_file_1, local_file_2)
detections = lapply(all_files, function(file){
  wave = load.wave(file, ffilter_from = 1100)
  detections = call.detect.multiple(wave, plot_it = FALSE)
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
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = readWave(local_file_1)
cd = call.detect.multiple(wave)

# call.detect
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = readWave(local_file_1)
cd = call.detect(wave)

# create.spec.object
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = readWave(local_file_1)
result = create.spec.object(wave, plot_it = FALSE)

# detect.and.assign
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/chunk@1@1@1@1.wav'
file_2 = '/chunk@2@1@1@1.wav'
url_1 = paste0(path_git, path_repo, file_1)
url_2 = paste0(path_git, path_repo, file_2)
local_file_1 = paste(tempdir(), file_1, sep = '/')
local_file_2 = paste(tempdir(), file_2, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
if(!file.exists(local_file_2))
  download.file(url_2, destfile = local_file_2, mode = 'wb')
all_files = c(local_file_1, local_file_2)
result = detect.and.assign(all_files = all_files,
                           quiet = TRUE,
                           save_files = FALSE)

# load.selection.table
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/2020_10_27_091634.Table.1.selections.txt'
file_2 = '/2020_10_27_132148.Table.1.selections.txt'
url_1 = paste0(path_git, path_repo, file_1)
url_2 = paste0(path_git, path_repo, file_2)
local_file_1 = paste(tempdir(), file_1, sep = '/')
local_file_2 = paste(tempdir(), file_2, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
if(!file.exists(local_file_2))
  download.file(url_2, destfile = local_file_2, mode = 'wb')
st = load.selection.tables(path_selection_tables = tempdir())

# load.selection.tables.audacity
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/audacity/chunk_15_ground_truth.txt'
url_1 = paste0(path_git, path_repo, file_1)
local_dir = paste(tempdir(), 'audacity', sep = '/')
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!dir.exists(local_dir)) dir.create(local_dir)
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
st = load.selection.tables.audacity(path_selection_tables = local_dir)

# load.wave
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = load.wave(local_file_1)

# measure.trace.multiple
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
file_2 = '/wave_2.wav'
url_1 = paste0(path_git, path_repo, file_1)
url_2 = paste0(path_git, path_repo, file_2)
local_file_1 = paste(tempdir(), file_1, sep = '/')
local_file_2 = paste(tempdir(), file_2, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
if(!file.exists(local_file_2))
  download.file(url_2, destfile = local_file_2, mode = 'wb')
all_files = c(local_file_1, local_file_2)
waves = lapply(all_files, load.wave)
new_waves = waves
detections = lapply(waves, call.detect)
traces = lapply(waves, trace.fund)
mt = measure.trace.multiple(traces = traces, waves = waves,
                            new_waves = new_waves, detections = detections)

# measure.trace
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = readWave(local_file_1)
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
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
file_2 = '/wave_2.wav'
url_1 = paste0(path_git, path_repo, file_1)
url_2 = paste0(path_git, path_repo, file_2)
local_file_1 = paste(tempdir(), file_1, sep = '/')
local_file_2 = paste(tempdir(), file_2, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
if(!file.exists(local_file_2))
  download.file(url_2, destfile = local_file_2, mode = 'wb')
all_files = c(local_file_1, local_file_2)
waves = lapply(all_files, load.wave)
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
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
file_2 = '/wave_2.wav'
url_1 = paste0(path_git, path_repo, file_1)
url_2 = paste0(path_git, path_repo, file_2)
local_file_1 = paste(tempdir(), file_1, sep = '/')
local_file_2 = paste(tempdir(), file_2, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
if(!file.exists(local_file_2))
  download.file(url_2, destfile = local_file_2, mode = 'wb')
wave_1 = readWave(local_file_1)
wave_2 = readWave(local_file_2)
so_1 = create.spec.object(wave = wave_1, plot_it = FALSE)
so_2 = create.spec.object(wave = wave_2, plot_it = FALSE)
out = sliding.pixel.comparison(so_1, so_2)

# trace.fund
require(callsync)
require(seewave)
require(tuneR)
path_git = 'https://raw.githubusercontent.com'
path_repo = '/simeonqs/callsync/master/tests/testthat/files'
file_1 = '/wave_1.wav'
url_1 = paste0(path_git, path_repo, file_1)
local_file_1 = paste(tempdir(), file_1, sep = '/')
if(!file.exists(local_file_1))
  download.file(url_1, destfile = local_file_1, mode = 'wb',)
wave = readWave(local_file_1)
trace = trace.fund(wave)

