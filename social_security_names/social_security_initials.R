library(data.table)
library(iotools)
library(RColorBrewer)
library(funchir)

#files from http://ssdmf.info/
#  each file roughly 3 gigabytes (about 100 million rows total)
ff = list.files('/media/data_drive/social_security_death_index',
                full.names = TRUE)
#other columns are included but ignored for purposes here
widths = c(10L, 1L, 23L, 1L, 42, 4L)
coltypes = c(rep('character', 5L), 'integer')
#see: http://stackoverflow.com/a/34190156/3576984
#  (still takes a few minutes on my machine)
ss_names = rbindlist(lapply(ff, function(x) 
  input.file(x, formatter = dstrfw,
             col_types = coltypes, widths = widths)))
ss_names[ , c(1, 3, 5) := NULL]
setnames(ss_names, c('first', 'last', 'birth'))
setkey(ss_names, first, last)

#data noise. didn't check whether
#  I messed up file input or what...
#  only birth > 0 restricts a noteworthy
#  number of rows (~ 100k)
ss_names = ss_names[grepl('[A-Z]', first) & 
                      grepl('[A-Z]', last) & birth > 0]

#for easier sharing without need to download 10GB
fwrite(ss_names[ , .N, by = .(first, last)],
       'social_security_tabulated.csv')
fwrite(ss_names[ , .N, by = .(first, last, birth)
                 ][CJ(first = first, last = last, birth = birth, 
                      unique = TRUE, sorted = TRUE), 
                   on = c('first', 'last', 'birth')
                   ][is.na(N), N := 0L],
       'social_security_tabulated_by_year.csv')

#a static plot
png('initial_heat.png',
    #bump up that rezzy
    width = 8, height = 8,
    units = 'in', res = 1200)
#see http://stackoverflow.com/questions/32063017/
par(mfrow = c(2L, 2L),
    mar = c(0, 0, 1.1, 0),
    oma = c(5.1, 4.1, 4.1, 2.1))
#see http://stackoverflow.com/a/11301350/3576984
idx1 = seq(1, 26, by = 2)
idx2 = seq(2, 26, by = 2)
axparams = 
  list(x = list(at = idx1, labels = LETTERS[idx1]),
       y = list(at = 1:26, labels = LETTERS, las = 1))
X = dcast(ss_names, first ~ last, 
          value.var = 'birth', fun.aggregate = length)

image(1:26, 1:26, as.matrix(X[ , !"first"]),
      xaxt = 'n', xlab = 'First',
      yaxt = 'n', ylab = 'Last',
      main = 'Relative Frequency (All Years)',
      col = colorRampPalette(brewer.pal(9, 'Reds'))(100))
tile.axes(1, 2, 2, axparams)
mtext(side = 2, 'Last Initial', line = 2)

#reciprocal so Blue is lowest... tried just
#  doing -log, but it came out looking different?
image(1:26, 1:26, log2(1/as.matrix(X[ , !"first"])),
      xaxt = 'n', xlab = 'First',
      yaxt = 'n', ylab = 'Last',
      main = 'Log-Relative Frequency (All Years)',
      col = colorRampPalette(brewer.pal(9, 'Blues'))(100))
tile.axes(2, 2, 2, axparams)

X = dcast(ss_names[birth >= 1992], first ~ last, 
          #fill with small positive number, otherwise
          #  the log stands out too much
          value.var = 'birth', fill = .1,
          #force numeric otherwise the fill gets ignored
          fun.aggregate = function(x) length(x)+0)
image(1:26, 1:26, as.matrix(X[ , !"first"]),
      xaxt = 'n', xlab = 'First',
      yaxt = 'n', ylab = 'Last',
      main = 'Relative Frequency (1992 - )',
      col = colorRampPalette(brewer.pal(9, 'Reds'))(100))
tile.axes(3, 2, 2, axparams)
axis(side = 1, idx2, LETTERS[idx2])
mtext(side = 1, 'First Initial', line = 2)
mtext(side = 2, 'Last Initial', line = 2)

image(1:26, 1:26, log2(1/as.matrix(X[ , !"first"])),
      xaxt = 'n', xlab = 'First',
      yaxt = 'n', ylab = 'Last',
      main = 'Log-Relative Frequency (1992 - )',
      col = colorRampPalette(brewer.pal(9, 'Blues'))(100))
tile.axes(4, 2, 2, axparams)
axis(side = 1, idx2, LETTERS[idx2])
mtext(side = 1, 'First Initial', line = 2)
title('Most Common Initials\nUS Social Security Death Master File Data',
      outer = TRUE)
dev.off()
