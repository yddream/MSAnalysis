ReadMGF <- function(fn) {
  # cat('reading file --', file, '\n')
  mgf.data.list <- ListMGF(fn)
  info.spec <- lapply(mgf.data.list, function(mgf.data) {
    nl.spec <- grep('^\\d', mgf.data)
    if (length(nl.spec) == 0) {
      return(NA)
    }
    info.mz <- grep('^PEPMASS', mgf.data, value = T)
    info.rt <- grep('^RTINSECONDS', mgf.data, value = T)
    # info.z <- grep('^CHARGE', mgf.data, value = T)
    info.precursor <- as.numeric(gsub('\\w+=', '', c(info.mz,info.rt)))
    names(info.precursor) <- c('mz', 'rt')

    spec <- data.frame(do.call(rbind,
                               strsplit(mgf.data[nl.spec],
                                        split = '\t|\\s')),
                       stringsAsFactors = F)
    names(spec) <- c('mz', 'intensity')
    spec$mz <- as.numeric(spec$mz)
    spec$intensity <- as.numeric(spec$intensity)
    list('info' = info.precursor,
         'spec' = spec)
  })
  info.spec <- info.spec[!is.na(info.spec)]
  return(info.spec)
}

ListMGF <- function(fn) {
  mgf.data <- readLines(fn)
  nl.rec.new <- 1
  idx.rec <- 1
  rec.list <- list()
  for(nl in 1:length(mgf.data))
  {
    if(mgf.data[nl]=="END IONS")
    {
      rec.list[idx.rec] <- list(Compound = mgf.data[nl.rec.new : nl])
      nl.rec.new <- nl + 1
      idx.rec <- idx.rec + 1
    }
  }
  rec.list
}

CheckInRange <- function(targets, range) {
  targets >= range[1] & targets <= range[2]
}
