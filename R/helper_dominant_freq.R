#' @import stats
.compute_dominant_freq = function(input, Fs, nth = 1){
  spectrum = .compute_fft(input, Fs)

  # if two peaks are too close, ignore
  peak_spectrum = spectrum
  peak_spectrum = peak_spectrum[order(peak_spectrum$VALUE, decreasing = TRUE),]
  dominant_freqs = c(0)
  dominant_intensity = c(0)
  i = 1
  while(length(dominant_freqs) < nth + 1) {
    while(any(abs(peak_spectrum[i, "FREQ"] - dominant_freqs) < 0.5)){
      i = i + 1
    }
    dominant_freqs = c(dominant_freqs, peak_spectrum[i, "FREQ"])
    dominant_intensity = c(dominant_intensity, peak_spectrum[i, "VALUE"])
  }
  dominant_freqs = data.frame(FREQ = dominant_freqs[-1], VALUE = dominant_intensity[-1])
  return(dominant_freqs)
}

#' @import stats
.compute_fft = function(input, Fs){
  N = nextn(length(input), factors = c(2))
  padded = c(input, rep(0, N - length(input)))
  spectrum = fft(padded)
  f = Fs/2*seq(0,1,length.out = N/2);
  spectrum = data.frame(FREQ = f, VALUE = abs(spectrum[1:(N/2)])/N)
  return(spectrum)
}
