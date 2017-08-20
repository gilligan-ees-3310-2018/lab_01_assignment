library(tidyverse)
library(stringr)

format_md = function(x, digits = NULL, scientific = NULL, comma = FALSE) {
  if (! is.null(digits)) x = signif(x, digits + 1)
  if (! is.null(scientific)) {
    if(str_to_lower(scientific) == 'auto') scientific = NULL
  }
  if (is.null(scientific)) {
    fixed = formatC(x, digits = digits, format = 'fg', flag = '#')
    sci = formatC(x, digits = digits, format = 'e')
    format = ifelse(str_length(fixed) > getOption('scipen') + str_length(sci), 'e', 'fg')
  }
  else {
    format = ifelse(scientific, 'e', 'fg')
  }
  mark = ifelse(comma, ',', '')

  formatC(x, digits = digits, format = format,
          flag = '#',
          big.mark = mark) %>%
    str_replace_all(c('\\+' = '', '[Ee](-?)0*([1-9][0-9]*)$' = '&times;10^\\1\\2^'))
}
