
library(stringi)

polskieznaki <- function(text){

  text <- stri_replace_all_regex(text, '\u00c5\u0082', 'ł')
  text <- stri_replace_all_regex(text, '\u00c4\u0099', 'ę')
  text <- stri_replace_all_regex(text, '\u00c4\u0085', 'ą')
  text <- stri_replace_all_regex(text, '\u00c5\u009b', 'ś')
  text <- stri_replace_all_regex(text, '\u00c4\u0087', 'ć')
  text <- stri_replace_all_regex(text, '\u00c5\u0084', 'ń')
  text <- stri_replace_all_regex(text, '\u00c5\u00ba', 'ź')
  text <- stri_replace_all_regex(text, '\u00c5\u00bc', 'ż')
  text <- stri_replace_all_regex(text, '\u00c3\u00b3', 'ó')
  
  text <- stri_replace_all_regex(text, '\u00c5\u0081', 'Ł')
  text <- stri_replace_all_regex(text, '\u00c4\u0098', 'Ę')
  text <- stri_replace_all_regex(text, '\u00c4\u0084', 'Ą')
  text <- stri_replace_all_regex(text, '\u00c5\u009a', 'Ś')
  text <- stri_replace_all_regex(text, '\u00c4\u0086', 'Ć')
  text <- stri_replace_all_regex(text, '\u00c5\u0083', 'Ń')
  text <- stri_replace_all_regex(text, '\u00c5\u00b9', 'Ź')
  text <- stri_replace_all_regex(text, '\u00c5\u00bb', 'Ż')
  text <- stri_replace_all_regex(text, '\u00c3\u009', 'Ó')
  
  flag <- 1
  
  if(flag == 1){
    
    text <- stri_replace_all_regex(text, 'ł', 'l')
    text <- stri_replace_all_regex(text, 'ę', 'e')
    text <- stri_replace_all_regex(text, 'ą', 'a')
    text <- stri_replace_all_regex(text, 'ś', 's')
    text <- stri_replace_all_regex(text, 'ć', 'c')
    text <- stri_replace_all_regex(text, 'ń', 'n')
    text <- stri_replace_all_regex(text, 'ź', 'z')
    text <- stri_replace_all_regex(text, 'ż', 'z')
    text <- stri_replace_all_regex(text, 'ó', 'o')
    
    text <- stri_replace_all_regex(text, 'Ł', 'L')
    text <- stri_replace_all_regex(text, 'Ę', 'E')
    text <- stri_replace_all_regex(text, 'Ą', 'A')
    text <- stri_replace_all_regex(text, 'Ś', 'S')
    text <- stri_replace_all_regex(text, 'Ć', 'C')
    text <- stri_replace_all_regex(text, 'Ń', 'N')
    text <- stri_replace_all_regex(text, 'Ź', 'Z')
    text <- stri_replace_all_regex(text, 'Ż', 'Z')
    text <- stri_replace_all_regex(text, 'Ó', 'O')
    
  }
  return(text)
}


polskieznaki_slownik <- function(text){
  text <- stri_replace_all_regex(text, 'ł', 'l')
  text <- stri_replace_all_regex(text, 'ę', 'e')
  text <- stri_replace_all_regex(text, 'ą', 'a')
  text <- stri_replace_all_regex(text, 'ś', 's')
  text <- stri_replace_all_regex(text, 'ć', 'c')
  text <- stri_replace_all_regex(text, 'ń', 'n')
  text <- stri_replace_all_regex(text, 'ź', 'z')
  text <- stri_replace_all_regex(text, 'ż', 'z')
  text <- stri_replace_all_regex(text, 'ó', 'o')
  
  text <- stri_replace_all_regex(text, 'Ł', 'L')
  text <- stri_replace_all_regex(text, 'Ę', 'E')
  text <- stri_replace_all_regex(text, 'Ą', 'A')
  text <- stri_replace_all_regex(text, 'Ś', 'S')
  text <- stri_replace_all_regex(text, 'Ć', 'C')
  text <- stri_replace_all_regex(text, 'Ń', 'N')
  text <- stri_replace_all_regex(text, 'Ź', 'Z')
  text <- stri_replace_all_regex(text, 'Ż', 'Z')
  text <- stri_replace_all_regex(text, 'Ó', 'O')
}
