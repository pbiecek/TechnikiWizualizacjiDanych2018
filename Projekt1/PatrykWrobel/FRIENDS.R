library('rvest')
library('reshape')

prepareTranscript <- function(url, i) {
  destFile <- "temp.html"
  download.file(url, destfile = destFile, quiet=TRUE)
  webpage <- read_html(destFile)
  script_data_html <- html_nodes(webpage,'.postbody p')
  script_data <- html_text(script_data_html)  
  title_html <- html_nodes(webpage,".boxheading h2")
  title <- html_text(title_html)  
  title <- gsub("([0-9]) - ([0-9])","\\1-\\2",title)
  
  script_data_without_author <- gsub(".*?: ","",script_data)
  script_data_without_author <- gsub("\\[.*?\\]","",script_data_without_author)
  script_data_without_author <- gsub(".*\\(.*?\\)","",script_data_without_author)
  script_data_without_author <- gsub(".*<.*?>","",script_data_without_author)
  script_data_without_author <- gsub("--.*--","",script_data_without_author)
  script_data_without_author <- gsub("\\{.*\\}","",script_data_without_author)
  script_data_without_author <- gsub("<.*>","",script_data_without_author)
  script_data_without_author <- gsub(">.*<","",script_data_without_author)
  script_data_without_author <- gsub("\".*\"","",script_data_without_author)

  script_data_author <- gsub("\\[.*","",script_data)
  script_data_author <- gsub("\\(.*","",script_data_author)
  script_data_author <- gsub("\\*.*","",script_data_author)
  script_data_author <- gsub("\\{.*","",script_data_author)
  script_data_author <- gsub("<.*","",script_data_author)
  script_data_author <- gsub(">.*","",script_data_author)
  script_data_author <- gsub("\".*","",script_data_author)
  script_data_author <- gsub("([A-Za-z]+).*","\\1",script_data_author)

  script_data_wa <- script_data_without_author[script_data_without_author != "" & script_data_without_author != "." & script_data_author != ""]
  script_data_a <- script_data_author[script_data_without_author != "" & script_data_without_author != "." & script_data_author != ""]
  quotesWithAuthors <- mapply(c, Author = script_data_a, Quote = script_data_wa, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  data.frame(EpisodeNo = i, Episode = strsplit(title," - ")[[1]][1], Title =   strsplit(title," - ")[[1]][2], Author = script_data_a, Quote = script_data_wa)
}

transcriptsUrls <- c(
  # SEASON 1
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31373',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31374',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31375',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31376',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31377',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31378',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31379',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31380',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31394',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31381',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31395',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31396',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31382',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31383',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31384',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31385',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31386',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31387',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31388',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31389',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31390',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31391',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31392',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31393',
  # SEASON 2
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31397',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31398',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31399',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31400',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31401',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31402',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31403',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31404',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31405',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31406',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31407',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31408', #12-13
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31409',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31410',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31411',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31412',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31413',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31414',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31415',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31416',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31417',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31418',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31419',
  # SEASON 3
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31420',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31421',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31422',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31423',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31424',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31425',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31426',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31427',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31428',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31429',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31430',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31431',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31432',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31433',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31434',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31435',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31436',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31437',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31438',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31439',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31440',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31441',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31442',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31443',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31444',
  # SEASON 4
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31445',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31446',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31447',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31448',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31449',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31450',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31451',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31452',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31453',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31454',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31455',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31456',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31457',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31458',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31459',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31460',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31461',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31462',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31463',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31464',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31465',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31466',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31467', #23-24
  # SEASON 5
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31468',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31469',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31470',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31471',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31472',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31473',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31474',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31475',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31476',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31477',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31478',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31479',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31480',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31481',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31482',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31483',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31484',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31485',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31486',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31487',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31488',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31489',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31490', #23-24
  # SEASON 6
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31491',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31492',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31493',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31494',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31495',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31496',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31497',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31498',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31499',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31500',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31501',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31502',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31503',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31504',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31505', #15-16
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31506',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31507',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31508',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31509',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31510',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31511',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31512',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31513', #24-25
  #SEASON 7
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31514',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31515',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31516',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31517',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31518',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31519',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31520',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31521',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31522',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31523',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31524',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31525',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31526',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31527',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31528',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31529',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31530',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31531',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31532',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31533',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31534',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31535',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31536', #23-24
  # SEASON 8
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31537',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31538',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31539',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31540',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31541',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31542',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31543',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31544',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31545',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31546',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31547',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31548',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31549',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31550',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31551',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31552',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31553',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31554',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31555',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31556',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31557',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31558',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31559', #23-24
  # SEASON 9
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31560',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31561',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31562',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31563',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31564',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31565',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31566',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31567',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31568',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31569',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31570',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31571',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31572',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31573',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31574',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31575',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31576',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31577',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31578',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31579',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31580',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31581',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31582', #23-24
  # SEASON 10
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31583',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31584',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31585',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31586',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31587',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31588',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31589',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31590',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31591',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31592',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31593',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31594',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31595',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31596',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31597',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31598',
  'http://transcripts.foreverdreaming.org/viewtopic.php?f=845&t=31599'  #17-18
)

transcripts <- prepareTranscript(transcriptsUrls[1], 1)
for (i in 2:length(transcriptsUrls)) {
  transcripts <- rbind(transcripts, prepareTranscript(transcriptsUrls[i], i))
}

friendsNames <- c("CHANDLER","CHAN","Chandler","JOEY","Joey","MONICA","MNCA","Monica","PHOEBE","PHOE","Phoebe","RACHEL","RACH","Rachel","ROSS","Ross")
transcripts <- transcripts[transcripts$Author %in% friendsNames,]
transcripts$Author <- as.character(transcripts$Author)
transcripts$Quote <- as.character(transcripts$Quote)
transcripts[transcripts$Author == "CHAN" | transcripts$Author == "Chandler","Author"] <- "CHANDLER"
transcripts[transcripts$Author == "Joey","Author"] <- "JOEY"
transcripts[transcripts$Author == "MNCA" | transcripts$Author == "Monica","Author"] <- "MONICA"
transcripts[transcripts$Author == "PHOE" | transcripts$Author == "Phoebe","Author"] <- "PHOEBE"
transcripts[transcripts$Author == "RACH" | transcripts$Author == "Rachel","Author"] <- "RACHEL"
transcripts[transcripts$Author == "Ross","Author"] <- "ROSS"
friendsNames <- friendsNames[c(1,4,6,9,12,15)]

library("stringr")
transcripts$WordsCount <- str_count(transcripts$Quote," ") + 1

library(dplyr)
authorPerEpisode <- transcripts %>%
  select(Author,EpisodeNo,Episode,WordsCount) %>%
  group_by(EpisodeNo,Author) %>%
  summarise(Ep = unique(Episode),WordsCountPerEpisode = sum(WordsCount),QuotesCountPerEpisode = length(WordsCount))

authorPerSeason <- transcripts %>%
  select(Author,EpisodeNo,Episode,WordsCount) %>%
  group_by(Season=substr(Episode,1,2),Author) %>%
  summarise(WordsCountPerSeason = sum(WordsCount),QuotesCountPerSeason = length(WordsCount))

authorPerSeries <- transcripts %>%
  select(Author,WordsCount) %>%
  group_by(Author) %>%
  summarise(WordsCountPerSeries = sum(WordsCount),QuotesCountPerSeries = length(WordsCount))

ratings <- c(8.5,8.2,8.3,8.2,8.5,8.2,9.0,8.2,8.3,8.1,8.3,8.3,8.7,8.4,8.4,8.3,8.5,8.9,8.2,8.0,8.0,8.4,8.7,8.9,
             8.5,8.3,8.4,8.0,8.3,8.7,9.0,8.5,8.0,8.1,8.2,8.8,9.4,8.9,8.6,8.4,8.5,8.6,8.2,8.2,9.0,8.2,8.3,
             8.5,9.0,8.2,8.2,8.2,9.1,8.4,8.4,9.1,8.2,8.6,8.3,8.3,8.0,8.6,9.1,8.3,8.5,8.2,8.2,8.8,8.3,8.2,8.6,8.9,
             9.1,8.1,8.5,8.3,8.5,8.6,8.8,9.2,8.0,8.1,8.5,9.5,8.3,8.3,8.5,8.3,8.7,8.4,8.8,8.6,7.3,8.6,9.0,
             8.9,9.0,8.8,8.4,8.8,8.1,8.5,9.2,9.1,8.2,9.1,8.3,8.2,9.7,8.6,8.6,8.5,8.1,8.8,8.4,8.6,8.3,9.0,
             8.8,8.3,8.3,8.6,8.2,8.6,8.5,8.6,9.2,8.6,8.2,8.2,8.3,8.6,8.6,9.1,8.4,8.4,7.5,8.7,9.0,8.6,9.1,
             8.4,8.4,8.2,8.2,8.2,8.9,8.5,8.3,8.2,8.6,8.6,8.7,8.5,8.6,8.5,8.7,8.5,8.5,8.8,8.5,7.6,8.5,9.0,
             8.7,9.1,8.7,9.3,8.2,8.6,8.0,8.8,9.4,8.2,8.1,8.6,8.6,8.3,8.4,8.3,8.2,8.3,7.6,8.3,8.3,8.4,8.9,
             8.6,8.6,8.3,8.2,8.5,8.5,8.7,8.8,8.2,7.6,8.1,8.1,8.2,8.3,8.4,8.3,8.5,8.6,8.2,8.3,8.3,7.9,8.6,
             8.6,8.8,8.8,8.4,8.3,8.3,8.4,8.9,8.6,8.1,9.0,8.9,8.5,8.7,8.6,8.9,9.6)

USViewers <- c(21.5,20.2,19.5,19.7,18.6,18.2,23.5,21.1,23.1,19.9,26.6,24.0,25.8,23.8,24.8,26.1,30.5,30.4,29.4,30.0,28.4,29.9,28.7,31.3,
               32.1,29.8,30.2,28.1,28.3,30.2,30.5,32.9,27.8,32.2,31.6,52.9,33.6,32.9,31.1,30.2,30.1,31.2,27.4,24.7,25.5,26.1,29.0,
               26.8,26.7,25.2,26.1,23.3,23.3,27.4,28.7,29.3,25.1,29.8,29.6,28.0,28.9,27.3,28.3,25.8,28.1,23.7,24.4,23.2,22.6,24.2,23.1,28.8,
               29.4,25.5,24.0,24.3,24.4,25.7,26.4,26.8,23.9,23.2,23.7,27.1,25.3,25.1,24.4,23.1,23.2,21.7,21.8,21.9,21.5,23.2,31.6,
               31.1,25.4,26.8,24.1,25.9,25.0,24.4,23.9,23.0,23.7,27.0,24.8,24.9,27.7,29.3,26.0,24.5,21.9,20.9,19.6,20.9,21.3,25.9,
               27.7,22.9,21.6,21.1,22.4,23.6,22.7,22.1,19.2,22.4,22.3,22.3,24.1,23.8,20.7,22.1,20.5,21.5,18.8,20.6,20.0,20.9,30.7,
               25.54,27.93,22.70,22.66,24.43,22.01,23.73,16.57,21.08,23.26,24.37,22.86,22.24,22.40,21.75,21.22,20.84,17.81,16.55,16.30,15.65,17.23,30.05,
               31.70,30.04,29.20,25.58,25.64,26.96,24.24,26.54,24.24,22.44,23.85,25.53,29.24,28.64,28.64,27.52,26.30,22.05,22.59,22.24,23.97,24.32,34.91,
               34.01,28.93,26.63,25.82,24.46,27.51,25.35,26.76,25.43,22.29,23.67,23.66,25.82,23.37,20.85,19.52,21.00,20.79,18.25,20.71,19.03,19.55,25.46,
               24.54,22.38,21.87,18.77,19.37,20.38,20.21,20.66,25.49,26.68,24.91,25.90,24.27,22.83,22.64,24.51,52.46
)

stats <- data.frame(EpisodeNo = unique(transcripts$EpisodeNo), Season = as.numeric(substr(unique(transcripts$Episode),1,2)), Rating = ratings, USViewers = USViewers)
stats$ChandlerWords <- unlist(authorPerEpisode[authorPerEpisode$Author == "CHANDLER","WordsCountPerEpisode"])
stats$JoeyWords <- unlist(authorPerEpisode[authorPerEpisode$Author == "JOEY","WordsCountPerEpisode"])
stats$MonicaWords <- unlist(authorPerEpisode[authorPerEpisode$Author == "MONICA","WordsCountPerEpisode"])
stats$PhoebeWords <- unlist(authorPerEpisode[authorPerEpisode$Author == "PHOEBE","WordsCountPerEpisode"])
stats$RachelWords <- unlist(authorPerEpisode[authorPerEpisode$Author == "RACHEL","WordsCountPerEpisode"])
stats$RossWords <- unlist(authorPerEpisode[authorPerEpisode$Author == "ROSS","WordsCountPerEpisode"])
stats$ChandlerQuotes <- unlist(authorPerEpisode[authorPerEpisode$Author == "CHANDLER","QuotesCountPerEpisode"])
stats$JoeyQuotes <- unlist(authorPerEpisode[authorPerEpisode$Author == "JOEY","QuotesCountPerEpisode"])
stats$MonicaQuotes <- unlist(authorPerEpisode[authorPerEpisode$Author == "MONICA","QuotesCountPerEpisode"])
stats$PhoebeQuotes <- unlist(authorPerEpisode[authorPerEpisode$Author == "PHOEBE","QuotesCountPerEpisode"])
stats$RachelQuotes <- unlist(authorPerEpisode[authorPerEpisode$Author == "RACHEL","QuotesCountPerEpisode"])
stats$RossQuotes <- unlist(authorPerEpisode[authorPerEpisode$Author == "ROSS","QuotesCountPerEpisode"])

authorPerEpisode$USViewers <- rep(USViewers,each=6)
authorPerEpisode$Rating <- rep(ratings, each=6)
authorPerEpisode$OddSeason <- as.numeric(substr(authorPerEpisode$Ep,1,2)) %% 2 == 0

USViewersPerSeason <- c(24.79,30.8,26.31,24.66,24.70,22.35,21.70,26.36,23.86,24.58)
ratingingPerSeason <- c(8.4,8.48,8.49,8.52,8.66,8.54,8.47,8.49,8.34,8.67)
authorPerSeason$USViewers <- rep(USViewersPerSeason,each=6)
authorPerSeason$Rating <- rep(ratingingPerSeason, each=6)

library('ggplot2')
ggplot(data = stats, mapping = aes(EpisodeNo,USViewers)) + geom_bar(stat = "identity")
ggplot(data = stats, mapping = aes(EpisodeNo,Rating)) + geom_bar(stat = "identity")

ggplot(data = authorPerSeason, mapping = aes(Season,USViewers,group=1)) + geom_line() + geom_point()

ggplot(data = authorPerEpisode, mapping = aes(EpisodeNo,WordsCountPerEpisode,color=Author)) + geom_line() + geom_point()
ggplot(data = authorPerSeason, mapping = aes(Season,WordsCountPerSeason,color=Author,group=Author)) + geom_line() + geom_point()
ggplot(data = authorPerEpisode, mapping = aes(EpisodeNo,WordsCountPerEpisode,color=Author)) + geom_bar(stat="identity")

ggplot(data = authorPerEpisode, mapping = aes(EpisodeNo,QuotesCountPerEpisode,color=Author)) + geom_line() + geom_point()
ggplot(data = authorPerSeason, mapping = aes(Season,QuotesCountPerSeason,color=Author,group=Author)) + geom_line() + geom_point()

chandlerPlot <- ggplot(data = authorPerEpisode[authorPerEpisode$Author=="CHANDLER",], mapping = aes(EpisodeNo,WordsCountPerEpisode,fill=OddSeason)) + 
  geom_bar(stat="identity") + 
  geom_line(mapping = aes(EpisodeNo,USViewers*8),inherit.aes = FALSE,color="#392613",size=1) +
  scale_y_continuous(sec.axis = sec_axis(~./8,name = "Widzowie (mln)")) +
  labs(y = "Liczba s³ów") +
  guides(fill=FALSE) +
  theme(axis.title.y = element_text(face="bold"),axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y.left = element_text(margin = margin(r=-40)), axis.text.y.right = element_text(margin = margin(l=-40))) +
  scale_fill_manual(values = c("#9a99a1", "#5d6363"))
rossPlot <- ggplot(data = authorPerEpisode[authorPerEpisode$Author=="ROSS",], mapping = aes(EpisodeNo,WordsCountPerEpisode,fill=OddSeason)) + 
  geom_bar(stat="identity") + 
  geom_line(mapping = aes(EpisodeNo,USViewers*8),inherit.aes = FALSE,color="#392613",size=1) +
  scale_y_continuous(sec.axis = sec_axis(~./8,name = "Widzowie (mln)")) +
  labs(y = "Liczba s³ów") +
  guides(fill=FALSE)  +
  theme(axis.title.y = element_text(face="bold"),axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y.left = element_text(margin = margin(r=-40)), axis.text.y.right = element_text(margin = margin(l=-40))) +
  scale_fill_manual(values = c("#00aaff", "#66ccff"))
joeyPlot <- ggplot(data = authorPerEpisode[authorPerEpisode$Author=="JOEY",], mapping = aes(EpisodeNo,WordsCountPerEpisode,fill=OddSeason)) + 
  geom_bar(stat="identity") + 
  geom_line(mapping = aes(EpisodeNo,USViewers*8),inherit.aes = FALSE,color="#392613",size=1) +
  scale_y_continuous(sec.axis = sec_axis(~./8,name = "Widzowie (mln)")) +
  labs(y = "Liczba s³ów") +
  guides(fill=FALSE)  +
  scale_fill_manual(values = c("#9a99a1", "#5d6363")) +
  theme(axis.title.y = element_text(face="bold"),axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y.left = element_text(margin = margin(r=-40)), axis.text.y.right = element_text(margin = margin(l=-40)))
phoebePlot <- ggplot(data = authorPerEpisode[authorPerEpisode$Author=="PHOEBE",], mapping = aes(EpisodeNo,WordsCountPerEpisode,fill=OddSeason)) + 
  geom_bar(stat="identity") + 
  geom_line(mapping = aes(EpisodeNo,USViewers*8),inherit.aes = FALSE,color="#392613",size=1) +
  scale_y_continuous(sec.axis = sec_axis(~./8,name = "Widzowie (mln)")) +
  labs(y = "Liczba s³ów") +
  guides(fill=FALSE)  +
  theme(axis.title.y = element_text(face="bold"),axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y.left = element_text(margin = margin(r=-40)), axis.text.y.right = element_text(margin = margin(l=-40))) +
  scale_fill_manual(values = c("#00aaff", "#66ccff"))
monicaPlot <- ggplot(data = authorPerEpisode[authorPerEpisode$Author=="MONICA",], mapping = aes(EpisodeNo,WordsCountPerEpisode,fill=OddSeason)) + 
  geom_bar(stat="identity") + 
  geom_line(mapping = aes(EpisodeNo,USViewers*8),inherit.aes = FALSE,color="#392613",size=1) +
  scale_y_continuous(sec.axis = sec_axis(~./8,name = "Widzowie (mln)")) +
  labs(y = "Liczba s³ów") +
  guides(fill=FALSE)  +
  theme(axis.title.y = element_text(face="bold"),axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y.left = element_text(margin = margin(r=-40)), axis.text.y.right = element_text(margin = margin(l=-40))) +
  scale_fill_manual(values = c("#9a99a1", "#5d6363"))
rachelPlot <- ggplot(data = authorPerEpisode[authorPerEpisode$Author=="RACHEL",], mapping = aes(EpisodeNo,WordsCountPerEpisode,fill=OddSeason)) + 
  geom_bar(stat="identity") + 
  geom_line(mapping = aes(EpisodeNo,USViewers*8),inherit.aes = FALSE,color="#392613",size=1) +
  scale_y_continuous(sec.axis = sec_axis(~./8,name = "Widzowie (mln)")) +
  labs(y = "Liczba s³ów") +
  guides(fill=FALSE)   +
  theme(axis.title.y = element_text(face="bold"),axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y.left = element_text(margin = margin(r=-40)), axis.text.y.right = element_text(margin = margin(l=-40))) +
  scale_fill_manual(values = c("#00aaff", "#66ccff"))

library(gridExtra)
grid.arrange(phoebePlot,monicaPlot,rachelPlot, ncol = 1)
grid.arrange(chandlerPlot, rossPlot,joeyPlot, ncol = 1)

                                                                                                                                                                         