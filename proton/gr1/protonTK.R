library(sqldf)
proton()
df <- employees

sqldf("SELECT * FROM df WHERE surname = 'Insecure'")
proton(action = "login", login = "johnins")

df_pass <- top1000passwords
for(n in 1:1000){
  proton(action = "login", login = "johnins", password = df_pass[n])
}

df_logs <- logs
sqldf("SELECT host, count(*) FROM df_logs WHERE login = 'slap' GROUP BY host ORDER BY 2 DESC")

proton(action = "server", host = "194.29.178.16")

df_bash <- bash_history
'% ' %in% df_bash
