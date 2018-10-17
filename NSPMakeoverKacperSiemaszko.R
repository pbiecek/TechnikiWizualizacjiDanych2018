
data <- data.frame(age=character(), sex=character(), health=character(), fraction=double())
data <- as.data.frame(cbind(rep(c("0-14","15-29","30-44","45-59","60+"),6)) %>%
                cbind("Kobieta") %>%
                cbind(rep(c("bardzo dobry", "dobry", "neutralny", "zły", "bardzo zły", "nieustalony"), each=5)) %>%
                cbind(c(26.3,18.7,5.2,2.2,1.7,
                        59.7,61.3,44.1,17.4,6.1,
                        11.4,17.5,41.0,48.2,37.0,
                        1.9,1.5,8.1,27.1,41.9,
                        0.3,0.3,1.2,4.8,13.0,
                        0.4,0.7,0.4,0.3,0.3)) %>%
                rbind(
                  cbind(rep(c("0-14","15-29","30-44","45-59","60+"),6)) %>%
                    cbind("Mężczyzna") %>%
                    cbind(rep(c("bardzo dobry", "dobry", "neutralny", "zły", "bardzo zły", "nieustalony"), each=5)) %>%
                    cbind(c(24.0,25.1,8.8,2.8,1.9,
                            59.1,56.5,48.5,24.8,12.0,
                            13.4,14.9,33.1,42.8,40.4,
                            2.7,2.6,8.3,24.8,35.7,
                            0.4,0.2,1.1,4.6,9.6,
                            0.4,0.7,0.2,0.2,0.4))))
colnames(data) <- c("age","sex","health","fraction")
data

library(ggplot2)
ggplot(data = data, aes(x = data$age, fill = data$health)) +
  geom_bar(position = position_stack())
