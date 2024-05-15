#importing ipm data
ipm <- read.csv2("C:/Users/Abraham/Documents/abraham/statistika ofisial/final project/cleaneddata.csv")
ipm
ipmwocities = ipm[,c(2:7)]
ipmwocities
ipm2022 = ipm[ipm$Tahun==2022,]
View(ipm2022)
cleanipm <- ipm2022[,c(3,5:6,8:9)]
View(cleanipm)
highestsix <- ipm2022 %>% slice_max(ipm2022$SMA, n=6)
highestsix
sixamh <- ipm2022 %>% slice_max(ipm2022$AMH, n=6)
sixamh
jatim <- read.csv2("C:/Users/Abraham/Documents/abraham/statistika ofisial/final project/summary.csv")
jatim

install.packages("gridExtra")

#LINE PLOT
par(mfrow=c(1,1))
plot(jatim$Tahun,
     jatim$IPM, 
     type = "b", #ganti jadi ada buletannya di line
     col = "#2F4858",
     xlab = "Year",
     ylab = "Human Development Index",
     ylim = c(70,75), #limit biasa
     pch = 16, #style buletan ges
     main = "Human Development Index Growth in East Java",
     frame.plot = FALSE, #hilangin frame
     xaxt='n', #hilangin tulisan di axis x
     yaxt='n') #hilangin tulisan di axis y
axis(1,
     col = "#A6AE9C",
     at = jatim$tahun)
axis(2,
     col = "#A6AE9C",
     at = seq(70, 75, 1))

#Visualization
par(mfrow=c(1,2))
plot(jatim$Tahun,
     jatim$HLS, 
     type = "b",
     col = "#2F4858",
     xlab = "Year",
     ylab = "Expected Years of Schooling",
     ylim = c(13,13.5),
     pch = 16,
     main = "Expected Years of Schooling in East Java",
     frame.plot = FALSE, #hilangin frame
     xaxt='n',
     yaxt='n')
axis(1,
     col = "#A6AE9C",
     at = jatim$tahun)
axis(2,
     col = "#A6AE9C",
     at = seq(13, 13.5, 0.1))

plot(jatim$Tahun,
     jatim$RLS, 
     type = "b",
     col = "#2F4858",
     xlab = "Year",
     ylab = "Mean Years of Schooling",
     ylim = c(7,8.2),
     pch = 16,
     main = "Mean Years of Schooling in East Java",
     frame.plot = FALSE, #hilangin frame
     xaxt='n',
     yaxt='n')
axis(1,
     col = "#A6AE9C",
     at = jatim$tahun)
axis(2,
     col = "#A6AE9C",
     at = seq(7,8.2,0.1))

install.packages("ggpubr")
p1 <- ggplot(highestsix, aes(x=Kota.Kabupaten, y=SMA, fill=Kota.Kabupaten)) +
  geom_bar(stat="identity") + 
  ggtitle("Top 6 Highest Number of High School Students in East Java") +
  labs(y = "Kota/Kabupaten", x = "Jumlah Siswa SMA") +
  scale_fill_manual(values=c("#cdb4db", "#ffc8dd", "#ffafcc","#bde0fe","#a2d2ff","#cfbaf0"))
p2 <- ggplot(sixamh, aes(x=Kota.Kabupaten, y=AMH, fill=Kota.Kabupaten)) +
  geom_bar(stat="identity") + 
  ggtitle("Top 6 Highest Literacy Rate in East Java") +
  labs(y = "Kota/Kabupaten", x = "Angka Melek Huruf") +
  scale_fill_manual(values=c("#cdb4db", "#ffc8dd", "#ffafcc","#bde0fe","#a2d2ff","#cfbaf0","#b8c0ff"))
grid.arrange(p1, p2, nrow=2)

#regression
r1 <- ggplot(data=cleanipm, aes(x=AMH, y=IPM)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_regline_equation(label.x=80, label.y=85) +
  stat_cor(aes(label=..rr.label..), label.x=80, label.y=80) +
  ggtitle("Plot of Human Development Index vs Literacy Rate") +
  labs(y = "IPM", x = "Angka Melek Huruf")

r2 <- ggplot(data=cleanipm, aes(x=HLS, y=IPM)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_regline_equation(label.x=12, label.y=85)+
  stat_cor(aes(label=..rr.label..), label.x=12, label.y=80) +
  ggtitle("Plot of Human Development Index vs Expected Years of Schooling") +
  labs(y = "IPM", x = "Harapan Lama Sekolah")

r3 <- ggplot(data=cleanipm, aes(x=RLS, y=IPM)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_regline_equation(label.x=6, label.y=85)+
  stat_cor(aes(label=..rr.label..), label.x=6, label.y=80) +
  ggtitle("Plot of Human Development Index vs Mean Years of Schooling") +
  labs(y = "IPM", x = "Rata-Rata Lama Sekolah")

r4 <- ggplot(data=cleanipm, aes(x=SMA, y=IPM)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_regline_equation(label.x=25, label.y=85)+
  stat_cor(aes(label=..rr.label..), label.x=25, label.y=80) +
  ggtitle("Plot of Human Development Index vs Number of High School Students") +
  labs(y = "IPM", x = "Jumlah Siswa SMA")
grid.arrange(r1, r2, r3, r4, ncol=2, nrow=2)

#CORRELATION PLOT
par(mfrow=c(1,1))
corrplot(
  cor(cleanipm),
  method = "color",
  addCoef.col = "white"
)

pairs.panels(cleanipm, 
             method = 'pearson', 
             density = TRUE,
             ellipses = TRUE,
             main = 'Pair Plot of IPM, HLS, RLS, SMA, AMH', 
             hist.col = c("#cdb4db", "#ffc8dd", "#ffafcc","#bde0fe","#a2d2ff"))

#trial bubble animation
install.packages("gganimate")
library(gganimate)
p <- ggplot(ipm, aes(x = HLS, y=IPM, size = PPRD, col=Kota.Kabupaten)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "Human Development Index", y = "Expected Years of Schooling") +
  ggtitle("Bubble Visualization of HDI vs Expected Years of Schooling in East Java")
p + transition_time(Tahun) +
  labs(title = "Year: {frame_time}")
