#Visualization

install.packages("esquisse")
install.packages("rgl")
install.packages("mlmRev")
install.packages("gcookbook")
install.packages("maps")
install.packages("psych")
library(ggplot2)
library(esquisse)
library(mlmRev)
library(gcookbook)
library(rgl)
library(maps)
library(stringr)
library(psych)

# esquisse{esquisser} - Addins를 이용해 빌더를 호출하거나, esquisser(전처리 data) 함수를 통해 빌더를 호출해 시각화를 수행한다.
# 사용할 데이터셋 datasets{VADeaths,quakes,iris,EuStockMarkets}, mlmRev{Chem97}, ggplot2{diamonds},gcookbook{}

#1. 막대차트 - esquisse :: ggplot
data("VADeaths") #datasets
VAD.df <- as.data.frame.table(VADeaths)
## 코드 디버깅시 이 부분 삭제해 함수 호출 혹은 그냥 하단 ggplot 실행 ## esquisser(VAD.df) 

#1 - 1. 가로차트
ggplot(VAD.df) +
 aes(x = Var1, weight = Freq) +
 geom_bar(fill = "#EF562D") +
 labs(title = "Bar Chart made by Esquisse") +
 coord_flip() +
 theme_gray() +
 theme(plot.title = element_text(size = 20L, hjust = 0.5), axis.title.y = element_text(size = 0L), 
 axis.title.x = element_text(size = 14L)) +
 facet_wrap(vars(Var2), nrow = 1L)

#1 - 2. 세로차트
ggplot(VAD.df) +
  aes(x = Var1, weight = Freq) +
  geom_bar(fill = "#EF562D") +
  labs(title = "Bar Chart made by Esquisse") +
  theme_gray() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5), axis.title.y = element_text(size = 0L), 
        axis.title.x = element_text(size = 14L)) +
  facet_wrap(vars(Var2), nrow = 1L)

# 2. 누적 막대그래프 - esquisse :: ggplot
ggplot(diamonds) + #ggplot2
  aes(x = price, fill = cut) +
  geom_histogram(bins = 30L) +
  scale_fill_viridis_d(option = "viridis", 
                       direction = 1) +
  labs(title = "Bar Chart made by Esquisse") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, 
                                  face = "bold", hjust = 0.5))

#3. 점차트 ggplot2 :: ggplot

data("tophitters2001") #gcookbook
tophit <- tophitters2001[1:25,]
ggplot(tophit, aes(x = reorder(name, avg), y = avg)) + 
  geom_segment(aes(xend = name), yend = 0, color = "black") +
  geom_point(size = 3, color = "red") +
    theme_bw() + 
  labs(title = "Dot Chart made by ggplot2", x = "Name", y = "Average") +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold", hjust = .5),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey60", linetype = "dashed"), 
    axis.text.x = element_text(angle = 60, hjust = 1)) 

#4. 원형그래프
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520) # 1차원 배열 
names(chart_data) <- c("2019 1분기", "2020 1분기", "2019 2분기", "2020 2분기", "2019 3분기", 
                       "2020 3분기", "2019 4분기", "2020 4분기")
pie(chart_data , labels = names(chart_data), border = 'blue', col=rainbow(8), cex=1.2) 
title("2019~2020년도 분기별 매출 현황")

  
#5. 박스 차트 esquisse :: ggplot VADeaths
ggplot(VAD.df) +
 aes(x = Var2, y = Freq) +
 geom_boxplot(shape = "circle", fill = "#EFF0F2") +
 labs(title = "Box Plot Chart made by Esquisse") +
 theme_gray() +
 theme(plot.title = element_text(size = 20L, hjust = 0.5), axis.title.y = element_text(size = 0L), 
 axis.title.x = element_text(size = 0L))

# 6. 히스토그램 esquisse :: ggplot
ggplot(Chem97) + #mlmRev 
  aes(x = gcsescore) +
  geom_histogram(bins = 30L, fill = "#D61D0A") +
  labs(title = "Histogram made by Esquisse", 
       caption = "facets by score") +
  theme_gray() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5), 
        plot.caption = element_text(size = 12L), axis.title.y = element_text(size = 14L), axis.title.x = element_text(size = 14L)) +
  facet_wrap(vars(score), scales = "free_y")

# 7. 산점도 esquisse :: ggplot
# 데이터 샘플링과정 필요. 범주화를통해 미리 준비해줘야함
# 산점도의 경우 데이터 전처리를 거치는 과정이 길어질 경우엔 lattice 패키지를 이용하는것이 더 효율적일수도 있음.
data("quakes") #datasets
quakes$depthgroup[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depthgroup[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depthgroup[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depthgroup[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depthgroup[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depthgroup[quakes$depth >= 551 & quakes$depth <= 680] <- 6
## 코드 디버깅시 이 부분 삭제해 함수 호출 혹은 그냥 하단 ggplot 실행 ## esquisser(quakes) 샘플링 데이터셋 import용 함수
ggplot(quakes) +
 aes(x = long, y = lat) +
 geom_point(shape = "circle", size = 2.55, colour = "#D90518") +
 labs(x = "longitude", y = "latitude", title = "Scatter Plot made by Esquisse") +
 theme_gray() +
 theme(plot.title = element_text(size = 20L, 
 face = "bold", hjust = 0.5), axis.title.y = element_text(size = 14L), axis.title.x = element_text(size = 14L)) +
 facet_wrap(vars(depthgroup))

# 9. 변수 비교 
data(iris)
attributes(iris)
pairs.panels(iris[iris$Species == "virginica", 1:4])
pairs.panels(iris[iris$Species == "setosa", 1:4])


# 10. 밀도 차트 esquisse :: ggplot
# 빌더 내 투명도옵션 미지원 => R코드로 copy해 온 후 geom_density(alpha = ) 적용 필요
# esquisser

ggplot(Chem97) + #mlmRev
  aes(x = gcsescore, fill = gender) +
  geom_density(adjust = 1L, alpha = .5) + 
  scale_fill_viridis_d(option = "viridis", 
                       direction = 1) +
  labs(title = "Density Plot made by Esquisse", caption = "facets by score") +
  theme_gray() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5), plot.caption = element_text(size = 12L), 
        axis.title.y = element_text(size = 14L), axis.title.x = element_text(size = 14L)) +
  facet_wrap(vars(score))

# 11. rgl{3dscatter} 
data("iris") #datasets
plot3d(iris, type = "s", size = 2, col = as.numeric(iris$Species)) # Species Factor를 숫자로 변환.
rgl.snapshot("iris3dplot.png")

# 12. 지도시각화 ggplot2 :: ggplot
#데이터 샘플링
korea <- map_data("world", region = "South Korea") #maps 패키지 이용해서 지도 데이터 준비

#매핑용 데이터 준비
pop <- read.csv(file.choose(), header = T) #population201901.csv.
region <- pop$지역명;lon <- pop$LON;lat <- pop$LAT 
tot_pop <- as.numeric(str_replace_all(pop$총인구수,',',''))
Map.df <- data.frame(region, lon, lat, tot_pop)
Map.df <- Map.df[1:17, ]

ggplot() + geom_polygon(data = korea, aes(x = long, y = lat, group = group)) + 
  geom_point(data = Map.df, aes(x = lon, y = lat, color = region, size = factor(tot_pop))) +
  geom_text(data = Map.df, aes(x = lon + 0.2, y = lat + 0.2, label = region, color = "#FFFFFF")) +
  theme(legend.position = "none")
# ggplot을 이용하는 경우에는 시각화 전에 데이터가 많이 필요하며 미학적으로도 떨어짐. 때문에 지도 정보 시각화에 전문화된
# ggmap등을 이용해 시각화하는게 더 전달력이 뛰어나도 할 수 있음.

# 대부분의 경우 ggplot2 패키지가 범용성이 뛰어나며, 각종 기하학/미적 요소들을 매핑하기 쉽기 때문에 ggplot2를 사용하는것이 
# 가장 효율적으로 보여지며, esquisse 패키지의 경우 ggplot을 기반으로 빌더를 지원하기 때문에 데이터셋의 샘플링만 거친 이후
# 간편하게 시각화가 가능했기때문에 ggplot2와 esquisse 패키지를 적극적으로 활용하여 사례연구를 수행하였음.
# esquisse 패키지에 적용하기 위한 전처리과정이 필요 이상 길어지거나 차트 형태를 지원하지 않는경우, 
# 그리고 다른 패키지를 사용하는 것이 나은 경우, 해당 패키지를 사용했음.
# 비교는 2번자료 완성 후 간단하게 비교하면?