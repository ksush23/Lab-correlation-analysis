library(corrplot)
library(qgraph)

marathon <- read.csv("MarathonData.csv")
km4week <- marathon$km4week
marathonTime <- marathon$MarathonTime
speed4week <-marathon$sp4week
diff <- marathon$Wall21

grades <- read.csv("nyc_school_explorer_refined.csv")
attendance <- grades$Student.Attendance.Rate
absent <- grades$Percent.of.Students.Chronically.Absent
collaboratives <- grades$Collaborative.Teachers..
support <- grades$Supportive.Environment..
math <- grades$Average.Math.Proficiency
ela <- grades$Average.ELA.Proficiency

whites <- grades$Percent.White
blacks <- grades$Percent.Black
asian <- grades$Percent.Asian
hispanic <- grades$Percent.Hispanic
races <- data.frame(percent=c(whites, blacks, asian))

d <- data.frame(km4week, marathonTime, speed4week, diff)

gr <- data.frame(attendance, absent, collaboratives, support, math, ela)

race <- data.frame(whites, blacks, asian, hispanic, math, ela)

pairs(d)
plot(km4week, speed4week)
plot(speed4week, marathonTime)
plot(marathonTime, diff)
M <- cor(d)
corrplot.mixed(M)
qgraph(M,layout="spring")

pairs(gr)
G <- cor(gr)
corrplot.mixed(G)
qgraph(G, layout="spring")

pairs(race)
R <- cor(race)
corrplot.mixed(R)
qgraph(R, layout="spring")

cor(marathonTime, diff)
cor(marathonTime, km4week)
cor(km4week, diff)

cor.test(marathonTime, diff)$p.value
cor.test(marathonTime, km4week)$p.value
cor.test(km4week, diff)$p.value

cor(attendance, absent)
cor(ela, math)
cor(support, collaboratives)

cor.test(attendance, absent)$p.value
cor.test(ela, math)$p.value
cor.test(support, collaboratives)$p.value

cor(whites, ela)
cor(whites, math)
cor(asian, math)
cor(asian, ela)

cor.test(whites, ela)$p.value
cor.test(whites, math)$p.value
cor.test(asian, math)$p.value
cor.test(asian, ela)$p.value

cor(marathonTime, diff, method = "spearman")
cor(marathonTime, km4week, method = "spearman")
cor(km4week, diff, method = "spearman")

cor.test(marathonTime, diff, method = "spearman", exact = FALSE)$p.value
cor.test(marathonTime, km4week, method = "spearman", exact = FALSE)$p.value
cor.test(km4week, diff, method = "spearman", exact = FALSE)$p.value

cor(attendance, absent, method = "spearman")
cor(ela, math, method = "spearman")
cor(support, collaboratives, method = "spearman")

cor.test(attendance, absent, method = "spearman", exact = FALSE)$p.value
cor.test(ela, math, method = "spearman", exact = FALSE)$p.value
cor.test(support, collaboratives, method = "spearman", exact = FALSE)$p.value

cor(whites, ela, method = "spearman")
cor(whites, math, method = "spearman")
cor(asian, math, method = "spearman")
cor(asian, ela, method = "spearman")

cor.test(whites, ela, method = "spearman", exact = FALSE)$p.value
cor.test(whites, math, method = "spearman", exact = FALSE)$p.value
cor.test(asian, math, method = "spearman", exact = FALSE)$p.value
cor.test(asian, ela, method = "spearman", exact = FALSE)$p.value

cor(marathonTime, diff, method = "kendall")
cor(marathonTime, km4week, method = "kendall")
cor(km4week, diff, method = "kendall")

cor.test(marathonTime, diff, method = "kendall")$p.value
cor.test(marathonTime, km4week, method = "kendall")$p.value
cor.test(km4week, diff, method = "kendall")$p.value

cor(attendance, absent, method = "kendall")
cor(ela, math, method = "kendall")
cor(support, collaboratives, method = "kendall")

cor.test(attendance, absent, method = "kendall")$p.value
cor.test(ela, math, method = "kendall")$p.value
cor.test(support, collaboratives, method = "kendall")$p.value

cor(whites, ela, method = "kendall")
cor(whites, math, method = "kendall")
cor(asian, math, method = "kendall")
cor(asian, ela, method = "kendall")

cor.test(whites, ela, method = "kendall")$p.value
cor.test(whites, math, method = "kendall")$p.value
cor.test(asian, math, method = "kendall")$p.value
cor.test(asian, ela, method = "kendall")$p.value