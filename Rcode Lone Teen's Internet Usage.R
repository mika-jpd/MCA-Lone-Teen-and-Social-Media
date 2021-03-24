library(FactoMineR)
library(factoextra)

# Dataset 2: Combo 2
path = file.path("C:/", "Users/mikad/Desktop/Winter 2021/Math 308/Assignements/Assignement 6/March 7-April 10, 2018 - Teens and Tech Survey - CSV.csv")
data = read.csv(path)
data = as.data.frame(data)
colnames(data)

#replace numerical data with sentences
data = data.frame(lapply(data, as.character), stringsAsFactors=FALSE)

#keep only those that answered 0 to SNS1_8
data = data[data$SNS1_8 == "0",]
data = data[data$FITIN == "2",]
data = data[data$GROUP1 != "4",]

#clean dataset
data = data[data$GROUP1 != "NA",]
data = data[data$GROUP1 != "98",]

data = data[data$GROUP3A != "98",]
data = data[data$GROUP3A != "NA",]

data = data[data$GROUP3B != "98",]
data = data[data$GROUP3B != "NA",]

data = data[data$GROUP3D != "98",]
data = data[data$GROUP3D != "NA",]

data = data[data$GROUP3C != "98",]
data = data[data$GROUP3C != "NA",]

data = data[data$SOC4A != "98",]
data = data[data$SOC4A != "NA",]

data = data[data$SOC4B != "98",]
data = data[data$SOC4B != "NA",]

data = data[data$SOC4C != "98",]
data = data[data$SOC4C != "NA",]

data = data[data$SOC4D != "98",]
data = data[data$SOC4D != "NA",]

data$GROUP3A
data$GROUP3B
data$GROUP3C
data$GROUP3D
data$SOC4A
data$SOC4B
data$SOC4C
data$SOC4D

#remove questions that you don't want
data = data[,-c(1,2,7:12,14:38,40:60,81:90,101:114,116:126, 131:134,136:176, 61:72, 91:100, 74:76,4:6,13,135)]
colnames(data)

#perform MCA
data.MCA = MCA(data, graph = FALSE)
eig.val = data.MCA$eig

#percentage of variability explained
head(eig.val)

#display how much variability is explained
fviz_screeplot(data.MCA,addlabels=T) +
  geom_hline(yintercept=8.333,linetype=2,color="red")

#biplot of individuals and variable category
fviz_mca_biplot(data.MCA,
                repel = TRUE,
                ggtheme = theme_minimal())

#correlation between variables categories and principal dimensions
fviz_mca_var(data.MCA, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal())

#relationship between variable categories
#coordinates of each variable categories in each dimension
var = get_mca_var(data.MCA)
head(round(var$coord, 2), 500)
#visualize
fviz_mca_var(data.MCA,
             repel = TRUE,
             ggtheme = theme_minimal())

#Quality of representation of variable categories
head(var$cos2, 13)
fviz_mca_var(data.MCA, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

#Contributions of variable categories to the dimension
head(round(var$contrib,2), 20)
fviz_contrib(data.MCA, choice = "var", axes = 1, top = 15)

#visualize individual contributions to quality 
fviz_mca_ind(data.MCA, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())
