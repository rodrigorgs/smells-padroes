# TODO: categoria de padrao
# TODO: analise por sistema
# TODO: ordenar tabelas de contingencia para melhorar visualizacao

rm(list=ls())
library(dplyr)
library(readr)
library(tidyr)
library(vcd)

## Carga de dados

sistemas <- c("commons", "imglib2", "junit", "spmf", "log4j")

classes <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)
padroes <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)
smells <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)
for (sistema in sistemas) {
  c <- read.csv(paste0("sistemas/", sistema, "_classes.txt"), header = F, sep = "\t", stringsAsFactors = F)
  p <- read.csv(paste0("sistemas/", sistema, "_padrao.txt"), header = F, sep = "\t", stringsAsFactors = F)
  s <- read.csv(paste0("sistemas/", sistema, "_smells.txt"), header = F, sep = "\t", stringsAsFactors = F)
  classes <- classes %>% rbind(cbind(c, sistema))
  padroes <- padroes %>% rbind(cbind(p, sistema))
  smells <- smells %>% rbind(cbind(s, sistema))
}
rm(c)
rm(p)
rm(s)
colnames(classes) <- c("classe", "sistema")
colnames(padroes) <- c("classe", "padrao", "sistema")
colnames(smells) <- c("classe", "smell", "sistema")

padroes <- padroes %>%
  mutate(padrao = gsub("(Object)", "", padrao, fixed = T))

# Categorias
catPadroes <- read.csv("catpadroes.csv", header=T, stringsAsFactors = F) %>%
  rbind(data.frame(padrao="SemPadrao", catPadrao="SemPadrao"))
catSmells <- read.csv("catsmells.csv", header=T, stringsAsFactors = F) %>%
  rbind(data.frame(smell="SemSmell", catSmell="SemSmell"))

padroes <- padroes %>%
  inner_join(catPadroes, by = "padrao")

smells <- smells %>%
  inner_join(catSmells, by = "smell")

# unique(padroes$padrao)
# unique(catPadroes$padrao)
# unique(smells$smell)
# unique(catSmells$smell)


data <- read.csv("todos-sistemas.csv", header=T, stringsAsFactors = F)

rownames(data) <- data$X
data$X <- NULL
mat <- t(as.matrix(data))

# Constroi data frame a partir da matriz
df <- data.frame(matrix(ncol = 2, nrow = 0), stringsAsFactors = F)
x <- c("padrao", "smell")
colnames(df) <- x
for (padrao in rownames(mat)) {
  for (smell in colnames(mat)) {
    qtd <- mat[padrao, smell]
    if (qtd > 0) {
      row <- data.frame(padrao=padrao, smell=smell, stringsAsFactors = F)
      rows <- row[rep(1, each=qtd),]
      df <- df %>% rbind(rows)
    }
  }
}
rownames(df) <- NULL
df <- df %>%
  mutate(isPadrao = padrao != 'SemPadrao',
         isSmell = smell != 'SemSmell') %>%
  left_join(catPadroes, by=c("padrao")) %>%
  left_join(catSmells, by=c("smell"))

# Versao sem padroes e smells com baixa incidencia
manterPadroes <- c("State", "Adapter", "Template", "SemPadrao")
manterSmells <- c("Shotgun", "Refused", "Feature", "GodClass", "SemSmell")
df2 <- df %>%
  filter(padrao %in% manterPadroes) %>%
  filter(smell %in% manterSmells)

mymosaic <- function(tab) {
  tab <- tab[order(tab[,1] / rowSums(tab)),]
  mosaic(tab, shade=T,  direction = "v",
         rot_labels=c(45,0,0,0),
         just_labels = c("left", 
                         "center", 
                         "center", 
                         "right"),
         gp_labels=(gpar(fontsize=6)))
}

######################################################################

# assocstats(tab): Cramer's V
# interpreting V < 0.3 as small, V < 0.5 as medium, and
# V > 0.5 as large effect size

#' # Existe associação entre ocorrência de padrões e ocorrência de smells? 
tab <- xtabs(~ isPadrao + isSmell, data=df)
#mosaicplot(tab, shade = T)
mymosaic(tab)
chisq.test(tab, correct = F)
assocstats(tab) %>% print()
#
mcnemar.test(tab)
prop.test(tab)
fisher.test(tab)

#' # Quais padrões possuem maior propensão a apresentar smells?
tab <- xtabs(~ padrao + isSmell, data=df)
mean(tab < 5)
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

#' # Quais categorias de padrão possuem maior propensão a apresentar smell?
tab <- xtabs(~ catPadrao + isSmell, data=df)
mean(tab < 5)
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

#' # Quais são os smells mais frequentes nas classes que possuem padrões?
tab <- xtabs(~ smell + isPadrao, data=df)
mean(tab < 5)
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

#' # Quais são as categorias de smell mais frequentes nas classes que possuem padrões?
tab <- xtabs(~ catSmell + isPadrao, data=(df %>% filter(isSmell)))
mean(tab < 5)
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

#' # Cada padrao com cada smell
tab <- xtabs(~ padrao + smell, data=df)
mean(tab < 5)
mymosaic(tab)
mymosaic(t(tab))
chisq.test(tab)
assocstats(tab) %>% print()

#' # Cada categoria de padrao com cada smell
tab <- xtabs(~ catPadrao + smell, data=(df %>% filter(isPadrao & isSmell)))
mean(tab < 5)
mymosaic(t(tab))
chisq.test(tab)
assocstats(tab) %>% print()

#' # Cada categoria de padrao com cada categoria de smell
tab <- xtabs(~ catPadrao + catSmell, data=(df %>% filter(isPadrao & isSmell)))
mean(tab < 5)
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

