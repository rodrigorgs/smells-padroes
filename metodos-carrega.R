rm(list=ls())
library(dplyr)
trim.spaces <- function (x) gsub("^\\s+|\\s+$", "", x)

sistemas <- c("FreeCS", "axion-1.0-M2", "collections-3.2.1", "colt-1.2.0", "displaytag-1.2", "drawswf-1.2.9", "emma-2.0.5312", "fitjava-1.1", "itext-5.0.3", "jOggPlayer114s", "jena-2.6.3", "jext-5.0", "jgraph-5.13.0.0", "jgrapht-0.8.1", "jmoney-0.4.4", "jparse-0.96", "jpf-1.5.1", "junit-4.10", "nekohtml-1.9.14", "oscache-2.3", "picocontainer-2.10.2", "pooka-3.0-080505", "proguard-4.9", "quickserver-1.4.7", "sablecc-3.2")
classes <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)
padroes <- data.frame(matrix(ncol = 4, nrow = 0), stringsAsFactors = F)
smells <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)
col_classes <- c("classe", "tipo", "metodo")
col_padroes <- c("classe", "metodo", "papel", "padrao")
col_smells <- c("classe", "metodo", "smell")
colnames(classes) <- col_classes
colnames(padroes) <- col_padroes
colnames(smells) <- col_smells

# Categorias e niveis
catPadroes <- read.csv("catpadroes.csv", header=T, stringsAsFactors = F) %>%
  rbind(data.frame(padrao="(nenhum)", catPadrao="(nenhum)", stringsAsFactors = F))
catSmells <- read.csv("catsmells.csv", header=T, stringsAsFactors = F) %>%
  rbind(data.frame(smell="(nenhum)", catSmell="(nenhum)", stringsAsFactors = F))
nivelSmells <- read.csv("nivelsmells.csv", header=T, stringsAsFactors = F) %>%
  rbind(data.frame(smell="(nenhum)", nivelSmell="(nenhum)", stringsAsFactors = F))


for (sistema in sistemas) {
  print(sistema)
  c <- read.csv(col.names = col_classes, paste0("coocorrencia/saida/", sistema, "_classes.txt"), header = F, sep = "\t", stringsAsFactors = F)
  p <- read.csv(col.names = col_padroes, paste0("coocorrencia/saida/", sistema, "_padrao.txt"), header = F, sep = "\t", stringsAsFactors = F)
  s <- read.csv(col.names = col_smells, paste0("coocorrencia/saida/", sistema, "_smells.txt"), header = F, sep = "\t", stringsAsFactors = F)
  
  # Remove linhas duplicadas
  c <- unique(c)
  
  # Remove $1, $2 etc., que representam classes anônimas internas
  p$classe <- gsub("\\$\\d+$", "", p$classe)
  # Normaliza (Object)Adapter
  p$padrao <- gsub("(Object)", "", p$padrao, fixed = T)
  p$padrao <- trim.spaces(p$padrao)
  # Remove ocorrência de padrões que não correspondem a métodos
  p <- p %>% filter(metodo != "null")
  
  # Achata classes com classes internas
  p$classe <- gsub("\\$.*", "", p$classe)
  c$classe <- gsub("\\$.*", "", c$classe)
  s$classe <- gsub("\\$.*", "", s$classe)
  
  # Troca $ por ., pois o JSpIRIT representa como .
  # p$classe <- gsub("$", ".", p$classe, fixed = T)
  # c$classe <- gsub("$", ".", c$classe, fixed = T)
  
  # Checa inconsistencias
  x <- setdiff(paste(p$classe, p$metodo), paste(c$classe, c$metodo))
  if (length(x) > 0) {
    print("p - c")
    print(x)
    # stop()
  }
  x <- setdiff(paste(s$classe, s$metodo), paste(c$classe, c$metodo))
  if (length(x) > 0) {
    print("s - c")
    print(x)
    # stop()
  }
  rm(x)
  
  classes <- classes %>% rbind(cbind(c, sistema))
  padroes <- padroes %>% rbind(cbind(p, sistema))
  smells <- smells %>% rbind(cbind(s, sistema))
}
rm(c, p, s)
rm(col_classes, col_padroes, col_smells)

# Remove campo metodo de smells a nivel de classe
classLevelSmells <- nivelSmells %>% filter(nivelSmell == "class") %>% .$smell
smells[smells$smell %in% classLevelSmells,]$metodo = ""

classes$elemento <- paste(classes$classe, classes$metodo, sep = ".") %>% gsub("[.]$", "", .)
padroes$elemento <- paste(padroes$classe, padroes$metodo, sep = ".") %>% gsub("[.]$", "", .)
smells$elemento <- paste(smells$classe, smells$metodo, sep = ".") %>% gsub("[.]$", "", .)

###############


padroes <- padroes %>%
  inner_join(catPadroes, by = "padrao")

smells <- smells %>%
  inner_join(catSmells, by = "smell") %>%
  inner_join(nivelSmells, by = "smell")

oldClasses <- classes

#######

# c1 <- oldClasses %>%
#   left_join(smells %>%
#               filter(nivelSmell == "class") %>%
#               select(classe, smell, catSmell, nivelSmell),
#             by="classe")

c2 <- oldClasses %>%
  left_join(smells %>%
              filter(nivelSmell == "method"))
# cMerge <- rbind(c1, c2)
cMerge <- c2

all <- cMerge %>%
  left_join(padroes) %>%
  mutate(hasPadrao = !is.na(padrao),
         hasSmell = !is.na(smell)) %>%
  unique()

# classes <- all %>%
#   select(-metodo, -elemento) %>%
#   filter(is.na(catSmell) | catSmell == "classe") %>%
#   unique()

# metodos <- all %>%
#   filter(is.na(catSmell) | nivelSmell == "metodo")
metodos <- all

rm(cMerge, all, c1, c2, oldClasses)
