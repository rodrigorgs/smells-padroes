library(dplyr)

sistemas <- c("axion-1.0-M2", "collections-3.2.1", "colt-1.2.0", "displaytag-1.2", "drawswf-1.2.9", "emma-2.0.5312", "fitjava-1.1", "FreeCS", "itext-5.0.3", "jena-2.6.3", "jext-5.0", "jgraph-5.13.0.0", "jgrapht-0.8.1", "jmoney-0.4.4", "jOggPlayer114s", "jparse-0.96", "jpf-1.5.1", "junit-4.10", "nekohtml-1.9.14", "oscache-2.3", "picocontainer-2.10.2", "pooka-3.0-080505", "proguard-4.9", "quickserver-1.4.7", "sablecc-3.2")
trim.spaces <- function (x) gsub("^\\s+|\\s+$", "", x)

classes <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)
padroes <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)
smells <- data.frame(matrix(ncol = 3, nrow = 0), stringsAsFactors = F)
for (sistema in sistemas) {
  print(sistema)
  c <- read.csv(paste0("sistemas/", sistema, "_classes.txt"), header = F, sep = "\t", stringsAsFactors = F)
  p <- read.csv(paste0("sistemas/", sistema, "_padrao.txt"), header = F, sep = "\t", stringsAsFactors = F)
  s <- read.csv(paste0("sistemas/", sistema, "_smells.txt"), header = F, sep = "\t", stringsAsFactors = F)
  
  # limpa
  p[,1] <- gsub("\\$.*", "", p[,1])  # remove classes internas
  p[,2] <- gsub("(Object)", "", p[,2], fixed = T)  # normaliza (Object)Adapter
  p[,2] <- trim.spaces(p[,2])
  c <- subset(c[,1], !grepl("package-info", c[,1]))
  
  # adiciona classes em (p - c) e (s - c) a c.
  x <- setdiff(p[,1], c)
  if (length(x) > 0) { c <- c %>% c(x) }
  x <- setdiff(s[,1], c)
  if (length(x) > 0) { c <- c %>% c(x) }
  
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

# padroes <- padroes %>%
#   mutate(padrao = gsub("(Object)", "", padrao, fixed = T))

# Categorias
catPadroes <- read.csv("catpadroes.csv", header=T, stringsAsFactors = F) %>%
  rbind(data.frame(padrao="(nenhum)", catPadrao="(nenhum)", stringsAsFactors = F))
catSmells <- read.csv("catsmells.csv", header=T, stringsAsFactors = F) %>%
  rbind(data.frame(smell="(nenhum)", catSmell="(nenhum)", stringsAsFactors = F))

padroes <- padroes %>%
  inner_join(catPadroes, by = "padrao") %>%
  mutate(hasSmell = classe %in% smells$classe)

smells <- smells %>%
  inner_join(catSmells, by = "smell") %>%
  mutate(hasPadrao = classe %in% padroes$classe)

classes <- classes %>%
  mutate(hasPadrao = classe %in% padroes$classe,
         hasSmell = classe %in% smells$classe)

padroes_smells <- classes %>%
  left_join(unique(padroes)) %>%
  left_join(unique(smells)) %>%
  select(classe, sistema, padrao, smell) %>%
  mutate(padrao = if_else(is.na(padrao), "(nenhum)", padrao)) %>%
  mutate(smell = if_else(is.na(smell), "(nenhum)", smell)) %>%
  left_join(catPadroes) %>%
  left_join(catSmells)

padroes_smells_simultaneos <- padroes_smells %>%
  filter(padrao != '(nenhum)' & smell != '(nenhum)')

# padroes_ou_smells <- padroes_smells %>%
#   filter(padrao != '(nenhum)' | smell != '(nenhum)')


# unique(padroes$padrao)
# unique(catPadroes$padrao)
# unique(smells$smell)
# unique(catSmells$smell)
