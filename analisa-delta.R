rm(list=ls())
library(readr)
library(dplyr)

# projects <- "fastjson"
# projects <- "commons-collections"
projects <- c("fastjson", "displaytag", "commons-io", "commons-collections")

# Carrega dados
USE_CACHE = TRUE
SAVE_FILE = "padroes_smells.RData"
if (USE_CACHE && file.exists(SAVE_FILE)) {
  load(SAVE_FILE)
} else {
  BASE_PATH <- "~/Dropbox/ARTIGOS/arquivos_2020/arquivos_para_analise/"
  
  padroes_orig <- NULL
  smells_orig <- NULL
  for (project in projects) {
    print(project)
    
    x <- read.csv(path.expand(paste0(BASE_PATH, project, "/", project, "_padrao.txt")), sep = "\t", header = F, stringsAsFactors = F,
             col.names = c("commit", "parent", "ancestor",  "classe", "metodo", "padrao", "operacao")) %>% mutate(project = project)
    padroes_orig <- padroes_orig %>% bind_rows(x)
    
    x <- read.csv(path.expand(paste0(BASE_PATH, project, "/", project, "_smells.txt")), sep = "\t", header = F, stringsAsFactors = F,
                  col.names = c("commit", "parent", "ancestor", "classe", "metodo", "smell", "operacao")) %>% mutate(project = project)
    smells_orig <- smells_orig %>% bind_rows(x)
  }
  
  if (USE_CACHE) {
    save(padroes_orig, smells_orig, file = SAVE_FILE)
  }
}

##########

#' # Estatísticas

#' ## Número de commits analisados

n_distinct(padroes_orig$commit)
n_distinct(smells_orig$commit)

padroes_orig %>% 
  group_by(project) %>%
  tally(n_distinct(commit)) %>%
  arrange(desc(n))

smells_orig %>% 
  group_by(project) %>%
  tally(n_distinct(commit)) %>%
  arrange(desc(n))

#' ## Número de classes analisadas
# TODO: são somente as classes que tiveram algum smell ou algum padrão em algum momento?

n_distinct(padroes_orig$classe)
n_distinct(smells_orig$classe)

padroes_orig %>%
  group_by(project) %>%
  summarise(n = n_distinct(classe)) %>%
  arrange(desc(n))

smells_orig %>%
  group_by(project) %>%
  summarise(n = n_distinct(classe)) %>%
  arrange(desc(n))


padroes_orig %>% group_by(project, classe) %>% summarise(n())

n_distinct(padroes_orig$classe)
n_distinct(smells_orig$classe)

#' ## Ocorrências de adição de padrão/smell

sum(padroes_orig$operacao == "Adicionado")
sum(smells_orig$operacao == "Adicionado")

padroes_orig %>% 
  filter(operacao == "Adicionado") %>%
  group_by(project) %>%
  tally() %>%
  arrange(desc(n))

smells_orig %>% 
  filter(operacao == "Adicionado") %>%
  group_by(project) %>%
  tally() %>%
  arrange(desc(n))


##########

# Remove registros sem commit anterior e considera somente alterações
# (padrão/smell adicionado/removido)

padroes <- padroes_orig %>%
  filter(ancestor != 'null' & parent != 'null') %>%
  filter(parent == ancestor) %>%
  filter(operacao != 'Passado') %>%
  select(-ancestor)

smells <- smells_orig %>%
  mutate(smell = trimws(smell)) %>%
  filter(ancestor != 'null' & parent != 'null') %>%
  filter(parent == ancestor) %>%
  filter(operacao != 'Passado') %>%
  select(-ancestor)

##########

#' Quantas adições/remoções de padrões?
padroes %>% filter(operacao != 'Passado') %>% tally()

#' Quantas adições/remoções de smells?
smells %>% filter(operacao != 'Passado') %>% tally()

##########

data <- padroes %>%
  rename(operacao_padrao = operacao) %>%
  filter(operacao_padrao == 'Adicionado') %>%
  left_join(smells) %>%
  rename(operacao_smell = operacao)

xtabs(~ padrao + smell, data=data, exclude=NULL, na.action=na.pass)

###########

#' Agrupando smells em sim/não

data2 <- data %>%
  group_by(commit, parent, classe, metodo, padrao) %>%
  summarise(added_smell = any(operacao_smell == 'Adicionado') %>% coalesce(FALSE),
            removed_smell = any(operacao_smell == 'Removido') %>% coalesce(FALSE))

#' # Ao adicionar um determinado padrão P, quantos % das vezes estamos adicionando algum smell (no mesmo commit)?
t <- xtabs(~ padrao + added_smell, data=data2, exclude=NULL, na.action=na.pass)
t
mosaicplot(t, shade=T)
chisq.test(t)

# xtabs(~ padrao + added_smell + project, data=data2, exclude=NULL, na.action=na.pass)

# t <- xtabs(~ padrao + removed_smell, data=data2, exclude=NULL, na.action=na.pass)
# t
# mosaicplot(t, shade=T)
# chisq.test(t)

##############

#' # Quais são os smells mais frequentes nos padrões State e Template Method?

data3 <- data %>%
  filter(padrao %in% c("State", "Template Method")) %>%
  filter(operacao_padrao == "Adicionado") %>%
  filter(is.na(operacao_smell) | operacao_smell != "Removido")

t <- xtabs(~ padrao + smell, data = data3, exclude=NULL, na.action=na.pass)
t
mosaicplot(t, shade=T)
chisq.test(t)

data3 %>% filter(operacao_smell == "Adicionado")
