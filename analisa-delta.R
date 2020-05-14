rm(list=ls())
library(readr)
library(dplyr)

project <- "displaytag"
padroes_orig <- read.csv(path.expand(paste0("~/Dropbox/ARTIGOS/arquivos_2020/novos/pai/", project, "_padrao.txt")), sep = "\t", header = F, col.names = c("commit", "parent", "classe", "metodo", "padrao", "operacao"), stringsAsFactors = F)
smells_orig <- read.csv(path.expand(paste0("~/Dropbox/ARTIGOS/arquivos_2020/novos/pai/", project, "_smells.txt")), sep = "\t", header = F, col.names = c("commit", "parent", "classe", "metodo", "smell", "operacao"), stringsAsFactors = F)

padroes <- padroes_orig %>%
  filter(operacao == 'Adicionado' | operacao == 'Passado') %>%
  select(-operacao)

padroes_do_pai <- padroes %>%
  select(parent) %>%
  inner_join(padroes, by = c("parent" = "commit")) %>%
  rename(commit = parent) %>%
  select(-parent.y) %>%
  distinct()

padroes <- padroes %>% select(-parent)

padroes_adicionados <- anti_join(padroes, padroes_do_pai)
padroes_removidos <- anti_join(padroes_do_pai, padroes)

delta_padroes <- padroes_adicionados %>%
  mutate(operacao = 'Adicionado') %>%
  rbind(padroes_removidos %>% mutate(operacao = 'Removido'))

###

smells <- smells_orig %>%
  filter(operacao == 'Adicionado' | operacao == 'Passado') %>%
  select(-operacao)

smells_do_pai <- smells %>%
  select(parent) %>%
  inner_join(smells, by = c("parent" = "commit")) %>%
  rename(commit = parent) %>%
  select(-parent.y) %>%
  distinct()

smells <- smells %>% select(-parent)

smells_adicionados <- anti_join(smells, smells_do_pai)
smells_removidos <- anti_join(smells_do_pai, smells)

delta_smells <- smells_adicionados %>%
  mutate(operacao = 'Adicionado') %>%
  rbind(smells_removidos %>% mutate(operacao = 'Removido'))

###

x <- delta_padroes %>%
  inner_join(delta_smells, by = c("commit", "classe", "metodo"))
