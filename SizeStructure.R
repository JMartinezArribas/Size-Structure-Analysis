library(tidyverse)
library(readxl)
library(rmarkdown)
library(FSA)

dbicon <-  DBI::dbConnect(RPostgres::Postgres(),
                          db = Sys.getenv('POSTGRES_DB'), 
                          host = Sys.getenv('POSTGRES_HOST'), 
                          port = Sys.getenv('POSTGRES_PORT'), 
                          user = Sys.getenv('POSTGRES_USER'),
                          password = Sys.getenv('POSTGRES_PASSWORD'))

sql <- 'SELECT * FROM "tblRPN-LF"'
df <- DBI::dbGetQuery(dbicon, sql)
DBI::dbDisconnect(dbicon)

gor <- df %>% filter(Especie == 'GOR' & Ano == 2017)
Summarize(gor$Comprim)

#Anderson e Neumann (1996) sugeriram intervalos de 1 cm (0.5 in) para peixes que 
#atingem 30 cm (12 in), intervalos de 2 cm (1 in) para peixes que atingem 60 cm 
#(24 in) e intervalos de 5 cm (2 in) para peixes que atingem 150cm (60 in) de 
#comprimento máximo.

mn <- min(gor$Comprim)
mx <- max(gor$Comprim)
gor.brks = c(seq(mn, 29, 1), seq(30, 60, 2))  # In cm units

#hist(~Comprim,data=gor,breaks=gor.brks,main="",xlab="Comprimento (cm)",col="beige")
ggplot(data=gor, aes(x=Comprim)) +
  geom_histogram(color = "black", fill = "beige", breaks = gor.brks) + 
  xlab("Comprimento (cm)") +
  ylab("Frequency")
ggsave("hist_gor_comp.png")



#Podemos ver os dados agrupados por diferentes variáveis, como por "Área":
ggplot(gor, aes(Comprim, group = Area)) + 
  geom_histogram(fill='beige',color='black') + 
  xlab("Comprimento (cm)") +
  ylab("Frequency") +
  facet_wrap(~ Area)
ggsave("hist_comp_area.png")

#o Subárea:
ggplot(gor, aes(Comprim, group = Subarea)) + 
  geom_histogram(fill='wheat1',color='black') + 
  xlab("Comprimento (cm)") +
  ylab("Frequency") +
  facet_wrap(~ Subarea)
ggsave("hist_comp_subarea.png")


#ou Estrato:
ggplot(gor, aes(Comprim, group = Estrato)) + 
  geom_histogram(fill='bisque3',color='black') + 
  xlab("Comprimento (cm)") +
  ylab("Frequency") +
  facet_wrap(~ Estrato) 
ggsave("hist_comp_estrato.png")


## Proportional Size Distribution

#Gabelhouse (1984b) introduziu o sistema de categorização de "comprimento de cinco células" 
#que define comprimentos para os chamados indivíduos de tamanho "stock", "quality", 
#"preferred", "memorable" e "tropy" de uma grande variedade de espécies.
#
#Anderson e Weithman (1978) escolheram comprimentos equivalente a:  
#  
#   - Stock = 20-26% de comprimento recorde.  
#   - Quality = 36-41% de comprimento recorde.   
#   - Preferred = 45-55% de comprimento recorde.  
#   - Memorable = 59-64% de comprimento recorde.  
#   - Trophy = 74-80% de comprimento recorde.  
#
#Vejamos essa categorização como está representada na função de distribuição da 
#variável "Comprimento":

# Quantiles
q <- c(0, 0.20, 0.26, 0.36, 0.41, 0.45, 0.55, 0.59, 0.64, 0.74, 0.8)

# data.frame for quantiles
qdf <- data.frame(
  q = factor(q),
  x = c(rep(-Inf, length(q)), rep(quantile(gor$Comprim, q), 2)),
  y = c(rep(q, 2), rep(-Inf, length(q)))
)

ggplot(gor, aes(Comprim)) +
  stat_ecdf() +
  xlab("Comprimento (cm)") +
  ylab("Cumulative frequency") +
  # Line segments to/from the ecdf line
  geom_path(
    data = qdf,
    aes(x = x, y = y, colour = q),
    linetype = 1
  ) +
  # Labels at x
  geom_text(
    data = subset(qdf, is.finite(x) & is.finite(y)),
    aes(x = x, y = 0.5 * y, label = x, colour = q),
    hjust = -1
  ) +
  # Labels at y
  geom_text(
    data = subset(qdf, is.finite(x) & is.finite(y)),
    aes(x - 0.5 * min(x), y = y, label = y, colour = q),
    vjust = -1
  )
ggsave("PSD_gor.png")
#Os percentis propostos por Anderson e Weithman correspondem aos comprimentos:
#  
#   - Stock = 26-28 cm of record length  
#   - Quality = 31-32 cm of record length  
#   - Preferred = 33-35 cm of record length  
#   - Memorable = 36-38 cm of record length  
#   - Trophy = 40-42 cm of record length  
#
#
#Os dados de frequência de comprimento podem ser resumidos em algumas estatísticas 
#úteis chamadas distribuição de tamanho proporcional índices (PSD-X; Guy et al. 2007).
#
#Um índice PSD-X é definido como a proporção de peixes do tamanho "stock" que
#também são maiores do que alguma outra categoria de tamanho maior.

#O cálculo do PSD-X é simplificado no R através do uso de várias funções no pacote FSA,
#mas não disponível para todas as espécies de peixes.
#
#Por padrão, as quebras de intervalo são inclusivas (por exemplo, um peixe de 8 cm 
#                                                    será incluído no intervalo de 
#                                                    6-8 cm em vez do intervalo de 
#                                                    8-10 cm). 
#Isso não é o que a maioria dos biólogos pesqueiros deseja, então vamos mudar as 
#quebras de intervalo para deixá-las inclusivas em seu lugar.
#
#Por isso vamos criar um dataframe com os dados necessários para calcular essas 
#funções para outras espécies que não aparecem no pacote:

interval_table <- table(cut(gor$Comprim,unname(quantile(gor$Comprim, q)),include.lowest = T, right = F))
inter <- as.data.frame((interval_table))
inter <- inter %>% mutate(csum = cumsum(Freq),
                          rcsum = rcumsum(Freq),
                          min = unname(quantile(gor$Comprim, q))[1:nrow(inter)],
                          labels = c('zero','stock','stock','quality','quality','preferred',
                                     'preferred','memorable','memorable','trophy'))

psd <- inter %>% group_by(labels) %>% summarise(Csum = min(csum), Rcsum = max(rcsum),Min = min(min))
psd <- merge(psd, inter, by.x = c("labels","Min"), 
             by.y = c("labels","min"), all.x = TRUE, all.y = FALSE) %>% 
  select(-Var1) %>% 
  arrange(Min)

#E então encontramos os diferentes índices PSD-X e seus intervalos de confiança 
#de 95% com a ajuda da função binCI():

#PSD-Q
psd_q <- round(psd[psd$labels=='quality','Rcsum']/psd[psd$labels=='stock','Rcsum'] *100,5)
LCI_q <- unname(binCI(as.numeric(psd[psd$labels=='quality','Rcsum']),
                      as.numeric(psd[psd$labels=='stock','Rcsum']))*100 )[1,1]
UCI_q <- unname(binCI(as.numeric(psd[psd$labels=='quality','Rcsum']),
                      as.numeric(psd[psd$labels=='stock','Rcsum']))*100 )[1,2]
#PSD-P
psd_p <- round(psd[psd$labels=='preferred','Rcsum']/psd[psd$labels=='stock','Rcsum'] *100,5)
LCI_p <- unname(binCI(as.numeric(psd[psd$labels=='preferred','Rcsum']),
                      as.numeric(psd[psd$labels=='stock','Rcsum']))*100 )[1,1]
UCI_p <- unname(binCI(as.numeric(psd[psd$labels=='preferred','Rcsum']),
                      as.numeric(psd[psd$labels=='stock','Rcsum']))*100 )[1,2]
#PSD-M
psd_m <- round(psd[psd$labels=='memorable','Rcsum']/psd[psd$labels=='stock','Rcsum'] *100,5)
LCI_m <- unname(binCI(as.numeric(psd[psd$labels=='memorable','Rcsum']),
                      as.numeric(psd[psd$labels=='stock','Rcsum']))*100 )[1,1]
UCI_m <- unname(binCI(as.numeric(psd[psd$labels=='memorable','Rcsum']),
                      as.numeric(psd[psd$labels=='stock','Rcsum']))*100 )[1,2]
#PSD-T
psd_t <- round(psd[psd$labels=='trophy','Rcsum']/psd[psd$labels=='stock','Rcsum'] *100,5)
LCI_t <- unname(binCI(as.numeric(psd[psd$labels=='trophy','Rcsum']),
                      as.numeric(psd[psd$labels=='stock','Rcsum']))*100 )[1,1]
UCI_t <- unname(binCI(as.numeric(psd[psd$labels=='trophy','Rcsum']),
                      as.numeric(psd[psd$labels=='stock','Rcsum']))*100 )[1,2]


lbls <- c("PSD-Q","PSD-P","PSD-M","PSD-T")
vals <- c(as.numeric(psd_q), as.numeric(psd_p),as.numeric(psd_m),as.numeric(psd_t))
lcis_95 <- c(LCI_q,LCI_p,LCI_m,LCI_t)
ucis_95 <- c(UCI_q,UCI_p,UCI_m,UCI_t)

(psd_df <- tibble(lbls,vals,lcis_95,ucis_95))

#Como um aviso para dizer que os cálculos de PSD não precisam ser restritos apenas 
#aos comprimentos na categorização de comprimento de cinco células sistema.
#
#
#Finalmente vamos construir um histograma de frequência de comprimento com barras 
#sombreadas representando o tamanho do estoque e peixes maiores e os comprimentos 
#da categoria de comprimento de cinco células marcados com linhas verticais:

ggplot(data=gor, aes(x=Comprim)) +
  geom_histogram(color = "black", fill = "beige", breaks = gor.brks) + 
  geom_vline(xintercept=31,col='red') +
  geom_vline(xintercept=33,col='orange') +
  geom_vline(xintercept=36,col='blue') +
  geom_vline(xintercept=40,col='green') +
  xlab("Comprimento (cm)") +
  ylab("Frequency")
  
ggsave("hist_gor_comp_PSD.png")


#Como regra geral, diz-se que uma população de peixes está "em equilíbrio" se o 
#valor de PSD estiver entre 30 e 70.  
#
#Diferentes intervalos foram propostos para diferentes espécies. A ideia por trás 
#desses alvos intervalos é que, se o valor de PSD for muito baixo, haverá poucos 
#peixes grandes na população, enquanto um valor muito PSD grande indica que há 
#poucos peixes pequenos na população.  
#
#Esses intervalos-alvo devem ser usados com cuidado, porém, como um PSD "em equilíbrio" 
#pode ser produzido a partir de uma ampla variedade de estruturas de tamanho.
#O PSD é um resumo útil, mas não deve ser usado sem também examinar os histogramas 
#de frequência de comprimento.

## Test Kolmogorov–Smirnov


df_ks <- df %>%
  filter(Especie == 'GOR') %>%
  mutate(Ano = as.factor(Ano)) %>%
  select(Ano, Comprim)
df_ks %>% 
  filter(Ano %in% c(2017, 2018)) %>%
  ggplot(aes(x = Comprim, fill = Ano)) +
  geom_density(alpha = 0.5) +
  xlab("Comprimento (cm)") +
  ylab("Density") +
  scale_fill_manual(values = c("gray60", "orangered2")) +
  labs(title = "Comprimento para anos 2017 e 2018",
       #subtitle = "Escala logarítmica, 738 observaciones por año",
       fill = "Ano") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("Dens_Comp_Anos.png")



#Cálculo da função de distribuição cumulativa empírica da amostra

ecdf_2017 <- ecdf(df_ks %>% filter(Ano == 2017) %>% pull(Comprim))
ecdf_2018 <- ecdf(df_ks %>% filter(Ano == 2018) %>% pull(Comprim))

#A probabilidade cumulativa de cada valor observado de comprimento é calculada 
#com cada uma das funções ecdf.
grid_comprim <- unique(df_ks %>% filter(Ano == c(2017,2018)) %>% pull(Comprim))
prob_cum_ecdf_2017 <- ecdf_2017(v = grid_comprim)
prob_cum_ecdf_2018 <- ecdf_2018(v = grid_comprim)


# Vincule os valores calculados em um dataframe.
df_ecdf <- data.frame(
  comprimento = grid_comprim,
  ecdf_2017 = prob_cum_ecdf_2017,
  ecdf_2018 = prob_cum_ecdf_2018
) %>%
  pivot_longer(
    cols = c(ecdf_2017, ecdf_2018),
    names_to = "ano",
    values_to = "ecdf"
  )

grafico_ecdf <- ggplot(data = df_ecdf,
                       aes(x = comprimento, y = ecdf, color = ano)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("gray60", "orangered1")) +
  labs(
    title = "Função de distribuição cumulativa empírica comprimento",
    color = "Ano",
    x = "Comprimento (cm)",
    y = "Probabilidade cumulativa"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(lineheight = 12))

grafico_ecdf

ggsave("Cumulative_function.png")


#Outra forma de representá-lo seria:

df_ks %>%
  filter(Ano %in% c(2017, 2018)) %>%
  ggplot(aes(x = Comprim, color = Ano)) +
  stat_ecdf(geom = "step") +
  labs(title = "Função de distribuição cumulativa empírica comprimento",
       color = "Ano") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=12))

ggsave("Cumulative_function_bis.png")


### Cálculo da distância Kolmogorov-Smirnov
# A distância de Kolmogorov é uma medida usada em estatística e teoria de 
# probabilidade para avaliar a semelhança entre duas distribuições de dados. 
# É usado principalmente para determinar se duas amostras vêm de populações diferentes.

# Calcule a diferença absoluta entre as probabilidades cumulativas de cada
# função.
abs_dif <-  abs(prob_cum_ecdf_2017 - prob_cum_ecdf_2018)

# A distância Kolmogorov–Smirnov é o máximo das distâncias absolutas.
distancia_ks <- max(abs_dif)
paste("Distância Kolmogorov–Smirnov:", round(distancia_ks,4))


#Plot a distância Kolmogorov–Smirnov
indice_ks <- which.max(abs_dif)

grafico_ecdf + 
  geom_segment(aes(
    x = grid_comprim[indice_ks],
    xend = grid_comprim[indice_ks],
    y = prob_cum_ecdf_2017[indice_ks],
    yend = prob_cum_ecdf_2018[indice_ks]
  ),
  arrow = arrow(ends = "both", length = unit(0.2,"cm")),
  color = "black")

ggsave("Cumulative_functions_distance.png")

# A função ks.test() do pacote stats calcula, como parte do teste, a distância 
# Kolmogorov–Smirnov. 
# Verifica-se, a título de validação, que ambos os valores coincidem.

test <- ks.test(
  x = df_ks %>% filter(Ano == 2017) %>% pull(Comprim),
  y = df_ks %>% filter(Ano == 2018) %>% pull(Comprim)
)
round(test$statistic,4)

# Para a estatística de Kolmogorov–Smirnov existem dois tipos de solução:
#   
# Solução analítica (exata): se as amostras forem grandes e não houver vínculos, 
# essa solução é muito mais rápida e gera valores-p exatos. 
# Essa solução é implementada na função ks.test() do pacote stats.
# 
# Solução através de um teste de reamostragem: consiste em simular, por meio de 
# permutações ou bootstrapping, as distâncias Kolmogorov-Smirnov que seriam obtidas 
# se ambas as amostras viessem da mesma distribuição. Uma vez obtidas as simulações, 
# calcula-se a porcentagem de distâncias iguais ou maiores que a observada.


# Solução analítica (exata):

test_ks <- ks.test(
  x = df_ks %>% filter(Ano == 2017) %>% pull(Comprim),
  y = df_ks %>% filter(Ano == 2018) %>% pull(Comprim)
)
test_ks

# (H0 diz que x e y foram retirados da mesma população)
# 
# Em ambos os casos, não há evidências suficientes para considerar que a distribuição 
# de comprimentos variou de um ano para o outro entre 2017 e 2018.


# Solução através de um teste de reamostragem só para comparar 2 anos:

set.seed(123)

test_ks_boot <- Matching::ks.boot(
  Tr = df_ks %>% filter(Ano == 2017) %>% pull(Comprim),
  Co = df_ks %>% filter(Ano == 2018) %>% pull(Comprim),
  alternative = "two.sided",
  nboots = 5000
)

test_ks_boot$ks.boot.pvalue




# Solução através de um teste de reamostragem com barras de status e há mais de 2 anos:
library(pbapply)
set.seed(123)

myfunct <- function(x,y) {
  set.seed(123)
  return(Matching::ks.boot(Tr = x,Co = y,
                           alternative = "two.sided",
                           nboots = 5000)$ks.boot.pvalue)
}

# Inputs necessários
ini <- 1995
fin <- 1997
no_data <- c(1998,2006,2009,2014,2015,2020)


result <- data.frame(Ano1=integer(),
                     Ano2=integer(),
                     p_value=double(),
                     stringsAsFactors = FALSE)

for (i in ini:(fin-1)) {
  
  if (i %in% no_data) next
  
  for (j in i:(fin-1)) {
    
    if ((j+1)  %in% no_data) next
    
    Tr = df_ks %>% filter(Ano == i) %>% pull(Comprim)
    Co = df_ks %>% filter(Ano == j+1) %>% pull(Comprim)
    
    # Finding maximum length
    max_ln <- max(c(length(Tr), length(Co)))
    df_ <- data.frame(Tr = c(Tr,rep(NA, max_ln - length(Tr))),
                      Co = c(Co,rep(NA, max_ln - length(Co))))
    p_value <- pblapply(df_[1], myfunct, df_[2])$Tr
    
    #print(paste("Kolmogorov–Smirnov entre el año",i,"y el año",(j+1), ": ", p_value))
    row <- data.frame(Ano1 = i, Ano2= (j+1), p_value = p_value, stringsAsFactors = FALSE)
    result <- rbind(result,row)
    
  }
}
