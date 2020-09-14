library("httr")
library("tidyverse")

################################################################################
# 1. Reading data
################################################################################

chromia_auction <- GET("https://hedget.com/auth-serv/bids/chr") %>%
  content() %>%
  read.table(
    text=.,
    sep=",",
    col.names=c("price", "n", "address"),
    colClasses=c("character", "character", "character")) %>%
  as_tibble() %>%
  mutate(
    n = as.numeric(n),
    price = as.numeric(price),
  ) %>%
  filter(price > 231.6/1.1) %>%
  group_by() %>%
    mutate(
      total_n = sum(n)
    ) %>%
  ungroup() %>%
  arrange(desc(price)) %>%
  mutate(
    rank = 1:nrow(.),
    cum_n = cumsum(n),
    relcum_n = round(cum_n/total_n, 2)
  )
View(chromia_auction)

usdt_auction <- GET("https://hedget.com/auth-serv/bids/usdt") %>%
  content() %>%
  read.table(
    text=.,
    sep=",",
    col.names=c("price", "n", "address"),
    colClasses=c("character", "character", "character")) %>%
  as_tibble() %>%
  mutate(
    n = as.numeric(n),
    price = as.numeric(price),
  ) %>%
  filter(price > 11.52/1.1) %>%
  arrange(desc(price)) %>%
  group_by() %>%
    mutate(
      total_n = sum(n)
  ) %>%
  ungroup() %>%
  arrange(desc(price)) %>%
  mutate(
    rank = 1:nrow(.),
    cum_n = cumsum(n),
    relcum_n = round(cum_n/total_n, 2)
  )
View(usdt_auction)

################################################################################
# 2. Various analyses
################################################################################

# Identifying whales vs shrimps
chromia_auction %>%
  ggplot(., aes(x=n, y=price, colour=n*price)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(5))

usdt_auction %>%
  filter(price < 100) %>%
  ggplot(aes(x=n, y=price, colour=n*price)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(5))

# Plotting the price vs cumulative distribution
chromia_auction %>%
  ggplot(aes(x=cum_n/1000, y=price)) +
  geom_line()

usdt_auction %>%
  filter(price < 100) %>%
  ggplot(aes(x=cum_n/1000, y=price)) +
  geom_line()

# Checking the total number of coins bidded by shrimps to whales:

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

nrow_chromia_auction = nrow(chromia_auction)
nrow_usdt_auction = nrow(usdt_auction)

total_n_chromia_auction = sum(chromia_auction$n)
total_n_usdt_auction = sum(usdt_auction$n)

chromia_auction_summary <- chromia_auction %>%
  mutate(
    category = case_when(
      n <= 1000 ~ "1. shrimp: 0 <= n <= 1000",
      n <= 5000 ~ "2. fish: 1000 < n <= 5000",
      n <= 10000 ~ "3. shark: 5000 < n <= 20000",
      TRUE~ "4. whale: n > 20000"
    )
  ) %>%
  group_by(category) %>%
  summarise(
    count = n(),
    count_perc = round(n()*100/nrow_chromia_auction, 2),
    total_n = sum(n),
    n_perc = round(sum(n)*100/total_n_chromia_auction, 2),
    mean_price = mean(price),
    median_price = median(price),
    mode_price = get_mode(price),
    min_price = min(price),
    max_price = max(price),
    sd_price = sd(price)) %>%
  arrange(category)
View(chromia_auction_summary)

usdt_auction_summary <- usdt_auction %>%
  mutate(
    category = case_when(
      n <= 1000 ~ "1. shrimp: 0 <= n <= 1000",
      n <= 5000 ~ "2. fish: 1000 < n <= 5000",
      n <= 10000 ~ "3. shark: 5000 < n <= 20000",
      TRUE~ "4. whale: n > 20000"
    )
  ) %>%
  group_by(category) %>%
  summarise(
    count = n(),
    count_perc = round(n()*100/nrow_usdt_auction, 2),
    total_n = sum(n),
    n_perc = round(sum(n)*100/total_n_usdt_auction, 2),
    mean_price = mean(price),
    median_price = median(price),
    mode_price = get_mode(price),
    min_price = min(price),
    max_price = max(price),
    sd_price = sd(price)) %>%
  arrange(category)

View(usdt_auction_summary)

# Identifying potentially dangerous players
chromia_auction %>%
  filter(n >= 1000, price < 231.60) %>%
  arrange(desc(price)) %>%
  group_by() %>%
  mutate(
    total_n = sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    cum_n = cumsum(n),
    relcum_n = round(cum_n/total_n, 2)
  ) %>%
  View()

usdt_auction %>%
  filter(n >= 1000, price < 11.52) %>%
  arrange(desc(price)) %>%
  group_by() %>%
  mutate(
    total_n = sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    cum_n = cumsum(n),
    relcum_n = round(cum_n/total_n, 2)
  ) %>%
  View()


# Entre más whales/sharks haya, más competitivo porque tienen más liquidez
# adicional para soportar sus apuestas. Es decir, entre más whales haya, más
# prioridad le tengo que dar a "defender" (ganar algo) y menos a "atacar"
# (ganar lo más posible)

# El precio en la primera etapa es crucial: demasiado bajo y puedo quedar fuera
# por default. Demasiado alto y compro menos...

# Creo que hay dos posibles estrategias:
# 1. Decidir a priori cuantas monedas quiero y tratar de asegurarlas aumentando
# el precio. Obviamente monitorear constantemente y más antes del deadline.
# 2. Más riesgosa: ir disminuyendo la cantidad de monedas que puedo comprar poco
# a poco conforme aumenta el precio.
# Creo que prefiero la primera...
# Revisar el precio 1:15 antes del deadline y luego 15min a 5 min antes para
# actualizar en caso de pánico.

# Creo que lo más inteligente es revisar antes del deadline y meter todo el usdt
# en chromia, asegurando HGET y comprando una cantidad decente: si lo tengo
# distribuido, puedo asegurar menos y tener una cantidad menos decente

# Mañana decidir si es necesario hacer un depósito adicional en Chromia. En caso
# afirmativo, si saco de Holo o deposito de mi tarjeta de débito (es un préstamo
# que voy a pagar cuando pueda retirar mi usdt).

# ((((25/1.1)/1.3)/1.5)/1.7)/1.9

# 68810.007 / 474147.107 8am
# 77776.310 / 492694.547 9am

# Estrategia: voy a ser conservador. Le tiro a un precio de (500+56*4.2)/100
# $7.35 por HGET, entonces, a menos que vea que el precio threshold está
# muy por debajo de ese, voy a poner ese precio como bid. Es decir,
# 7.352/0.052 140-150 CHR por HGET y 8USD por HGET.

# Estrategia: sigo jugando conservadoramente, mi meta es asegurar 100-200 HGET...
# Ya no pido más.

# Update: tengo que ser más conservador porque los increases dependen de mi bid
# inicial, no de mi bid del día anterior...Tomarlo MUCHO en cuenta. Creo que voy
# bien pero es mejor no correr riesgos...

# Precios iniciales: $10 y 130 CHR. Creo que para asegurar, tengo que estar
# mìnimo como $10 CHR arriba del threshold (para USD no hay mucho problema). Al
# parecer, mi jugada de subir hasta 215 CHR en el segundo día fue Excelente...

# En el tercer día subo mi apuesta a máximo de dólares que me alcance para
# asegurar 95 y al máximo de CHR para asegurar mínimo 250HGET... No creo que
# llegue tan alta la verdad, pero así aseguro y el resto lo vendo...

# Al final del penúltimo día: 225 CHR y $11.25 por HGET.



