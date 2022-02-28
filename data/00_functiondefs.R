
if (!require(pacman)) install.packages("pacman")
library(pacman)

p_load(tidyverse, wesanderson, gtools)



# plot theme --------------------------------------------------------------

theme_tell <- function (base_size = 11, 
                           base_family = "Arial Narrow", 
                           base_line_size = base_size/22, 
                           base_rect_size = base_size/22) 
{
  half_line <- base_size/2
  theme_grey(base_size = base_size, base_family = base_family, 
             base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
    theme(panel.background = element_rect(fill = "white", 
                                          colour = NA), 
          strip.background = element_blank(),
          panel.border = element_rect(fill = NA, 
                                      colour = "grey70", 
                                      size = rel(1)), 
          panel.grid = element_line(colour = "grey87"), 
          panel.grid.major = element_line(size = rel(0.5)), 
          panel.grid.minor = element_line(size = rel(0.25)), 
          axis.ticks = element_line(colour = "grey70", size = rel(0.5)), 
          legend.key = element_rect(fill = "white", colour = NA), 
          #strip.background = element_rect(fill = "grey70", 
          #                                colour = NA), 
          strip.text = element_text(colour = "black", 
                                    size = rel(0.8), 
                                    margin = margin(0.8 * half_line, 
                                                    0.8 * half_line, 
                                                    0.8 * half_line, 
                                                    0.8 * half_line)), 
          complete = TRUE)
}

# display car::Anova() ----------------------------------------------------

my_Anova <- 
  function(mod) {
    car::Anova(mod) %>%
    rownames_to_column(var = "effect") %>% 
    mutate(
      Chisq = round(Chisq, 
                    digits = 2),
      sig. = stars.pval(`Pr(>Chisq)`),
      `Pr(>Chisq)` = round(`Pr(>Chisq)`,
                         digits=4)
    )
  }

# bing when done ----------------------------------------------------------

done <- function() {
  beepr::beep(sound = "coin")
}


# test lx effects given dv and dat ----------------------------------------

test_lx_effects <- function(dv, dat) {
  results <- tibble()
  ivs <- c("duration", "PFE_pl", "PFE_vm", "FFE_pl",
           "FFE_vm")
  for (i in 1:length(ivs)) {
    mod_formula <- formula(paste(dv, "~", ivs[i]))
    mod <- lm(mod_formula, data = dat)
    results <- bind_rows(results,
                         broom::glance(mod))
  }

  results <- results %>% 
    mutate(effect = ivs,
           sig. = stars.pval(p.value),
           p.value = round(p.value, digits=3),
           r.squared = format(r.squared, digits=2),
           adj.r.squared = format(adj.r.squared, digits=2),
           AIC = round(AIC, digits=1) %>% format(big.mark=","),
           deviance = format(deviance, digits=4, big.mark=",")
           ) %>% 
    select(effect, p.value, sig., everything()) %>% 
    select(-4, -5, -BIC, -sigma, -statistic, -logLik, -df.residual, -nobs) %>% 
    arrange(deviance)
  return(results)
}


# test demographic effects given dv and dat -------------------------------

test_demo_effects <- function(dv, dat) {
  results <- tibble()
  ivs <- c("jcan_imm", "jcan_rec_imm", "jcan_eth_orig", 
           "carib_eth_orig", "black", "vis_min")
  for (i in 1:length(ivs)) {
    mod_formula <- formula(paste(dv, "~", ivs[i]))
    mod <- lm(mod_formula, data = dat)
    results <- bind_rows(results,
                         broom::glance(mod))
  }
  results <- results %>% 
    mutate(effect = ivs,
           sig. = stars.pval(p.value),
           p.value = round(p.value, digits=3),
           r.squared = format(r.squared, digits=2),
           adj.r.squared = format(adj.r.squared, digits=2),
           AIC = round(AIC, digits=1) %>% format(big.mark=","),
           deviance = format(deviance, digits=4, big.mark=",")
           ) %>% 
    select(effect, p.value, sig., everything()) %>% 
    select(-4, -5, -BIC, -sigma, -statistic, -logLik, -df.residual, -nobs) %>% 
    arrange(deviance)
  return(results)
}

# make flextable ----------------------------------------------------------

my_ft <- function(df) {
  library(flextable)
  library(magrittr)
  df %>% 
    flextable() %>% 
    autofit() %>% 
    font(fontname = "Consolas") %>% 
    font(fontname = "Consolas", part = "header") %>% 
    fontsize(size = 10) %>% 
    border_inner_h()
}

# getspeakervowel() -------------------------------------------------------

getspeakervowel <- function(d, s, v){
  
  if (nrow(unique(d %>% select(speaker))) > 1) {
    d <- d                              %>% 
      filter(speaker == s) 
  }
  
  d %>% 
    filter(vowel == v, timepoint == 50) %>% 
    summarize(f1lob = mean(f1_lobanov),
              f2lob = mean(f2_lobanov)) %>% 
    as.list()
}


# notin -------------------------------------------------------------------

`%notin%` <- Negate(`%in%`)


# get vowel space centroid ------------------------------------------------

getcentroid <- function(d, s){
  fleece     <- getspeakervowel(d, s, "FLEECE")           %>% 
    as_tibble()
  trapbath   <- getspeakervowel(d, s, "TRAP/BATH")        %>% 
    as_tibble()
  lotpalm    <- getspeakervowel(d, s, "LOT/PALM/START")   %>% 
    as_tibble()
  goose      <- getspeakervowel(d, s, "GOOSE")            %>% 
    as_tibble()
  rbind(fleece, trapbath, lotpalm, goose)                 %>%
    summarize(f1lob = mean(f1lob),
              f2lob = mean(f2lob))                          %>%
    as.list() 
}


# get Cartesian distances -------------------------------------------------

cdis <- function(x1, y1, x2, y2){
  cart_dis <- sqrt((x1-x2) ^ 2 + (y1-y2) ^ 2)
  return(cart_dis)
}

getCD1 <- function(v1, v2){
  return(cdis(nth(v1, 1),nth(v1, 2),nth(v2, 1),nth(v2, 2)))
}

getCD2 <- function(d, s, v1, v2){
  dat <- d                    %>% 
    filter(name == s)
  vowel1 <- dat               %>% 
    getspeakervowel(s, v1)    %>% 
    as.numeric()
  vowel2 <- dat               %>% 
    getspeakervowel(s, v2)    %>% 
    as.numeric()
  
  return(getCD1(vowel1,vowel2))
}


# code phonetic environments ----------------------------------------------

## preceding --------------------------------------------------------------

code_PFE_pl <- function(dat){
  vowelz <- c("AA", "AE", "AH", "AO", 
              "AW", "AY", "EH", "ER", 
              "EY", "IH", "IY", "OW", 
              "OY", "UH", "UW")
  dat <- dat %>% 
    mutate(
      auxseg = str_sub(preSeg, 1, 2),
      PFE_pl = 
        case_when(
          auxseg %in% vowelz ~ "vow",
          auxseg %in% c("M", "B", "P", "F", "V", "W") ~ "lab", 
          auxseg %in% c("Z", "S", "TH", 
                        "DH", "ZH", "JH", 
                        "SH", "N", "D", "CH",
                        "T", "R", "L", "Y") ~ "cor", 
          auxseg %in% c("NG", "K", "G") ~ "vel",
          auxseg %in% c("HH") ~ "pha",
          TRUE ~ "pau"
        )
    ) %>% 
    select(-auxseg)
  return(dat)
}

code_PFE_vm <- function(dat){
  vowelz <- c("AA", "AE", "AH", "AO", 
              "AW", "AY", "EH", "ER", 
              "EY", "IH", "IY", "OW", 
              "OY", "UH", "UW")
  dat <- dat %>% 
    mutate(auxseg = str_sub(preSeg, 1, 2),
           PFE_vm = 
             case_when(
               auxseg %in% vowelz ~ "vow",
               auxseg %in% 
                 c("F", "S", "SH", "CH", "TH", "HH") ~ "vlf",
               auxseg %in%
                 c("V", "Z", "ZH", "DJ", "DH") ~ "vdf",
               auxseg %in%
                 c("P", "T", "K") ~ "vls", 
               auxseg %in% c("B", "D", "G") ~ "vds",
               auxseg %in% c("M", "N", "NG") ~ "nas",
               auxseg %in% c("L", "R") ~ "liq",
               auxseg %in% c("W", "Y") ~ "gld", 
               TRUE ~ "pau"
             )
    ) %>%
    select(-auxseg)
  return(dat)
}


## following --------------------------------------------------------------

code_FFE_pl <- function(dat){
  vowelz <- c("AA", "AE", "AH", "AO", 
              "AW", "AY", "EH", "ER", 
              "EY", "IH", "IY", "OW", 
              "OY", "UH", "UW")
  dat <- dat %>% 
    mutate(auxseg = str_sub(folSeg, 1, 2),
           FFE_pl = 
             case_when(
               auxseg %in% vowelz ~ "vow",
               auxseg %in% c("M", "B", "P", "F", "V", "W") ~ "lab", 
               auxseg %in% c("Z", "S", "TH", 
                             "DH", "ZH", "JH", 
                             "SH", "N", "D", "CH",
                             "T", "R", "L", "Y") ~ "cor", 
               auxseg %in% c("NG", "K", "G") ~ "vel",
               auxseg %in% c("HH") ~ "pha",
               TRUE ~ "cod"
             )
    ) %>% 
    select(-auxseg)
  return(dat)
}

code_FFE_vm <- function(dat){
  vowelz <- c("AA", "AE", "AH", "AO", 
              "AW", "AY", "EH", "ER", 
              "EY", "IH", "IY", "OW", 
              "OY", "UH", "UW")
  dat <- dat %>% 
    mutate(auxseg = str_sub(folSeg, 1, 2),
           FFE_vm = 
             case_when(
               auxseg %in% vowelz ~ "vow",
               auxseg %in% 
                 c("F", "S", "SH", "CH", "TH", "HH") ~ "vlf",
               auxseg %in%
                 c("V", "Z", "ZH", "DJ", "DH") ~ "vdf",
               auxseg %in%
                 c("P", "T", "K") ~ "vls", 
               auxseg %in% c("B", "D", "G") ~ "vds",
               auxseg %in% c("M", "N", "NG") ~ "nas",
               auxseg %in% c("L", "R") ~ "liq",
               auxseg %in% c("W", "Y") ~ "gld", 
               TRUE ~ "cod"
             )
    ) %>% 
    select(-auxseg)
  return(dat)
}


# plot continuous dv by categorical iv ------------------------------------

plotCatCont <- function(iv, dv){
  nlevels <- length(df                       %>% 
                      pull(as.name(iv))      %>% 
                      unique())
  pal <- wes_palette(nlevels, 
                     name = "Zissou1", 
                     type = "continuous")
  f <- as.formula(paste(dv, iv, sep = "~"))
  m <- lm(f, data = df)
  pval  <- broom::glance(m, width = Inf) %>%
    pull(p.value)
  stars <- stars.pval(pval)
  if (pval < 0.0001) {
    pval <- paste0(" < 0.0001") } else {
      pval <- paste0(" = ", format(pval,
                                   digits = 4,
                                   nsmall = 4,
                                   scientific = F))
    }
  df <- df                                   %>% 
    select(sym(iv), sym(dv))                 %>% 
    na.omit()
  ordervec <- 
    df                                       %>% 
    group_by(!!sym(iv))                      %>%  
    summarise(mean = median(!!sym(dv)))      %>% 
    arrange(desc(mean))                      %>% 
    pull(1)
  p <-
    df                                       %>% 
    ggplot(aes(
      x = factor(!!sym(iv), 
                 levels = ordervec),
      y = !!sym(dv),
      fill = !!sym(iv)))                       +
    #scale_y_log10()                           +
    geom_jitter(width = 0.45, 
                alpha = 0.37, 
                size = 0.5,
                color = dotcol)                +
    geom_boxplot(
      notch = F,
      notchwidth = 0.7,
      outlier.shape = NA,
      color = "white",
      alpha = 0.6
    )                                          +
    scale_fill_manual(values = pal)            +
    theme_tell() +
    labs(title = str_to_title(iv),
         subtitle = bquote(italic('p')~.(pval)~.(stars)),
         y = NULL, 
         x = NULL)                             +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 9),
          panel.grid.major.x = element_blank(),
          legend.position="none"
    )
  return(p)
}


# plotCatCont_fill() ------------------------------------------------------

plotCatCont_fill <- function(iv, dv, filler){
  nlevels <- length(df                       %>% 
                      pull(as.name(filler))  %>% 
                      unique())
  pal <- wes_palette(nlevels, 
                     name = "BottleRocket2", 
                     type = "continuous")
  f <- as.formula(paste(dv, iv, sep = "~"))
  m <- lm(f, data = df)
  pval  <- broom::glance(m, width = Inf) %>%
    pull(p.value)
  stars <- stars.pval(pval)
  if (pval < 0.0001) {
    pval <- paste0(" < 0.0001") } else {
      pval <- paste0(" = ", format(pval, 
                                   digits = 4, 
                                   nsmall = 4,
                                   scientific = F))
    }
  df <- df                                   %>% 
    select(sym(iv), sym(dv), sym(filler))    %>% 
    na.omit()
  ordervec <- 
    df                                       %>% 
    select(-!!sym(filler))                   %>% 
    group_by(!!sym(iv))                      %>%  
    summarise(mean = median(!!sym(dv)))      %>% 
    arrange(desc(mean))                      %>% 
    pull(1)
  p <-
    df                                       %>% 
    ggplot(aes(
      x = factor(!!sym(iv), 
                 levels = ordervec),
      y = !!sym(dv),
      fill = !!sym(filler)))                   +
    #scale_y_log10()                           +
    geom_jitter(width = 0.45, 
                alpha = 0.37, 
                size = 0.5,
                color = dotcol)                +
    geom_boxplot(
      notch = F,
      notchwidth = 0.7,
      outlier.shape = NA,
      color = "white",
      alpha = 0.6
    )                                          +
    scale_fill_manual(values = pal)            +
    theme_tell() +
    labs(title = str_to_title(iv),
         subtitle = bquote(italic('p')~.(pval)~.(stars)),
         y = NULL, 
         x = NULL)                             +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 9),
          panel.grid.major.x = element_blank(),
          legend.position="none"
    )
  return(p)
}


# plot continuous dv by continuous iv -------------------------------------

plotContCont <- function(iv, dv, jitter = FALSE){
  pal <- wes_palette(4, 
                     name = "Rushmore1", 
                     type = "discrete")
  f <- as.formula(paste(dv, iv, sep = "~"))
  m <- lm(f, data = df)
  pval  <- broom::glance(m, width = Inf)     %>%
    pull(p.value)
  stars <- gtools::stars.pval(pval)
  if (pval < 0.0001) {
    pval <- paste0(" < 0.0001") } else {
      pval <- paste0(" = ", format(pval, 
                                   digits = 4, 
                                   nsmall = 4,
                                   scientific = F))
    }
  p <- df                                    %>% 
    select(iv, dv)                           %>% 
    na.omit                                  %>% 
    ggplot(aes(x = !!sym(iv), 
               y = !!sym(dv),
               color = !!sym(iv)))             +
    scale_color_gradient(low = pal[1], 
                         high = pal[3])        +
    theme_tell()  +
    labs(title = str_to_title(iv),
         subtitle = bquote(italic('p')~.(pval)~.(stars)),
         y = NULL, 
         x = NULL)                             +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 9),
          panel.grid.major.x = element_blank(),
          legend.position = "none")
  if (jitter == TRUE) {
    p <- p + geom_jitter(width = 0.5, 
                         alpha = 0.35,
                         size = 0.5)
  } else {
    p <- p + geom_point(alpha = 0.35,
                        size = 0.5)
  }
  p <- p + stat_smooth(geom = "line",
                       method = "loess",
                       size = 1.3,
                       alpha = 0.4,
                       se = FALSE,
                       color = pal[4])
  return(p)
}


# standard rules for exclusion of tokens in disqualif. env. ---------------

standardExclude <- function(dat){
  nrowpre <- nrow(dat)
  dat <- dat %>%
    dplyr::filter(
      PFE_vm != "gld",
      preSeg != "R",
      FFE_vm %notin% c("liq", "gld"),
      vStress != 0
    )
  nrowpost <- nrow(dat)
  return(dat)
  cat("Excluded", 
      format(nrowpre-nrowpost, 
             big.mark = ","), 
      "rows.\n\n")
}


# standard exclude but preserve unstressed tokens -------------------------

standardExclude2 <- function(dat){
  nrowpre <- nrow(dat)
  dat <- dat %>%
    dplyr::filter(
      PFE_vm != "gld",
      preSeg != "R",
      FFE_vm %notin% c("liq", "gld")
    )
  nrowpost <- nrow(dat)
  return(dat)
  cat("Excluded", 
      format(nrowpre-nrowpost, 
             big.mark = ","), 
      "rows.\n\n")
}


# get Pillai via manova ---------------------------------------------------

pillai <- function(...) {
  summary(manova(...))$stats["following_nasal","Pillai"]
}


# test for log conv -------------------------------------------------------

testLogConvert <- function(dat, predvec, resp){
  
  # set up vector of iv names
  ivvec <- c()
  for (p in predvec){
    q = p %>% as.integer()
    r = dat %>% colnames() %>% nth(q)
    ivvec <- c(ivvec, r)
  }
  
  # format dv name
  dv <- resp %>% as.integer()
  dv = dat %>% colnames() %>% nth(dv)
  
  cat("\nRESPONSE:", 
      dv, 
      "\nShould you log-convert these predictors in a model?")
  
  # loop over iv names and test
  for (i in ivvec){
    ichar <- nchar(i)
    x <- 12-ichar
    if (x < 0) x = 0
    f1 <- as.formula(paste(dv, i, sep = "~"))
    m1 <- glm(f1, data = dat); dev_m1 <- m1$deviance
    f2 <- as.formula(paste0(dv, "~ log(", i, ")"))
    m2 <- glm(f2, data = dat); dev_m2 <- m2$deviance
    if (dev_m2 < dev_m1){
      cat(
        "\npredictor", strrep(" ", x), 
        i,
        " - convert to",
        paste0("log(", i, ")"))
    } else {
      cat("\npredictor", strrep(" ", x),  
          i,
          " - keep as is")
    }
  }
  cat("\n\n")
}

