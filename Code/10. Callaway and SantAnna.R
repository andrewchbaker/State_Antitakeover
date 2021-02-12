library(tidyverse)
library(fixest)
library(ggthemes)
library(fastDummies)
library(lubridate)
library(pbapply)
library(parallel)
library(tictoc)

# set ggplot theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

options(knitr.kable.NA = '')
dropbox <- ""
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# load the cleaned data
data <- read_rds(here::here("Data/COMPILED", "data4.rds"))

# save long covs and short covs
long_covs <- c("size", "age", "size2", "age2", "gen1", "cs", "dd", 
               "fp")

short_covs <- c("size", "age", "size2", "age2")

# put the dependent variables into a vector (only doing three here)
depvars <- c("roa", "sga", "leverage")

# keep just the data we need 
data <- data %>% 
  select(gvkey, year, datadate, firm, state, incorp, incorporation, state_year, starts_with("industry"), all_of(long_covs), 
         bc, pp, all_of(depvars), bc_date, pp_date) %>% 
  filter(year %>% between(1976, 1995))

# add a row id for the influence functions
data$rowid <- 1:nrow(data)

# sub functions we need to make standard errors
# function to combine estimation error from each group
wif <- function(keepers, pg, G, group, glist) {

  # effect of estimating weights in the numerator
  if1 <- sapply(keepers, function(k) {
    #Get the corresponding entry in the Gindics matrix
    GG <- G[, match(group[k], glist)]
    
    (1*GG - pg[k]) /
      sum(pg[keepers])
  })
  # effect of estimating weights in the denominator
  if2 <- rowSums( sapply( keepers, function(k) {
    #Get the corresponding entry in the Gindics matrix
    GG <- G[, match(group[k], glist)]
    
    1*GG - pg[k]
  })) %*%
    t(pg[keepers]/(sum(pg[keepers])^2))
  
  # return the influence function for the weights
  if1 - if2
}

# Get an influence function for particular aggregate parameters
get_agg_inf_func <- function(att, inffunc1, whichones, weights.agg, wif=NULL) {
  
  # enforce weights are in matrix form
  weights.agg <- as.matrix(weights.agg)
  
  # multiplies influence function times weights and sums to get vector of weighted IF (of length n)
  thisinffunc <- inffunc1[,whichones]%*%weights.agg
  
  # Incorporate influence function of the weights
  if (!is.null(wif)) {
    thisinffunc <- thisinffunc + wif%*%as.matrix(att[whichones])
  }
  
  # return influence function
  return(thisinffunc)
}

# Function to ake influence function and return standard errors
getSE <- function(thisinffunc, DIDparams=NULL) {
  alp <- .05
  bstrap <- TRUE
  cband <- FALSE
  n <- length(thisinffunc)
  if (bstrap) {
    bout <- mboot(thisinffunc, DIDparams)
    return(bout$se)
  } else {
    return(sqrt( mean((thisinffunc)^2)/n ))
  }
}

# define parameters for the mboot function
dt_ids <- list(
  data = as.data.frame(data),
  idname = "gvkey",
  clustervars = NULL,
  biters = 1000,
  tname = "year",
  alp <- 0.05,
  panel = FALSE,
  true_repeated_cross_sections = FALSE
)

# source the mboot function (this comes from the did package)
source(here::here("Code/did/R/mboot.R"))

# function to run CS with PP
run_cs <- function(dep, covs) {
  
  # get model type
  covtype <- if(identical(covs, NULL)) "Model 1" else
    if(identical(covs, short_covs)) "Model 2" else "Model 3"

  if(covtype == "Model 3") {
    covs = c(covs, "bc")
  }
  
  # stupid code to grab names at the end
  names <- data %>% 
    select(pp, {{dep}}) %>% 
    slice(1) %>% 
    pivot_longer(cols = c(pp, {{dep}})) %>% 
    pull(name)
  
  # First get a list of valid treatment years - in which 1) the law turns on, 2) there is only one year in separation before the prior
  # observation for a given firm, and 3) the law turned on that year because it was passed in the fiscal year
  treats <- data %>% 
    select(gvkey, year, pp, pp_date, datadate) %>% 
    # identify years in which 1) the law turns on, 2) there is only one year in separation before the prior
    # observation for a given firm, and 3) the law turned on that year because it was passed in the fiscal year
    mutate(switch = if_else(pp == 1 & lag(pp) == 0 & gvkey == lag(gvkey) & 
                              year - lag(year) == 1 & 
                              pp_date <= datadate & pp_date > coalesce(lag(datadate), datadate - years(1)), 1, 0)) %>% 
    # keep a list of just the valid treatment years
    filter(switch == 1 & year < 1995) %>% 
    select(gvkey, treat_year = year)
  
  # get list of treated group years
  glist <- sort(unique(treats %>% pull(treat_year)))
  
  # get the number of groupds and the number of relative time periods
  nG <- length(glist)
  nT <- length(-5:5)
  
  # matrix to store the influence functions in - set to 0 for now
  inffunc <- Matrix::Matrix(data=0,nrow=nrow(data), ncol=nG*nT, sparse=TRUE)
  
  # matrix to save G indicators
  Gindics <- Matrix::Matrix(data=0,nrow=nrow(data), ncol=nG, sparse=TRUE)
  
  # will populate with all att(g,t) information
  attgt.list <- list()
  counter <- 0
  
  # loop over our four treatment groups
  for (g in 1:length(glist)) {
    
    # get the group name
    treatgroup <- glist[g]
    
    # get treat ids for this year
    tt <- treats %>% filter(treat_year == treatgroup) %>% pull(gvkey)
    
    # make a shorter dataset - it has all treated units with data in t - 5 and t + 5
    # and all control units not treated yet within t + 5
    
    # first make sure that treated units have full observations from t - 1 to t + 1
    tt <- data %>% 
      # firms already identified as treateds
      filter(gvkey %in% tt) %>% 
      # keep just variables we need 
      select(gvkey, year, {{dep}}, all_of(covs), pp) %>% 
      drop_na() %>% 
      # keep just years in -1 to + 1
      filter(year %>% between(treatgroup - 1, treatgroup + 1)) %>% 
      # make sure you have three obs with pp = 0,1,1
      group_by(gvkey) %>% 
      filter(length(gvkey) == 3) %>% 
      filter(pp == c(0, 1, 1)) %>% 
      pull(gvkey) %>% unique()
    
    # get controls - these are all firms that aren't in the treated group, and also have full covs over 
    # t - 1 to t + 1
    cc <- data %>% 
      # keep just variables we need 
      select(gvkey, year, {{dep}}, all_of(covs), pp) %>% 
      drop_na() %>% 
      # drop treated and non-control observations
      filter(!(gvkey %in% tt) & year %>% between(treatgroup - 1, treatgroup + 1) & pp == 0) %>% 
      # make sure you have three obs with pp = 0,0,0
      group_by(gvkey) %>% 
      filter(length(gvkey) == 3) %>% 
      filter(pp == c(0, 0, 0)) %>% 
      pull(gvkey) %>% unique()
    
    # make dataset with identified control and treated obs
    smalldata <- data %>% 
      # keep just variables we need 
      select(gvkey, year, {{dep}}, all_of(covs), pp, rowid) %>% 
      drop_na() %>% 
      # keep only years t - 6 to t + 5 for our treated and controls
      filter(year %>% between(treatgroup - 6, treatgroup + 5) & gvkey %in% c(tt, cc)) %>% 
      # mark treateds and controls
      mutate(G = (gvkey %in% tt),
             C = (gvkey %in% cc)) %>% 
      # finally require that the law variable stays constant - if it turns in the pre period for any 
      # treated units, drop all obs in that year and earlier. If it turns off post-treatment, drop that 
      # obs and all afterwards. We just want 000011111 type treatment paths. For controls if it turns on
      # in the pre period drop all observations beforehand - if it turns on in the post period drop all after, 
      # we just want treatment paths that look like 00000000.
      group_by(gvkey) %>% 
      mutate(min = if_else(sum(pp[which(year < treatgroup)]) > 0, max(year[which(pp == 1 & year < treatgroup)]), 0),
             max = case_when(
               G == TRUE ~ if_else(length(which(pp == 0 & year >= treatgroup)) > 0, min(year[which(pp == 0 & year >= treatgroup)]), Inf),
               G == FALSE ~ if_else(length(which(pp == 1 & year >= treatgroup)) > 0, min(year[which(pp == 1 & year >= treatgroup)]), Inf)
             )) %>% 
      ungroup() %>% 
      filter(year > min & year < max) %>% 
      select(-c(min, max))
    
    ## Save the G indicator variables 
    n <- nrow(data)
    GG <- rep(0, n)
    
    # get rowids where treat = TRUE
    ids <- smalldata %>% filter(G == TRUE) %>% pull(rowid)
    
    # set those to 1
    GG[ids] <- 1
    
    # fill in the column
    Gindics[, g] <- GG
    
    # loop over the relative time periods
    for (i in max(-5, min(smalldata$year - treatgroup)):max(smalldata$year - treatgroup)) {
      
      counter <- counter + 1
      
      # store the relevant year
      cal_year <- treatgroup + i
      
      # make reference year - lagged year if before treatment, otherwise t - 1
      if (i < 1) {
        ref_year <- cal_year - 1
      } else {
        ref_year <- treatgroup - 1
      }
      
      # keep a data set that has just the observations that we want
      regdata <- smalldata %>% 
        # keep only obs in the calendar year and reference year
        filter(year %in% c(cal_year, ref_year)) %>% 
        select(-covs) %>% 
        # join in covariates - use just the values from the reference year
        left_join(smalldata %>% filter(year == ref_year) %>% select(gvkey, covs), by = "gvkey") %>% 
        drop_na()
      
      # give short names for data in this iteration
      G <- regdata$G
      C <- regdata$C
      Y <- regdata %>% pull({{dep}})
      post <- 1*(regdata$year == cal_year)
      
      # num obs. for computing ATT(g,t)
      n1 <- sum(G+C)
      
      # checks to make sure that we have enough observations
      skip_this_att_gt <- FALSE
      if ( sum(G*post) == 0 ) {
        #warning(paste0("No units in group ", glist[g], " in time period ", tlist[t+1]))
        skip_this_att_gt <- TRUE
      }
      if ( sum(G*(1-post)) == 0) {
        #warning(paste0("No units in group ", glist[g], " in time period ", tlist[t]))
        skip_this_att_gt <- TRUE
      }
      if (sum(C*post) == 0) {
        #warning(paste0("No available control units for group ", glist[g], " in time period ", tlist[t+1]))
        skip_this_att_gt <- TRUE
      }
      if (sum(C*(1-post)) == 0) {
        #warning(paste0("No availabe control units for group ", glist[g], " in time period ", tlist[t]))
        skip_this_att_gt <- TRUE
      }
      
      if (skip_this_att_gt) {
        attgt.list[[counter]] <- list(att = NA, group=treatgroup, year=cal_year, relative_year = i, post=(i >= 0), ntreat = sum(G))
        inffunc[,counter] <- NA
        counter <- counter+1
        next
      }
      
      # matrix of covariates
      if (!is.null(covs)) {
        covariates <- model.matrix(as.formula(paste0("~", paste0(covs, collapse = "+"))), data = regdata)
        # identify which covariates do not have 0 variance for the control units in both pre and post period
        covs_var <- intersect(covs[which(apply(covariates[G == 0 & post == 0,], 2, var) > 0) - 1],
                              covs[which(apply(covariates[G == 0 & post == 1,], 2, var) > 0) - 1])
        # new matrix with just those values
        covariates <- model.matrix(as.formula(paste0("~", paste0(covs_var, collapse = "+"))), data = regdata)
      } else {
        covariates = NULL
      }
      
      # regression
      attgt <- DRDID::reg_did_rc(y=Y,
                                 post=post,
                                 D=G,
                                 covariates=covariates,
                                 boot=FALSE,
                                 inffunc=TRUE)
      
      # n/n1 adjusts for estimating the
      # att_gt only using observations from groups
      # G and C
      n <- nrow(data)
      attgt$att.inf.func <- (n/n1)*attgt$att.inf.func
      
      # save results for this att(g,t)
      attgt.list[[counter]] <- list(att=attgt$ATT, group=treatgroup, year=cal_year, relative_year = i, post=(i >= 0), ntreat = sum(G))
      
      # recover the influence function
      # start with vector of 0s because influence function
      # for units that are not in G or C will be equal to 0
      inf.func <- rep(0, n)
      
      # populate the influence function in the right places
      inf.func[regdata$rowid] <- attgt$att.inf.func
      
      # save it in influence function matrix
      # inffunc[g,t,] <- inf.func
      inffunc[,counter] <- inf.func
    } # end looping over t
  }
  
  # get unnested results for the attgts 
  ATTs <- do.call(bind_rows, attgt.list) %>% 
    # make weights for each row which equals the count / sum (count) %>% 
    # here set ntreat = 0 if ATT is missing
    mutate(ntreat = if_else(is.na(att), as.integer(0), ntreat)) %>% 
    group_by(relative_year) %>% 
    mutate(wt = ntreat/sum(ntreat)) %>% 
    ungroup()
  
  ## Set parameters
  att <- ATTs$att
  # save a vector of the weights from the ATT table
  pg <- ATTs$wt
  # save the groups from the ATT table
  group <- ATTs$group
  
  # get se for each year from t - 5 to t + 5
  dynamic.se.inner <- lapply(sort(unique(ATTs$relative_year)), function(e) {
    # which rows of ATT matrix are for relatie time period e
    whiche <- which(ATTs$relative_year == e & !is.na(ATTs$att))
    # what is the relative contribution of each group to this total average
    pge <- ATTs$ntreat[whiche]/sum(ATTs$ntreat[whiche])
    # run the function to add in the estimation error in getting the group
    wif.e <- wif(whiche, pg, Gindics, group, glist)
    inf.func.e <- as.numeric(get_agg_inf_func(att=att,
                                              inffunc1=inffunc,
                                              whichones=whiche,
                                              weights.agg=pge,
                                              wif=wif.e))
    getSE(inf.func.e, dt_ids)
  })
  
  # save the data we need
  ATTs %>% 
    # get weighted average att
    group_by(relative_year) %>% 
    summarize(att = sum(att*wt, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(se = unlist(dynamic.se.inner),
           law = names[1],
           dep = names[2],
           whichcovs = covtype)
}

# parallelize and run over all of our dep variables
# set cores
tic()
cl <- makeCluster(4)

## export data and functions to the clusters
clusterExport(cl, c("data", "dt_ids", "depvars", "long_covs", "short_covs",
                    "get_agg_inf_func", "getSE", "mboot", "wif", "run_cs"))

# export needed programs
clusterEvalQ(cl, c(library(tidyverse), library(lubridate), library(pbapply)))

## run the command on the clusters
pp_out_1 <- do.call(rbind, pblapply(cl = cl, X = depvars, FUN = run_cs, covs = NULL))
pp_out_2 <- do.call(rbind, pblapply(cl = cl, X = depvars, FUN = run_cs, covs = short_covs))
pp_out_3 <- do.call(rbind, pblapply(cl = cl, X = depvars, FUN = run_cs, covs = long_covs))

# bind together
pp_out <- bind_rows(pp_out_1, pp_out_2, pp_out_3)
stopCluster(cl)
toc()

# make plot
depvar_names <- tribble(
  ~dep, ~varname,
  "roa", "ROA",
  "sga", "SGA Expense",
  "leverage", "Leverage"
)

# make ggplot
fig9 <-  pp_out %>% 
  left_join(depvar_names, by = "dep") %>% 
  mutate(conf.low = att - 1.96*se, 
         conf.high = att + 1.96*se) %>% 
  mutate(varname = factor(varname, levels = c("ROA", "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = relative_year, y = att)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(whichcovs), scales = "free", switch = "y")

# save
ggsave(fig9, filename = paste(dropbox, "fig9.png", sep = ""), dpi = 500,
       width = 10, height = 6)
