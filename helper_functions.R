
## Helper Functions

## APA Correlation Matrix
#' correlation_matrix
#' Creates a publication-ready / formatted correlation matrix, using `Hmisc::rcorr` in the backend.
#'
#' @param df dataframe; containing numeric and/or logical columns to calculate correlations for
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param decimal.mark character; which decimal.mark to use; gets passed to `formatC`; defaults to `.`
#' @param use character; which part of the correlation matrix to display; options are `"all"`, `"upper"`, `"lower"`; defaults to `"all"`
#' @param show_significance boolean; whether to add `*` to represent the significance levels for the correlations; defaults to `TRUE`
#' @param replace_diagonal boolean; whether to replace the correlations on the diagonal; defaults to `FALSE`
#' @param replacement character; what to replace the diagonal and/or upper/lower triangles with; defaults to `""` (empty string)
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix(iris)`
#' `correlation_matrix(mtcars)`
correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = ""){
  
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0, na.rm = T) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}

# Processes p-values if rounds to 0, rounding to 3
p_value_func <- function(p_value){
  if(round(p_value,3) < .001) {
    "p < .001"
    } else {
      paste0("p = ", round(p_value,3))
    }
}

# Processes p-values, rounds to 3 digits with only values, no "p=", will handle NA's
p_value_only_func <- function(p_value){
  if(!is.na(p_value)){
    if(round(p_value,3) < .001) {
      "< .001"
      } else {
        as.character(round(p_value,3))
      }
  } else{
    NA
  }
}

p_value_only_func_significance <- function(p_value, round_value = 3){
  if(!is.na(p_value)){
      if(round(p_value, round_value) < .05 & round(p_value, round_value) >= .01) {
        	paste(round(p_value, round_value), "*")
        } else if(round(p_value, round_value) < .01 & round(p_value, round_value) >= .001) {
        	paste(round(p_value, round_value), "**")
        } else if(round(p_value, round_value) < .001) {
        	"< .001 ***"
        } else {
          	as.character(round(p_value,round_value))
        }
  } else{
    	NA
  }
}

###*** Following converts p-value to interpretive value- good for map
                p_val_func <- function(p_val, round_value = 2){
                    data.frame(
                      upper = c(Inf, .05, .01, .001),
                      lower = c(.05, .01, .001, 0),
                      outcome = c(
                          "NS",
                          "*",
                          "**",
                          "< .001 ***"
                        )
                    ) %>% 
                  filter(p_val < upper & p_val >= lower) %>% 
                  dplyr::summarize(out = ifelse(p_val < .001, outcome, paste(round(p_val, round_value), outcome))) %>% 
                  pull()
                }

### In recode changes to factor, orders levels alphabetically, except 'other' or 'another', at end

## To color text in Rmarkdown
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, 
            x)
  } else x
}

## Use to number tables and figures
table_counter_n <<- 0
table_counter_func <- function(table_title){
  table_counter_n <<- table_counter_n + 1
  paste0("*Table* ", table_counter_n, ": ", table_title)
}
figure_counter_n <<- 0
figure_counter_func <- function(figure_title){
  figure_counter_n <<- figure_counter_n + 1
  paste0("*Figure* ", figure_counter_n, ": ", figure_title)
}

getOutputFormat <- function() {
  output <- rmarkdown:::parse_yaml_front_matter(
    readLines(knitr::current_input())
  )$output
  if (is.list(output)){
    return(names(output)[1])
  } else {
    return(output[1])
  }
}

## Use to print tables in any format

library(flextable)
library(kableExtra)

table_output_func <- function(df, colnames, caption = NULL, output_format, size, ...){
  # with html- typically use table_output_func(df, colnames = c("col1", "col2"), caption = table_counter_func("caption")), df req
  # if (output_format == "html"){
  if(knitr::is_html_output()){
    size = NULL
    kable(df, row.names = F, col.names = colnames, caption = caption) %>%
      kable_options()
  } else {
    # with pdf or word use table_output_func(df, colnames = c("col1", "col2"), values = table_counter_func("caption")), df req
    names1 = names(df)
    names2 = if(missing(colnames)){
      names1
    } else {
      colnames
    }
    if (missing(caption)){
      caption <- NULL
    }
    FitFlextableToPage <- function(ft, pgwidth = 6){
      
      ft_out <- ft %>% autofit()
      
      ft_out <- width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable_dim(ft_out)$widths))
      return(ft_out)
    }
    df %>% 
      flextable() %>% 
      set_header_df(x = ., mapping = data.frame(keys = names1, values = names2, stringsAsFactors = FALSE),
                    key = "keys" ) %>%
      add_header_lines(top = TRUE, values = caption) %>% 
      theme_zebra() %>% FitFlextableToPage() %>% fontsize(part = "header", size = size) 
  }
}

      # For kableExtra note printed in APA format
      footnote_func <- function(df = ., footnote_caption = "", i = 1, j = 1) {
        if (isTRUE(getOption('knitr.in.progress'))) {
          if(knitr::is_html_output()){
          # if (opts_knit$get("rmarkdown.pandoc.to") == "html") { # If kable
            df %>% 
              kableExtra::footnote(general = footnote_caption, general_title = "Note. ",
                       footnote_as_chunk = T, title_format = c("italic"))
          } else {
            df %>%
              flextable::footnote(., i = i, j = j, value = as_paragraph(footnote_caption), ref_symbols = c(""))
          }
        } else {
          df %>% 
            kableExtra::footnote(general = footnote_caption, general_title = "Note. ",
                     footnote_as_chunk = T, title_format = c("italic"))
        }
      }
###**** Add header to kable or flextable table
      # For kableExtra note printed in APA format
      header_func <- function(df = ., header_text, col_length) {
        if (isTRUE(getOption('knitr.in.progress'))) {
          if(knitr::is_html_output()){
          # if (opts_knit$get("rmarkdown.pandoc.to") == "html") { # If kable
            df %>% 
              kableExtra::add_header_above(., header = data.frame(names = header_text,
								colspan = col_length)
						)
          } else {
            df %>%
              flextable::add_header_row(., values = header_text, colwidths = col_length)
          }
        } else {
          df %>% 
              kableExtra::add_header_above(., header = data.frame(names = header_text,
								colspan = col_length)
						)
        }
      }

      table_print <- function(df, colnames = "", caption = "", row_names = F, output_format = "html", size = 6, ...){
        colnames <- case_when(
          colnames == "" ~ pretty_columns_func(colnames(df)),
          TRUE ~ colnames
        )
        if (isTRUE(getOption('knitr.in.progress'))) {
          table_output_func(df, colnames, caption, output_format, size, ...)
        } else {
          print("knitr in progrss")
          df %>%
            kable(caption = caption, col.names = colnames, row.names = row_names) %>%
            kable_options(., ...)
        }
      }
      
    scroll_box_func <- function(data, width = 100, border_size = 0){
      data %>% 
        purrr::when(
          knitr::is_html_output()
          ~ scroll_box(., width = paste0(width, "%"), 
                       box_css = paste0("border: ", border_size, "px;")),
          ~ .
        )
    }
  
# Prettify column names
pretty_columns_func <- function(colnames){
	tools::toTitleCase(gsub("[_|.]", " ", colnames))
	}

# Function to generate summary tables- can choose data, grouping, summary variables, and whether to rename
generate_summary_tbl <- function(dataset, group_column, summary_column, useColname = T) {
  group_column   <- enquo(group_column)
  # Uncomment Below if not passing list of quosures for summary variables
  # summary_column <- enquo(summary_column)
  overall_p_value_result <- manova_p_value_results_func(dataset, !!group_column)
  dataset %>% 
    group_by(!!group_column) %>% 
    summarise(
      N = n(),
      mean = round(mean(!!summary_column),2),
      SD  = round(sd(!!summary_column),2),
      SEM = round(psych::describe(!!summary_column)$se, 2)
      # Other metrics that need to be generated frequently
    ) %>% 
    # Get proportion and consolidate some columns
    mutate(grouping_column = !!group_column,
           prop = paste0(round(N/sum(N),4)*100, "%"),
           n_prop = paste0(N," (", prop, ")"),
           mean_sd = paste0(mean, " (", SD, ")"),
    ) %>%
    dplyr::select(grouping_column, n_prop, mean_sd, SEM) %>%
    ungroup -> smryDta
  
  if (useColname) 
    smryDta <- smryDta %>%  
    rename_at(
      vars(-one_of(quo_name(summary_column))), 
      ~paste(quo_name(summary_column), .x, sep="_")
    ) 
  smryDta <- smryDta %>%
    rename_at(1, ~paste(quo_name(group_column), overall_p_value_result))
  
  
  return(smryDta)
}

# Round df to specific digits if numeric
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

# Kable styling
kable_options <- function(.) {kable_styling(.,bootstrap_options = c("striped", "hover", "condensed", "responsive"))}

#### ******** Linear regression stepwise functions- can only use when predictors are numeric

# data <- # name dataset

# Get predictor variables
# pred_var <- data %>%
#   dplyr::select() %>% # input var names to select) %>%
#   colnames()

# Get outcome variables
# outcome_var <- data %>%
#   dplyr::select() %>% # input var names to select) %>%
#   colnames()

# subset data so that it contains only predictor and outcome vars
# lm_data <- data %>%
#   dplyr::select(all_of(pred_var), all_of(outcome_var))


# Constructs lm formula

lm_formula_func <- function(outcome_var, pred_var, data){
  model_formula <- formula(paste0(outcome_var, "~ ", sub(" \\+ $.*", "", paste(sapply(pred_var, function(pred_var) paste(pred_var, "+ ")), collapse = ""))))
  lm(model_formula, data = data) -> mod1
}

# Following performs backwards stepwise regression based on t-values of vars

lm_stepwise_t_func <- function(outcome_var, pred_var, data){
  mod_sum <- summary(lm_formula_func(outcome_var, pred_var, data))
  outcome_msg <- "proceed"
  while (outcome_msg != "No significant predictors" & outcome_msg != "Final model") {
    if (all(mod_sum$coefficients[-1,4] > .05)){
      outcome_msg <- "No significant predictors"
    } else if (all(mod_sum$coefficients[-1,4] <= .05)){
      outcome_msg <- "Final model"
    } else {
      pred_var <- names(mod_sum$coefficients[-1,4][mod_sum$coefficients[-1,4] <= .05])
      mod_sum <- summary(lm_formula_func(outcome_var, pred_var, sand_all))
    } 
  }
  print(outcome_var)
  print(outcome_msg)
  print(mod_sum)
}
# invisible(lapply(outcome_var, function(outcome_var) lm_stepwise_t_func(outcome_var, pred_var, lm_data)))

# Following performs backwards stepwise regression with likelihood test of models
lm_stepwise_func <- function(outcome_var, pred_var, data){
  pred_var <- pred_var[sapply(data[pred_var], function(x) n_distinct(x, na.rm = T) > 1)]
  data <- data %>%
    mutate(across(pred_var, as.character))
  mod_full <- lm_formula_func(outcome_var, pred_var, data)
  mod_full_sum <- summary(mod_full)
  pred_var <- pred_var[pred_var != names(mod_full_sum$coefficients[-1,1][mod_full_sum$coefficients[-1,4] == max(mod_full_sum$coefficients[-1,4])])]
  mod_reduced <- lm_formula_func(outcome_var, pred_var, data = na.omit(data[ , all.vars(formula(mod_full))]))
  full_reduced_anova <- anova(mod_full, mod_reduced)
  if (full_reduced_anova$`Pr(>F)`[-1] > .05){
    outcome_msg <- "proceed" 
  } else {
    outcome_msg <- "No significant predictors"
  }
  while (outcome_msg != "Final Model" & outcome_msg != "No significnat predictors") {
    mod_full <- lm_formula_func(outcome_var, pred_var, data)
    mod_full_sum <- summary(mod_full)
    pred_var <- pred_var[pred_var != names(mod_full_sum$coefficients[-1,1][mod_full_sum$coefficients[-1,4] == max(mod_full_sum$coefficients[-1,4])])]
    mod_reduced <- lm_formula_func(outcome_var, pred_var, data=na.omit(data[ , all.vars(formula(mod_full))]))
    full_reduced_anova <- anova(mod_full, mod_reduced)
    if (full_reduced_anova$`Pr(>F)`[-1] > .05){
      outcome_msg <- "proceed" 
    } else {
      outcome_msg <- "Final Model"
    }
  }
  print(paste0("Results of the linear regression indicated that there was", 
               ifelse(full_reduced_anova$`Pr(>F)`[-1] > .05, " no significant effect", " a significant effect "), 
               "between predictor variables ", paste(row.names(mod_full_sum$coefficients)[-1], collapse = ", "), 
               " and the outcome variable ", outcome_var, " ??2(", abs(full_reduced_anova$Df[-1]), "), ", 
               ifelse(full_reduced_anova$`Pr(>F)`[-1] < .001, "p < .001", 
                      paste0("p = ", round(full_reduced_anova$`Pr(>F)`[-1], 3))), ", R2 = ", round(mod_full_sum$r.squared,3))) 
  
  # Table of coefficients
  mod_full_sum$coefficients %>%
    as_tibble() %>%
    rownames_to_column(., var = "Coefficients") %>% 
    mutate(across(where(is.double), round,3)) %>%
    kable(caption = table_counter_func(paste(outcome_var, outcome_msg))) %>%
    kable_options() %>%
    print()
}
# invisible(lapply(outcome_var, function(outcome_var) lm_stepwise_func(outcome_var, pred_var, lm_data)))



### **** FOllowing is lmer stepwise func

lmer_formula_func <- function(outcome_var, pred_var, rand_var, data){
  model_formula <- formula(paste0(outcome_var, "~ ", paste(pred_var, collapse = " + "), " + ", paste(paste0("(1|", rand_var, ")"), collapse = " + ")))
  lmer(model_formula, data = data, REML = F) -> mod1
}

lmer_stepwise_func <- function(outcome_var, pred_var, rand_var, data){
  mod_full <- lmer_formula_func(outcome_var, pred_var, rand_var, data)
  mod_full_sum <- summary(mod_full)
  pred_var <- pred_var[pred_var != names(mod_full_sum$coefficients[-1,1][mod_full_sum$coefficients[-1,4] == max(mod_full_sum$coefficients[-1,4])])]
  mod_reduced <- lmer_formula_func(outcome_var, pred_var, rand_var, data = na.omit(data[ , all.vars(formula(mod_full))]))
  full_reduced_anova <- lrtest(mod_full, mod_reduced)
  if (full_reduced_anova$`Pr(>Chisq)`[-1] > .05){
    outcome_msg <- "proceed" 
  } else {
    outcome_msg <- "No significant predictors"
  }
  while (outcome_msg != "Final Model" & outcome_msg != "No significnat predictors" & pred_var[1] != "1") {
    mod_full <- lmer_formula_func(outcome_var, pred_var, rand_var, data)
    mod_full_sum <- summary(mod_full)
    pred_var <- pred_var[pred_var != names(mod_full_sum$coefficients[-1,1][mod_full_sum$coefficients[-1,4] == max(mod_full_sum$coefficients[-1,4])])]
    pred_var <- if(identical(pred_var, character(0))) {
      "1"
      } else{
        pred_var
      }
    mod_reduced <- lmer_formula_func(outcome_var, pred_var, rand_var, data=na.omit(data[ , all.vars(formula(mod_full))]))
    full_reduced_anova <- lrtest(mod_full, mod_reduced)
    if (full_reduced_anova$`Pr(>Chisq)`[-1] > .05){
      outcome_msg <- "proceed" 
    } else {
      outcome_msg <- "Final Model"
    }
  }
  paste0("Results of the mixed-effects linear regression indicated that there was", 
               ifelse(full_reduced_anova$`Pr(>Chisq)`[-1] > .05, " no significant effect ", " a significant effect "), 
               "between predictor variables ", paste(row.names(mod_full_sum$coefficients)[-1], collapse = ", "), 
               " and the outcome variable ", outcome_var, " ??2(", abs(full_reduced_anova$Df[-1]), "), ", 
               ifelse(full_reduced_anova$`Pr(>Chisq)`[-1] < .001, "p < .001", 
                      paste0("p = ", round(full_reduced_anova$`Pr(>Chisq)`[-1], 3)))) -> cap
  
  # Table of coefficients
  # regulartable(mod_full_sum$coefficients %>%
  #   as_tibble(rownames = "Coefficients") %>%
  #   mutate(across(where(is.double), round,3)))%>%
  #   add_header_lines(cap)
  #   theme_zebra() %>% 
  #   autofit() %>%
  #   fit_to_width(9.5)
  mod_full_sum$coefficients %>%
                 as_tibble() %>%
                 rownames_to_column(., var = "Coefficients") %>% 
                 mutate(across(where(is.double), round,3))%>%
    kable(caption = table_counter_func(paste(outcome_var, cap))) %>%
    kable_options()
}
# invisible(lapply(outcome_var, function(outcome_var) lmer_stepwise_func(outcome_var, pred_var, rand_var, lm_data)))

## LMER for all variables- stepwise 

lmer_formula_func <- function(outcome_var, pred_var, rand_var, data){
  model_formula <- formula(paste0(outcome_var, "~ ", paste(pred_var, collapse = " + "), " + ", paste(paste0("(1|", rand_var, ")"), collapse = " + ")))
  lmer(model_formula, data = data, REML = F) -> mod1
}

lmer_factor_stepwise_func <- function(outcome_var, pred_var, rand_var, data){
  final_var <- pred_var
  mod_full <- lmer_formula_func(outcome_var, pred_var, rand_var, data)
  mod_full_sum <- summary(mod_full)
  # Generate models for pred_vars removing one at each iteration
  mod_one_removed<- lapply(1:length(pred_var), function(i) lmer_formula_func(outcome_var, pred_var[-i], rand_var, data))
  aic_vec <- sapply(1:length(pred_var), function(i) summary(mod_one_removed[[i]])$AICtab[1])
  pred_var <- pred_var[-which(aic_vec == min(aic_vec))]
  mod_reduced <- lmer_formula_func(outcome_var, pred_var, rand_var, data = na.omit(data[ , all.vars(formula(mod_full))]))
  full_reduced_anova <- lrtest(mod_full, mod_reduced)
  if (full_reduced_anova$`Pr(>Chisq)`[-1] > .05){
    outcome_msg <- "proceed" 
  } else {
    outcome_msg <- "All significant predictors"
  }
  while (outcome_msg != "Final Model" & outcome_msg != "All significnat predictors" & pred_var[1] != "1") {
    final_var <- pred_var
    mod_full <- lmer_formula_func(outcome_var, pred_var, rand_var, data)
    mod_full_sum <- summary(mod_full)
    mod_one_removed<- lapply(1:length(pred_var), function(i) lmer_formula_func(outcome_var, pred_var[-i], rand_var, data))
    aic_vec <- sapply(1:length(pred_var), function(i) summary(mod_one_removed[[i]])$AICtab[1])
    pred_var <- pred_var[-which(aic_vec == min(aic_vec))]
    pred_var <- if(identical(pred_var, character(0))) {
      "1"
    } else{
      pred_var
    }
    mod_reduced <- lmer_formula_func(outcome_var, pred_var, rand_var, data=na.omit(data[ , all.vars(formula(mod_full))]))
    full_reduced_anova <- lrtest(mod_full, mod_reduced)
    if (!isTRUE(getOption('knitr.in.progress'))) {
      print(full_reduced_anova)
    }
    if (full_reduced_anova$`Pr(>Chisq)`[-1] > .05){
      outcome_msg <- "proceed" 
    } else {
      outcome_msg <- "Final Model"
    }
  }
  paste0("Results of the mixed-effects linear regression indicated that there was", 
         ifelse(full_reduced_anova$`Pr(>Chisq)`[-1] > .05, " no significant effect ", " a significant effect "), 
         "between predictor variables ", paste(final_var, collapse = ", "), 
         " and the outcome variable ", outcome_var, " X2(", abs(full_reduced_anova$Df[-1]), "), ", 
         ifelse(full_reduced_anova$`Pr(>Chisq)`[-1] < .001, "p < .001", 
                paste0("p = ", round(full_reduced_anova$`Pr(>Chisq)`[-1], 3)))) -> cap
  
  # Table of coefficients
  # regulartable(mod_full_sum$coefficients %>%
  #   as_tibble(rownames = "Coefficients") %>%
  #   mutate(across(where(is.double), round,3)))%>%
  #   add_header_lines(cap)
  #   theme_zebra() %>% 
  #   autofit() %>%
  #   fit_to_width(9.5)
  mod_full_sum$coefficients %>%
    as_tibble() %>%
    rownames_to_column(., var = "Coefficients") %>% 
    mutate(across(where(is.double), round,3)) -> output_df
  
  if (isTRUE(getOption('knitr.in.progress'))) {
    output_df %>%
      table_output_func(., colnames = colnames(.), caption = table_counter_func(paste(outcome_var, cap)))
  } else {
    output_df %>%
      kable(caption = table_counter_func(paste(outcome_var, cap))) %>%
      kable_options()
  }
}
# invisible(lapply(outcome_var, function(outcome_var) lmer_factor_stepwise_func(outcome_var, pred_var, rand_var, lm_data)))

lm_factor_stepwise_func <- function(outcome_var, pred_var, data){
	pred_var <- pred_var[sapply(data[pred_var], function(x) n_distinct(x, na.rm = T) > 1)]
    final_var <- pred_var
  mod_full <- lm_formula_func(outcome_var, pred_var, data)
  mod_full_sum <- summary(mod_full)
if (length(final_var) > 1) {
  # Generate models for pred_vars removing one at each iteration
  mod_one_removed<- lapply(1:length(pred_var), function(i) lm_formula_func(outcome_var, pred_var[-i], data))
  #aic_vec <- sapply(1:length(pred_var), function(i) AIC(mod_one_removed[[i]]))
  aic_vec <- sapply(1:length(pred_var), function(i) summary(mod_one_removed[[i]])$adj.r.squared)
  pred_var <- pred_var[-which(aic_vec == max(aic_vec, na.rm = T))]

  mod_reduced <- lm_formula_func(outcome_var, pred_var, data = na.omit(data[ , all.vars(formula(mod_full))]))
  } else {
mod_reduced <- lm_formula_func(outcome_var, "1", data = na.omit(data[ , all.vars(formula(mod_full))]))
}	
full_reduced_anova <- lrtest(mod_full, mod_reduced)
  if (full_reduced_anova$`Pr(>Chisq)`[-1] > .05){
    outcome_msg <- "proceed" 
  } else {
    outcome_msg <- "All significant predictors"
# print(outcome_msg)
  }
  while ((outcome_msg != "Final Model") & (outcome_msg != "All significnat predictors") & 
(pred_var[1] != "1") & (length(final_var) > 1)) {
    final_var <- pred_var
    mod_full <- lm_formula_func(outcome_var, pred_var, data)
    mod_full_sum <- summary(mod_full)
    if (length(final_var) > 1) {
      mod_one_removed<- lapply(1:length(pred_var), function(i) lm_formula_func(outcome_var, pred_var[-i], data))
    # aic_vec <- sapply(1:length(pred_var), function(i) AIC(mod_one_removed[[i]]))
    aic_vec <- sapply(1:length(pred_var), function(i) summary(mod_one_removed[[i]])$adj.r.squared)
    pred_var <- pred_var[-which(aic_vec == max(aic_vec, na.rm = T))]
    pred_var <- if(identical(pred_var, character(0))) {
      "1"
    } else{
      pred_var
    }
    mod_reduced <- lm_formula_func(outcome_var, pred_var, data=na.omit(data[ , all.vars(formula(mod_full))]))
    } else {
      mod_reduced <- lm_formula_func(outcome_var, "1", data = na.omit(data[ , all.vars(formula(mod_full))]))
    }
    full_reduced_anova <- lrtest(mod_full, mod_reduced)
    if (isFALSE(getOption('knitr.in.progress'))) {
      print(full_reduced_anova)
    }
    if (full_reduced_anova$`Pr(>Chisq)`[-1] > .05){
      outcome_msg <- "proceed" 
    } else {
      outcome_msg <- "Final Model"
    }
  }
  paste0("Results of the linear regression indicated that there was", 
         ifelse(full_reduced_anova$`Pr(>Chisq)`[-1] > .05, " no significant effect ", " a significant effect "), 
         "between predictor variables ", paste(final_var, collapse = ", "), 
         " and the outcome variable ", outcome_var, " X2(", abs(full_reduced_anova$Df[-1]), "), ", 
         ifelse(full_reduced_anova$`Pr(>Chisq)`[-1] < .001, "p < .001", 
                paste0("p = ", round(full_reduced_anova$`Pr(>Chisq)`[-1], 3)))) -> cap
  
  # Table of coefficients
  # regulartable(mod_full_sum$coefficients %>%
  #   as_tibble(rownames = "Coefficients") %>%
  #   mutate(across(where(is.double), round,3)))%>%
  #   add_header_lines(cap)
  #   theme_zebra() %>% 
  #   autofit() %>%
  #   fit_to_width(9.5)
  mod_full_sum$coefficients %>%
    as_tibble() %>%
    rownames_to_column(., var = "Coefficients") %>% 
    mutate(across(where(is.double), round,3)) -> output_df
  
  if (isTRUE(getOption('knitr.in.progress'))) {
    output_df %>%
      table_output_func(., colnames = colnames(.), caption = table_counter_func(paste(outcome_var, cap)))
    } else {
      output_df %>%
      kable(caption = table_counter_func(paste(outcome_var, cap))) %>%
      kable_options()
    }
}
# invisible(lapply(outcome_var, function(outcome_var) lmer_stepwise_func(outcome_var, pred_var, rand_var, lm_data)))

p_value_asterisk_significance_note <- "* = _p_ < .05; ** = _p_ < .01; *** = _p_ < .001"