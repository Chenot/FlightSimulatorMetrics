# Scripts/questionnaire_analysis.R
library(rstudioapi)

# Load data and libraries
setwd(dirname(getActiveDocumentContext()$path))
source("data_loading.R")

# Questionnaire analysis

#######################################################
############### 1) Prepare data for analyses ##########
#######################################################

# Load the questionnaire data
df_questionnaire <- data_frames[["app_questionnaire.csv"]]

# Translate and rename columns for clarity
df_questionnaire_translated <- df_questionnaire %>%
  rename(
    Demographics_Timestamp = Horodateur,
    Demographics_Participant_Number = Numéro.de.participant,
    Demographics_Flight_Hours = Nombre.d.heures.de.vol,
    Demographics_Aircraft_Type = Type.d.aéronef.principalement.piloté,
    Usability_Layout_Is_Intuitive = La.disposition.de.l.application.est.intuitive.,
    Usability_App_Is_Responsive = L.application.est.réactive.,
    Usability_Visual_Design_Is_Appealing = Le.design.visuel.de.l.application.est.attrayant.,
    Relevance_Information_Is_Relevant = Les.informations.fournies.par.l.application.sont.pertinentes.,
    Relevance_App_Presents_Scores_Clearly = L.application.présente.clairement.les.scores.et.les.métriques.de.performance.,
    Relevance_Understand_How_Scores_Are_Calculated = Je.comprends.comment.les.scores.sont.calculés.et.ce.qu.ils.représentent.,
    Relevance_Feedback_Is_Detailed = Les.retours.fournis.sont.détaillés.et.spécifiques.,
    Utility_Exp_Feedback_Was_Useful = L.application.a.fourni.des.retours.utiles.sur.ma.performance.lors.de.l.expérience.,
    Utility_Exp_Feedback_Is_Relevant = Les.retours.fournis.par.l.application.sont.pertinents.pour.comprendre.ma.performance.lors.de.l.expérience.,
    Utility_Exp_Helped_Identify_Improvements = L.application.m.a.aidé.à.identifier.les.domaines.où.je.pourrais.améliorer.ma.performance.,
    Utility_PPL_App_Would_Be_Useful = Une.application.similaire.serait.utile.pour.la.formation.PPL.,
    Utility_PPL_Help_Identify_Improvements = Une.application.similaire.aiderait.à.identifier.les.domaines.où.les.pilotes.en.formation..PPL.ou.autre..peuvent.s.améliorer.,
    Utility_PPL_Provide_Useful_Feedback = Une.application.similaire.fournirait.des.retours.utiles.pour.les.pilotes.en.formation..PPL.ou.autre.,
    Utility_PPL_Improve_Training_Quality = Une.application.similaire.permettrait.d.améliorer.la.qualité.de.formation.des.pilotes..PPL.ou.autre..
  )

# Convert Likert scale responses to numeric values
likert_columns <- grep("Usability|Relevance|Utility", names(df_questionnaire_translated), value = TRUE)
df_questionnaire_translated[likert_columns] <- lapply(df_questionnaire_translated[likert_columns], as.numeric)

# Filter only the items included in your article
selected_items <- c(
  "Usability_Layout_Is_Intuitive",
  "Usability_App_Is_Responsive",
  "Usability_Visual_Design_Is_Appealing",
  "Relevance_Information_Is_Relevant",
  "Relevance_App_Presents_Scores_Clearly",
  "Relevance_Understand_How_Scores_Are_Calculated",
  "Relevance_Feedback_Is_Detailed",
  "Utility_Exp_Feedback_Was_Useful",
  "Utility_Exp_Helped_Identify_Improvements",
  "Utility_Exp_Feedback_Is_Relevant",
  "Utility_PPL_App_Would_Be_Useful",
  "Utility_PPL_Help_Identify_Improvements",
  "Utility_PPL_Provide_Useful_Feedback",
  "Utility_PPL_Improve_Training_Quality"
)

df_questionnaire_selected <- df_questionnaire_translated %>%
  select(Demographics_Participant_Number, all_of(selected_items))

############################################################
### 2) Questionnaire results analyses ######################
############################################################

### Descriptive results

# Calculate mean scores for each dimension per participant
df_means <- df_questionnaire_selected %>%
  rowwise() %>%
  mutate(
    Usability_GUI = mean(c_across(starts_with("Usability")), na.rm = TRUE),
    Relevance = mean(c_across(starts_with("Relevance")), na.rm = TRUE),
    Utility_Experimental_Feedback = mean(c_across(starts_with("Utility_Exp")), na.rm = TRUE),
    Utility_Pilot_Training_Feedback = mean(c_across(starts_with("Utility_PPL")), na.rm = TRUE)
  ) %>%
  ungroup()


# Select relevant columns
df_means <- df_means %>%
  select(Demographics_Participant_Number, Usability_GUI, Relevance, Utility_Experimental_Feedback, Utility_Pilot_Training_Feedback)

# Rename the participant number column for simplicity
colnames(df_means)[1] <- "Participant_Number"

# Compute descriptive statistics for each dimension
descriptive_stats <- df_means %>%
  summarise(
    across(Usability_GUI:Utility_Pilot_Training_Feedback, list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Dimension", "Statistic"),
    names_pattern = "(.+)_(mean|sd)"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value
  ) %>%
  mutate(
    mean = round(mean, 2),
    sd = round(sd, 2)
  )

# Print descriptive statistics
print(descriptive_stats)

### Descriptive statistics for each item

# Simplify question labels to match your article
question_labels <- c(
  # Usability (GUI)
  "Usability_Layout_Is_Intuitive" = "Intuitive",
  "Usability_App_Is_Responsive" = "Responsive",
  "Usability_Visual_Design_Is_Appealing" = "Appealing",
  # Relevance
  "Relevance_Information_Is_Relevant" = "Relevance",
  "Relevance_App_Presents_Scores_Clearly" = "Score presentation",
  "Relevance_Understand_How_Scores_Are_Calculated" = "Score understanding",
  "Relevance_Feedback_Is_Detailed" = "Detailed feedback",
  # Utility (Experimental Feedback)
  "Utility_Exp_Feedback_Was_Useful" = "Feedback utility (experiment)",
  "Utility_Exp_Helped_Identify_Improvements" = "Identify areas to improve (experiment)",
  "Utility_Exp_Feedback_Is_Relevant" = "Feedback relevance (experiment)",
  # Utility (Pilot Training Feedback)
  "Utility_PPL_App_Would_Be_Useful" = "Utility of a similar app",
  "Utility_PPL_Help_Identify_Improvements" = "Identify areas to improve",
  "Utility_PPL_Provide_Useful_Feedback" = "Feedback utility",
  "Utility_PPL_Improve_Training_Quality" = "Improve training quality"
)

# Prepare data for item-level analysis
df_items <- df_questionnaire_selected %>%
  pivot_longer(cols = -Demographics_Participant_Number, names_to = "Question", values_to = "Score")

# Add the "Dimension" column based on question prefixes
df_items <- df_items %>%
  mutate(
    Dimension = case_when(
      str_detect(Question, "Usability") ~ "Usability (GUI)",
      str_detect(Question, "Relevance") ~ "Relevance",
      str_detect(Question, "Utility_Exp") ~ "Utility (Experimental Feedback)",
      str_detect(Question, "Utility_PPL") ~ "Utility (Pilot Training Feedback)"
    )
  )

# Apply the mapping to rename the questions
df_items <- df_items %>%
  mutate(Question_Label = recode(Question, !!!question_labels))

# Define the desired order of dimensions and items
dimensions_order <- c("Usability (GUI)", "Relevance", "Utility (Experimental Feedback)", "Utility (Pilot Training Feedback)")

# Define the desired order of items within each dimension
item_order <- data.frame(
  Dimension = c(
    rep("Usability (GUI)", 3),
    rep("Relevance", 4),
    rep("Utility (Experimental Feedback)", 3),
    rep("Utility (Pilot Training Feedback)", 4)
  ),
  Question_Label = c(
    # Usability (GUI)
    "Intuitive",
    "Responsive",
    "Appealing",
    # Relevance
    "Relevance",
    "Score presentation",
    "Score understanding",
    "Detailed feedback",
    # Utility (Experimental Feedback)
    "Feedback utility (experiment)",
    "Identify areas to improve (experiment)",
    "Feedback relevance (experiment)",
    # Utility (Pilot Training Feedback)
    "Utility of a similar app",
    "Identify areas to improve",
    "Feedback utility",
    "Improve training quality"
  ),
  Item_Order = c(
    # Usability (GUI)
    1, 2, 3,
    # Relevance
    1, 2, 3, 4,
    # Utility (Experimental Feedback)
    1, 2, 3,
    # Utility (Pilot Training Feedback)
    1, 2, 3, 4
  )
)

# Calculate descriptive statistics per item
descriptive_stats_items <- df_items %>%
  group_by(Dimension, Question_Label) %>%
  summarise(
    Mean = round(mean(Score, na.rm = TRUE), 2),
    SD = round(sd(Score, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  left_join(item_order, by = c("Dimension", "Question_Label")) %>%
  arrange(factor(Dimension, levels = dimensions_order), Item_Order)

# Add item numbers within each dimension
descriptive_stats_items <- descriptive_stats_items %>%
  mutate(N = Item_Order) %>%
  select(-Item_Order)

# Identify the lowest and highest scoring items
lowest_score_item <- descriptive_stats_items %>% arrange(Mean) %>% slice(1)
highest_score_item <- descriptive_stats_items %>% arrange(desc(Mean)) %>% slice(1)

# Print descriptive statistics per item
print(descriptive_stats_items)

### Generate table

# Format the dimension labels to match your article
descriptive_stats_items <- descriptive_stats_items %>%
  mutate(
    Dimension_Display = case_when(
      Dimension == "Usability (GUI)" ~ "Usability \\\\textit{(GUI)}",
      Dimension == "Relevance" ~ "Relevance",
      Dimension == "Utility (Experimental Feedback)" ~ "Utility \\\\textit{(Experimental Feedback)}",
      Dimension == "Utility (Pilot Training Feedback)" ~ "Utility \\\\textit{(Pilot Training Feedback)}"
    )
  )

# Prepare the table data
table_data <- descriptive_stats_items %>%
  select(Dimension_Display, N, Question_Label, Mean, SD)
print(table_data)

# Create the LaTeX table
questionnaire_table <- kable(table_data, format = "latex", booktabs = TRUE, align = "llccc", col.names = c("Dimension", "N", "Item", "Mean", "SD")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  add_header_above(c(" " = 1, " " = 1, " " = 1, "N = 21" = 2)) %>% # Add sample size in header if needed
  row_spec(row = which(!duplicated(table_data$Dimension_Display)), extra_latex_after = "\\hline") %>%
  collapse_rows(columns = 1, latex_hline = "none", valign = "top")

# Print the LaTeX output for the table
cat(questionnaire_table)

### Generate text summary

# Create the report paragraph
report_paragraph <- paste0(
  "The descriptive statistics for each dimension are as follows: ",
  paste(
    descriptive_stats$Dimension,
    " (Mean = ",
    descriptive_stats$mean,
    ", SD = ",
    descriptive_stats$sd,
    ")",
    collapse = "; "
  ),
  ". The lowest score was obtained for the item '",
  lowest_score_item$Question_Label,
  "' (Mean = ",
  lowest_score_item$Mean,
  ", SD = ",
  lowest_score_item$SD,
  ") while the highest score was obtained for the item '",
  highest_score_item$Question_Label,
  "' (Mean = ",
  highest_score_item$Mean,
  ", SD = ",
  highest_score_item$SD,
  "). The Table \\ref{tab:Questionnaire_table_results} shows the results for each item."
)

# Print the report paragraph
print(report_paragraph)