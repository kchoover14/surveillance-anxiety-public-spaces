######################## SMART FESTIVALS -- ANNALS 2019 FIGURES
######################## Reproduces published Figures 1-4 (Gender breakdown)
######################## Plus new ranked choice figure (3 versions for review)
######################## Input:  revised-data.xlsx, revised-data-ranked-choice.xlsx
######################## Output: revised-annals-fig1.png  -- Panel A: raw counts, B: proportions
########################         revised-annals-fig2.png  -- Panel A: dodged, B: stacked w/ labels
########################         revised-annals-fig3.png  -- Panel A: dodged, B: stacked w/ labels
########################         revised-annals-fig4.png  -- Panel A: dodged, B: stacked w/ labels
########################         revised-annals-fig-rc-stacked.png  (rank distribution, values in bars)
########################         revised-annals-fig-rc-dot.png      (mean rank dot plot by gender)
########################         revised-annals-fig-rc-heat.png     (rank distribution heatmap)

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

######################## DATA

specsafety = read_excel("revised-data.xlsx", sheet = "specsafety")
measures   = read_excel("revised-data.xlsx", sheet = "measures")
feelings   = read_excel("revised-data.xlsx", sheet = "feelings")
ranked     = read_excel("revised-data-ranked-choice.xlsx", sheet = "ranked_choice")

# Gender three-level factor (Female/Male/Nonbinary)
# Trans and Intersex collapsed into Nonbinary -- matches Annals 2020 Table 1
specsafety$Gender = factor(specsafety$Gender, levels = c("Female", "Male", "Nonbinary"))
measures$Gender   = factor(measures$Gender,   levels = c("Female", "Male", "Nonbinary"))
feelings$Gender   = factor(feelings$Gender,   levels = c("Female", "Male", "Nonbinary"))
ranked$Gender     = factor(ranked$Gender,     levels = c("Female", "Male", "Nonbinary"))

n_total = nrow(specsafety)

######################## HELPERS

prop_by_gender = function(df, var) {
  df |>
    filter(!is.na(Gender)) |>
    group_by(Gender) |>
    summarise(prop = mean(.data[[var]], na.rm = TRUE), .groups = "drop") |>
    mutate(variable = var)
}

count_by_gender = function(df, var) {
  df |>
    filter(!is.na(Gender)) |>
    group_by(Gender) |>
    summarise(n = sum(.data[[var]], na.rm = TRUE),
              prop = mean(.data[[var]], na.rm = TRUE), .groups = "drop") |>
    mutate(variable = var)
}

# Shared theme for all panels
theme_annals = function() {
  theme_classic() +
    theme(axis.text.x  = element_text(angle = 30, hjust = 1),
          legend.position = "top",
          legend.title    = element_blank())
}

######################## FIGURE 1: SAFETY CONCERNS -- OVERALL (panel A = counts, B = proportions)

sc_vars   = c("CrowdViolence", "UnwantedSecurity", "Sexual",
              "Terrorism", "Physical", "Police")
sc_labels = c("Crowd Violence", "Unwanted Security", "Sexual Safety",
              "Terrorist Attack", "Physical Assault", "Police")

sc_overall = specsafety |>
  summarise(across(all_of(sc_vars), list(n = ~ sum(.x, na.rm = TRUE),
                                         prop = ~ mean(.x, na.rm = TRUE)))) |>
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_(?=[^_]+$)") |>
  mutate(label = sc_labels[match(variable, sc_vars)])

fig1 = ggplot(sc_overall, aes(x = reorder(label, prop), y = prop)) +
  geom_col(fill = "#2E6F8E") +
  geom_text(aes(label = percent(prop, accuracy = 1)), hjust = -0.2, size = 3.5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, max(sc_overall$prop) * 1.15)) +
  coord_flip() +
  labs(x = "", y = "Percentage selecting",
       caption = paste0("N = ", n_total,
                        ". Note: revised 6-category coding; minor differences from published figure reflect this revision.")) +
  theme_classic()

ggsave("revised-annals-fig1.png", plot = fig1, width = 6, height = 4,
       units = "in", dpi = 300)

######################## FIGURE 2: SAFETY CONCERNS BY GENDER

sc_bygender = bind_rows(lapply(sc_vars, function(v) count_by_gender(specsafety, v))) |>
  mutate(label = sc_labels[match(variable, sc_vars)])

fig2 = ggplot(sc_bygender, aes(x = label, y = prop, fill = Gender)) +
  geom_col(position = "stack") +
  geom_text(aes(label = percent(prop, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 2.5, color = "white") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "", y = "Percentage selecting",
       caption = paste0("N = ", n_total,
                        ". Note: revised 6-category coding; minor differences from published figure reflect this revision.")) +
  theme_annals()

ggsave("revised-annals-fig2.png", plot = fig2, width = 8, height = 5,
       units = "in", dpi = 300)

######################## FIGURE 3: SAFETY MEASURES BY GENDER

meas_vars   = c("Friends", "HealthTents", "EthosVibe", "CitSecurity",
                "Location", "Police", "Surveillance", "PvtSecurity")
meas_labels = c("Going with Friends", "Health Tents", "Festival Ethos",
                "Citizen Security", "Venue Location", "Police",
                "Surveillance", "Private Security")

meas_bygender = bind_rows(lapply(meas_vars, function(v) count_by_gender(measures, v))) |>
  mutate(label = meas_labels[match(variable, meas_vars)])

fig3 = ggplot(meas_bygender, aes(x = label, y = prop, fill = Gender)) +
  geom_col(position = "stack") +
  geom_text(aes(label = percent(prop, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 2.5, color = "white") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "", y = "Percentage selecting",
       caption = paste0("N = ", n_total,
                        ". Note: revised 6-category coding; minor differences from published figure reflect this revision.")) +
  theme_annals()

ggsave("revised-annals-fig3.png", plot = fig3, width = 8, height = 5,
       units = "in", dpi = 300)

######################## FIGURE 4: FEELINGS ABOUT SURVEILLANCE BY GENDER

feel_vars   = c("ChangesVibe", "Watched", "Safe", "Indifferent",
                "AtRisk", "Anxious", "RuinsFest", "Unsafe")
feel_labels = c("Changes Vibe", "Watched by the Man", "Safer", "Indifferent",
                "At Risk from Authorities", "Anxious", "Ruins Festival", "Unsafe")

feel_bygender = bind_rows(lapply(feel_vars, function(v) count_by_gender(feelings, v))) |>
  mutate(label = feel_labels[match(variable, feel_vars)])

fig4 = ggplot(feel_bygender, aes(x = label, y = prop, fill = Gender)) +
  geom_col(position = "stack") +
  geom_text(aes(label = percent(prop, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 2.5, color = "white") +
  scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "", y = "Percentage selecting",
       caption = paste0("N = ", n_total,
                        ". Note: revised 6-category coding; minor differences from published figure reflect this revision.")) +
  theme_annals()

ggsave("revised-annals-fig4.png", plot = fig4, width = 8, height = 5,
       units = "in", dpi = 300)

######################## RANKED CHOICE DATA PREP

rc_vars   = c("get_away", "rules_free_zone", "substances_sex",
              "live_performances", "non_perf_events", "friends")
rc_labels = c("Get Away", "Rules-Free Zone", "Substances & Sex",
              "Live Performances", "Non-Perf Events", "Friends")

rc_long = ranked |>
  pivot_longer(cols = all_of(rc_vars),
               names_to = "reason", values_to = "rank") |>
  mutate(reason_label = rc_labels[match(reason, rc_vars)],
         rank = as.integer(rank)) |>
  filter(!is.na(rank), !is.na(Gender))

######################## RANKED CHOICE FIG A: STACKED BAR with values inside

rc_stacked = rc_long |>
  count(reason_label, rank) |>
  group_by(reason_label) |>
  mutate(prop = n / sum(n))

rc_order = rc_stacked |>
  filter(rank == 1) |>
  arrange(desc(prop)) |>
  pull(reason_label)

fig_rc_stacked = ggplot(rc_stacked,
                        aes(x = factor(reason_label, levels = rc_order),
                            y = prop, fill = factor(rank))) +
  geom_col(position = "stack") +
  geom_text(aes(label = ifelse(prop >= 0.05, percent(prop, accuracy = 1), "")),
            position = position_stack(vjust = 0.5),
            size = 2.8, color = "white") +
  scale_fill_viridis_d(option = "D", begin = 0.05, end = 0.95,
                       name = "Rank", labels = paste("Rank", 1:6)) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "", y = "Proportion of respondents",
       title = "Why Attend Festivals? Distribution of Rankings") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "right")

ggsave("revised-annals-fig-rc-stacked.png", plot = fig_rc_stacked,
       width = 8, height = 5, units = "in", dpi = 300)

######################## RANKED CHOICE FIG B: DOT PLOT (mean rank by gender)

rc_mean = rc_long |>
  group_by(reason_label, Gender) |>
  summarise(mean_rank = mean(rank, na.rm = TRUE), .groups = "drop")

fig_rc_dot = ggplot(rc_mean,
                    aes(x = mean_rank,
                        y = reorder(reason_label, -mean_rank),
                        color = Gender, shape = Gender)) +
  geom_point(size = 4) +
  scale_color_viridis_d(option = "D", begin = 0.1, end = 0.85) +
  scale_x_continuous(limits = c(1, 6), breaks = 1:6,
                     labels = paste("Rank", 1:6)) +
  labs(x = "Mean Rank (1 = most important)", y = "",
       color = "", shape = "",
       title = "Why Attend Festivals? Mean Rank by Gender") +
  theme_classic() +
  theme(legend.position = "top")

ggsave("revised-annals-fig-rc-dot.png", plot = fig_rc_dot,
       width = 7, height = 4, units = "in", dpi = 300)

######################## RANKED CHOICE FIG C: HEATMAP

rc_heat = rc_long |>
  count(reason_label, rank) |>
  group_by(reason_label) |>
  mutate(prop = n / sum(n))

fig_rc_heat = ggplot(rc_heat,
                     aes(x = factor(rank), y = reason_label, fill = prop)) +
  geom_tile(color = "white") +
  geom_text(aes(label = percent(prop, accuracy = 1)), size = 3) +
  scale_fill_viridis_c(option = "D", labels = percent_format(),
                       name = "% assigned\nthis rank") +
  labs(x = "Rank (1 = most important)", y = "",
       title = "Why Attend Festivals? Rank Distribution Heatmap") +
  theme_classic() +
  theme(legend.position = "right")

ggsave("revised-annals-fig-rc-heat.png", plot = fig_rc_heat,
       width = 8, height = 4, units = "in", dpi = 300)

######################## TIDY
rm(list = ls())
gc()