######################## SURVEILLANCE, TRUST, AND POLICING AT MUSIC FESTIVALS
######################## ETL: raw SurveyMonkey data -> analytical datasets
######################## Input:  data-raw.xlsx (sheet: raw)
######################## Output: revised-data.xlsx (analytical tabs)
########################         revised-ranked-choice.xlsx (ranked choice data)
########################
######################## SurveyMonkey export structure (data-raw.xlsx):
########################   Row 1: question text (spans multiple cols for multi-select)
########################   Row 2: option labels ("Response" for single-answer questions)
########################   Row 3+: participant data (217 rows including 2 under-18)
########################   Multi-select: col contains option label text if selected, NA if not
########################
######################## Two survey waves determined by start date:
########################   Wave 1: start date < 2018-11-07  (n = 189)
########################   Wave 2: start date >= 2018-11-07 (n = 27)
########################   Wave 2 added one question: country of festival attendance (col BI)
########################   All other questions are identical across waves (cols D-BH)
########################
######################## Col layout (1-indexed, after skip=1):
########################   A  (1):  Respondent id
########################   B  (2):  Start Date
########################   C  (3):  End Date
########################   D  (4):  Age (Response)
########################   E-J(5-10): Gender (Male/Female/Non-binary/Prefer not to say/Trans/Intersex)
########################   K-O(11-15): Sexual orientation (Lesbian/Gay/Queer/Bisexual/Heterosexual)
########################   P  (16): Experience (Response)
########################   Q-V(17-22): Ranked choice - why attend festivals
########################   W-Y(23-25): Attended same festival (Yes/No/Which ones)
########################   Z-AI(26-35): Specific safety concerns (multi-select + comments)
########################   AJ-AS(36-45): Safety measures (multi-select + comments)
########################   AT (46): Safety changes (Response)
########################   AU (47): Personal safety concern (Response)
########################   AV (48): Notice surveillance (Response)
########################   AW (49): Notice change in surveillance (Response)
########################   AX (50): Intro surv incline attend (Response)
########################   AY-BF(51-58): Feelings about surveillance (multi-select)
########################   BG (59): Need more/less surveillance (Response)
########################   BH (60): Declined to attend unsafe (Response)
########################   BI (61): Country (Open-Ended Response)

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(janitor)

######################## LOAD RAW DATA

# skip = 1: question-text row skipped; row 2 (option labels) becomes col names
raw = read_excel("data-raw.xlsx", sheet = "raw", col_names = TRUE, skip = 1)

######################## RENAME AMBIGUOUS COLS BEFORE clean_names()
# Positional renames group each ambiguous col with its question context.
# Cols are 1-indexed here; after skip=1, col positions match orig row-2 labels.

# Metadata
colnames(raw)[1]  = "id"
colnames(raw)[2]  = "start_date"
colnames(raw)[3]  = "end_date"

# Single-answer questions named "Response" by SurveyMonkey
colnames(raw)[4]  = "age"
colnames(raw)[16] = "experience"
colnames(raw)[46] = "safety_changes"
colnames(raw)[47] = "per_safety"
colnames(raw)[48] = "notice_surv"
colnames(raw)[49] = "notice_change_surv"
colnames(raw)[50] = "intro_surv_incline_attend"
colnames(raw)[59] = "need_more_less"
colnames(raw)[60] = "decline_unsafe"

# Attended same festival
colnames(raw)[23] = "attend_same_yes"
colnames(raw)[24] = "attend_same_no"
colnames(raw)[25] = "attend_same_which"

# Duplicate "None of the above" and "Other (please specify)"
colnames(raw)[20] = "for_live_performances"
colnames(raw)[39] = "surveillance_cctv"
colnames(raw)[34] = "spec_safety_none"
colnames(raw)[35] = "spec_safety_comments"
colnames(raw)[44] = "measures_none"
colnames(raw)[45] = "measures_comments"

# Country
colnames(raw)[61] = "country"

# Apply janitor clean_names to standardize all remaining col names
raw = janitor::clean_names(raw)
# Rename cols that clean_names() produces unwieldy names for
names(raw)[names(raw) == "unwanted_security_including_i_ds_checkpoints_guards_surveillance"] = "unwanted_security_surv"

message("Rows before exclusions: ", nrow(raw))

######################## EXCLUSIONS
# Two respondents under 18 excluded (no age value)
raw = raw[!is.na(raw$age), ]
message("Rows after age exclusion: ", nrow(raw))
# Expected: 216

######################## WAVE
# Determined by start date only
raw$wave = if_else(as.Date(raw$start_date) < as.Date("2018-11-07"), 1L, 2L)
message("Wave 1 n = ", sum(raw$wave == 1))
message("Wave 2 n = ", sum(raw$wave == 2))

######################## HELPER: BINARY FLAG
# Multi-select: option label present = 1, NA = 0
bin_flag = function(x) as.integer(!is.na(x))

######################## HELPER: ORDINAL LOOKUP
ord_lookup = function(x, levels_vec, codes_vec) {
  lookup = setNames(codes_vec, levels_vec)
  as.numeric(lookup[x])
}

######################## DEMOGRAPHICS

# --- Age ---
raw$Age  = raw$age
raw$Agen = ord_lookup(raw$Age,
                      c("18-24","25-34","35-44","45-54","55-64","65+"),
                      c(1,      2,      3,      4,      5,      6))

raw$Age2  = ifelse(raw$Age %in% c("35-44","45-54","55-64","65+"), "35+", raw$Age)
raw$Age2n = ord_lookup(raw$Age2, c("18-24","25-34","35+"), c(1,2,3))

# --- Gender ---
# Cols E-J after skip: male, female, non_binary, prefer_not_to_say, trans, intersex
raw$Male_ind      = bin_flag(raw$male)
raw$Female_ind    = bin_flag(raw$female)
raw$NonBinary_ind = bin_flag(raw$non_binary)
raw$PrefNotSay    = bin_flag(raw$prefer_not_to_say)
raw$Trans_ind     = bin_flag(raw$trans)
raw$Intersex_ind  = bin_flag(raw$intersex)

# Gender: collapsed label
# Trans and Intersex collapsed into Nonbinary (too small for separate analysis)
# Matches published Annals 2020 paper coding (Table 1: Female/Male/Nonbinary)
raw$Gender = case_when(
  raw$Male_ind      == 1 & raw$Female_ind == 0 &
    raw$NonBinary_ind == 0 & raw$Trans_ind == 0 & raw$Intersex_ind == 0 ~ "Male",
  raw$Female_ind    == 1 & raw$Male_ind   == 0 &
    raw$NonBinary_ind == 0 & raw$Trans_ind == 0 & raw$Intersex_ind == 0 ~ "Female",
  raw$NonBinary_ind == 1 | raw$Trans_ind == 1 | raw$Intersex_ind == 1   ~ "Nonbinary",
  TRUE ~ NA_character_
)
raw$Gendern = ord_lookup(raw$Gender, c("Female","Male","Nonbinary"), c(1,2,3))

# Sex binary (published paper rule):
# female + non-binary  -> Female
# male   + non-binary  -> Male
# male + female + non-binary -> Female (n=1)
# non-binary only -> NA (retained, excluded from sex-based models)
raw$Sex = case_when(
  raw$Female_ind == 1 & raw$Male_ind == 0                              ~ "Female",
  raw$Male_ind   == 1 & raw$Female_ind == 0 & raw$NonBinary_ind == 0   ~ "Male",
  raw$Male_ind   == 1 & raw$NonBinary_ind == 1 & raw$Female_ind == 0   ~ "Male",
  raw$Female_ind == 1 & raw$NonBinary_ind == 1                         ~ "Female",
  TRUE ~ NA_character_
)
raw$Sexn = ord_lookup(raw$Sex, c("Female","Male"), c(1,2))

# --- Sexual Orientation ---
# Cols K-O after skip: lesbian, gay, queer, bisexual, heterosexual
raw$Lesbian_ind      = bin_flag(raw$lesbian)
raw$Gay_ind          = bin_flag(raw$gay)
raw$Queer_ind        = bin_flag(raw$queer)
raw$Bisexual_ind     = bin_flag(raw$bisexual)
raw$Heterosexual_ind = bin_flag(raw$heterosexual)

raw$SexOrient = case_when(
  raw$Heterosexual_ind == 1 &
    (raw$Lesbian_ind + raw$Gay_ind + raw$Queer_ind + raw$Bisexual_ind) == 0 ~ "Heterosexual",
  (raw$Lesbian_ind + raw$Gay_ind + raw$Queer_ind + raw$Bisexual_ind) > 0    ~ "LGBQA",
  TRUE ~ NA_character_
)
raw$SexOrientn = ord_lookup(raw$SexOrient, c("Heterosexual","LGBQA"), c(1,2))

# --- Festival Experience ---
# "0-2" recoded to "1-2" to match analytical tab labels
raw$Experience  = ifelse(raw$experience == "0-2", "1-2", raw$experience)
raw$Experiencen = ord_lookup(raw$Experience, c("1-2","3-5","6+"), c(1,2,3))

# Shared demographic block used across all analytical tabs
demog_block = c("id", "wave",
                "Age","Agen","Age2","Age2n",
                "Gender","Gendern","Sex","Sexn",
                "SexOrient","SexOrientn",
                "Experience","Experiencen")

######################## SPECIFIC SAFETY CONCERNS
# Cols Z-AI (26-35): multi-select + comments
# sexual_harassment, sexual_assault, unwanted_solicitation..., terrorist_attacks,
# crowd_violence, police_police_abuse_of_power,
# unwanted_security_including_ids_checkpoints_guards_surveillance,
# physical_assault, spec_safety_none, spec_safety_comments

raw$SexHar           = bin_flag(raw$sexual_harassment)
raw$SexAssault       = bin_flag(raw$sexual_assault)
raw$Sexual           = as.integer(raw$SexHar == 1 | raw$SexAssault == 1)
raw$UnwantedSubs     = bin_flag(raw$unwanted_solicitation_to_purchase_illegal_substances)
raw$Terrorism        = bin_flag(raw$terrorist_attacks)
raw$CrowdViolence    = bin_flag(raw$crowd_violence)
raw$Police_sc        = bin_flag(raw$police_police_abuse_of_power)
raw$UnwantedSecurity = bin_flag(raw$unwanted_security_surv)
raw$Physical         = bin_flag(raw$physical_assault)

specsafety = raw |>
  dplyr::select(all_of(demog_block),
                SexHar, SexAssault, Sexual,
                UnwantedSubs, Terrorism, CrowdViolence,
                Police = Police_sc, UnwantedSecurity, Physical,
                SpecSafetyComments = spec_safety_comments)

######################## SAFETY MEASURES
# Cols AJ-AS (36-45): multi-select + comments
# location_of_venue, festival_ethos_rules_or_vibe, police_presence,
# surveillance_including_security_checkpoints_ids_and_cctv,
# private_security, citizen_security_volunteer_safe_point,
# going_with_a_pack_of_friends, health_tents,
# measures_none, measures_comments

raw$Location    = bin_flag(raw$location_of_venue)
raw$EthosVibe   = bin_flag(raw$festival_ethos_rules_or_vibe)
raw$Police_m    = bin_flag(raw$police_presence)
raw$Surv_m      = bin_flag(raw$surveillance_cctv)
raw$PvtSecurity = bin_flag(raw$private_security)
raw$CitSecurity = bin_flag(raw$citizen_security_volunteer_safe_point)
raw$Friends     = bin_flag(raw$going_with_a_pack_of_friends)
raw$HealthTents = bin_flag(raw$health_tents)

measures = raw |>
  dplyr::select(all_of(demog_block),
                Location, EthosVibe, Police = Police_m, Surveillance = Surv_m,
                PvtSecurity, CitSecurity, Friends, HealthTents,
                MeasComments = measures_comments)

######################## GENERAL SAFETY CONCERNS
# Col AT (46) = safety_changes; Col AU (47) = per_safety

raw$Changes = case_when(
  raw$safety_changes == "No, my safety concerns have not changed in last 5 years"             ~ "NoChange",
  raw$safety_changes == "Yes, my safety concerns have decreased slightly in the last 5 years" ~ "DecreasedSlightly",
  raw$safety_changes == "Yes, my safety concerns have increased slightly in last 5 years"     ~ "IncreasedSlightly",
  raw$safety_changes == "Yes, my safety concerns have increased greatly in last 5 years"      ~ "IncreasedGreatly",
  TRUE ~ NA_character_
)
# Non-sequential coding preserved from existing analytical tabs
raw$Changesn = ord_lookup(raw$Changes,
                          c("DecreasedSlightly","NoChange","IncreasedSlightly","IncreasedGreatly"), c(1,2,4,3))

raw$PerSafety = case_when(
  raw$per_safety == "No, I’m not that concerned"                            ~ "NotConcerned",
  raw$per_safety == "Yes, but it is a minor thought in the back of my mind" ~ "MinorThought",
  raw$per_safety == "Yes, it is a major concern"                            ~ "MajorConcern",
  TRUE ~ NA_character_   # includes "I’m not sure"
)
raw$PerSafetyn = ord_lookup(raw$PerSafety,
                            c("NotConcerned","MinorThought","MajorConcern"), c(1,2,3))

gensafety = raw |>
  dplyr::select(all_of(demog_block),
                PerSafety, PerSafetyn, Changes, Changesn)

######################## FEELINGS ABOUT SURVEILLANCE
# Cols AY-BF (51-58): multi-select
# safer, watched_by_the_man, indifferent, anxious,
# unsafe, more_at_risk_of_unwarranted_assault_by_authorities,
# ruins_the_festival, changes_the_vibe

raw$Safe        = bin_flag(raw$safer)
raw$Watched     = bin_flag(raw$watched_by_the_man)
raw$Indifferent = bin_flag(raw$indifferent)
raw$Anxious     = bin_flag(raw$anxious)
raw$Unsafe_f    = bin_flag(raw$unsafe)
raw$AtRisk      = bin_flag(raw$more_at_risk_of_unwarranted_assault_by_authorities)
raw$RuinsFest   = bin_flag(raw$ruins_the_festival)
raw$ChangesVibe = bin_flag(raw$changes_the_vibe)

feelings = raw |>
  dplyr::select(all_of(demog_block),
                Safe, Watched, Indifferent, Anxious,
                Unsafe = Unsafe_f, AtRisk, RuinsFest, ChangesVibe)

######################## SURVEILLANCE BEHAVIOUR
# Col AV (48) = notice_surv
# Col AW (49) = notice_change_surv
# Col AX (50) = intro_surv_incline_attend
# Col BG (59) = need_more_less
# Col BH (60) = decline_unsafe

raw$NoticeSurv = case_when(
  raw$notice_surv == "No"         ~ "No",
  raw$notice_surv == "Yes, a bit" ~ "A_Bit",
  raw$notice_surv == "Yes, a lot" ~ "A_Lot",
  TRUE ~ NA_character_
)
raw$NoticeSurvn = ord_lookup(raw$NoticeSurv, c("No","A_Bit","A_Lot"), c(1,2,3))

raw$NoticeChangeSurv = case_when(
  raw$notice_change_surv == "No noticeable change" ~ "NoChange",
  raw$notice_change_surv == "Yes, an increase"     ~ "Increase",
  TRUE ~ NA_character_
)
raw$NoticeChangeSurvn  = ord_lookup(raw$NoticeChangeSurv, c("NoChange","Increase"), c(0,1))
raw$NoticeChangeSurvn2 = ord_lookup(raw$NoticeChangeSurv, c("NoChange","Increase"), c(2,1))

raw$IntroSurvInclineAttend = case_when(
  raw$intro_surv_incline_attend == "Less inclined"  ~ "Less",
  raw$intro_surv_incline_attend == "About the same" ~ "Same",
  raw$intro_surv_incline_attend == "More inclined"  ~ "More",
  TRUE ~ NA_character_
)
raw$IntroSurvInclineAttendn = ord_lookup(raw$IntroSurvInclineAttend,
                                         c("Less","Same","More"), c(1,2,3))

raw$NeedMoreLess = case_when(
  raw$need_more_less == "Less"           ~ "Less",
  raw$need_more_less == "About the same" ~ "NoChange",
  raw$need_more_less == "More"           ~ "More",
  TRUE ~ NA_character_
)
raw$NeedMoreLessn = ord_lookup(raw$NeedMoreLess, c("Less","NoChange","More"), c(1,2,3))

raw$DeclineUnsafe = case_when(
  raw$decline_unsafe == "No"                        ~ "No",
  raw$decline_unsafe == "Yes, but only once"        ~ "Once",
  raw$decline_unsafe == "Yes, on several occasions" ~ "Several",
  TRUE ~ NA_character_   # includes "I’m not sure"
)
raw$DeclineUnsafen = ord_lookup(raw$DeclineUnsafe, c("No","Once","Several"), c(1,2,3))

surveillance_tab = raw |>
  dplyr::select(all_of(demog_block),
                NoticeSurv, NoticeSurvn,
                NoticeChangeSurv, NoticeChangeSurvn, NoticeChangeSurvn2,
                NeedMoreLess, NeedMoreLessn,
                DeclineUnsafe, DeclineUnsafen,
                IntroSurvInclineAttend, IntroSurvInclineAttendn)

######################## FESTIVAL ATTENDANCE INFO TAB

festival_attend = raw |>
  dplyr::select(all_of(demog_block),
                country,
                attend_same_yes,
                attend_same_which)

######################## DEMOG TAB

demog = raw |>
  dplyr::select(id, wave,
                Age2, Gender, Sex, SexOrient, Experience,
                Age2n, Gendern, Sexn, SexOrientn, Experiencen)

######################## LONG FORMAT TAB

safety_cats   = c("Physical","CrowdViolence","Sexual","Terrorism","Police","UnwantedSecurity")
safer_cats    = c("Location","EthosVibe","Police","Surveillance","PvtSecurity","CitSecurity","Friends","HealthTents")
feelings_cats = c("Safe","Watched","Indifferent","Anxious","AtRisk","ChangesVibe")

safety_long = specsafety |>
  pivot_longer(cols = all_of(safety_cats),
               names_to = "SafetyCat", values_to = "SafetyData")

safer_long = measures |>
  pivot_longer(cols = all_of(safer_cats),
               names_to = "SaferCat", values_to = "SaferData")

feelings_long = feelings |>
  pivot_longer(cols = all_of(feelings_cats),
               names_to = "FeelingsCat", values_to = "FeelingsData")

long_tab = safety_long |>
  dplyr::left_join(
    safer_long  |> dplyr::select(id, SaferCat, SaferData),
    by = "id", relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    feelings_long |> dplyr::select(id, FeelingsCat, FeelingsData),
    by = "id", relationship = "many-to-many"
  )

######################## COMMENTS TAB

spec_comments = raw |>
  dplyr::filter(!is.na(spec_safety_comments)) |>
  dplyr::select(all_of(demog_block), Comments = spec_safety_comments) |>
  dplyr::mutate(Category = "Specific Safety")

meas_comments = raw |>
  dplyr::filter(!is.na(measures_comments)) |>
  dplyr::select(all_of(demog_block), Comments = measures_comments) |>
  dplyr::mutate(Category = "Safety Measures")

comments_tab = dplyr::bind_rows(spec_comments, meas_comments) |>
  dplyr::select(all_of(demog_block), Category, Comments)

######################## RANKED CHOICE TAB
# Cols Q-V (17-22): why do you attend festivals
# Numeric ranks 1-6 retained as-is for use in 2019 Annals figures
# Col names after clean_names():
# get_away_from_everyday_life, be_in_a_rules_free_zone,
# to_indulge_in_substances_and_sex_inc_legal_and_illegal_substances,
# for_the_live_performances_whether_music_comedy_improv_theatre_performance_art_etc,
# for_the_non_performance_events, hang_out_with_friends

ranked_choice = raw |>
  dplyr::select(all_of(demog_block),
                get_away          = get_away_from_everyday_life,
                rules_free_zone   = be_in_a_rules_free_zone,
                substances_sex    = to_indulge_in_substances_and_sex_inc_legal_and_illegal_substances,
                live_performances = for_live_performances,
                non_perf_events   = for_the_non_performance_events,
                friends           = hang_out_with_friends)

######################## WRITE OUTPUT

write_xlsx(
  list(
    `festival attend info` = as.data.frame(festival_attend),
    demog                  = as.data.frame(demog),
    gensafety              = as.data.frame(gensafety),
    specsafety             = as.data.frame(specsafety),
    measures               = as.data.frame(measures),
    feelings               = as.data.frame(feelings),
    surveillance           = as.data.frame(surveillance_tab),
    long                   = as.data.frame(long_tab),
    comments               = as.data.frame(comments_tab)
  ),
  "revised-data.xlsx"
)

write_xlsx(
  list(ranked_choice = as.data.frame(ranked_choice)),
  "revised-data-ranked-choice.xlsx"
)

message("ETL complete.")
message("  revised-data.xlsx: ", nrow(demog), " participants, 9 tabs")
message("  revised-ranked-choice.xlsx: ", nrow(ranked_choice), " participants")