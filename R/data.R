#' MICUSP corpus tagged with pseudobibeR features
#'
#' The Michigan Corpus of Upper-Level Student Papers (MICUSP) contains 828
#' student papers. Here each document is tagged with Biber features using the
#' pseudobibeR package. Type-to-token ratio is calculated using the moving
#' average type-to-token ratio (MATTR).
#'
#' @format A data frame with 828 rows and 68 columns:
#' \describe{
#' \item{doc_id}{Document ID (from MICUSP)}
#' \item{f_01_past_tense}{Rate of past tense per 1,000 tokens}
#' \item{f_02_perfect_aspect}{Rate of perfect aspect per 1,000 tokens}
#' \item{f_03_present_tense}{Rate of present tense per 1,000 tokens}
#' \item{f_04_place_adverbials}{Rate of place adverbials per 1,000 tokens}
#' \item{f_05_time_adverbials}{Rate of time adverbials per 1,000 tokens}
#' \item{f_06_first_person_pronouns}{Rate of first person pronouns per 1,000 tokens}
#' \item{f_07_second_person_pronouns}{Rate of second person pronouns per 1,000 tokens}
#' \item{f_08_third_person_pronouns}{Rate of third person pronouns per 1,000 tokens}
#' \item{f_09_pronoun_it}{Rate of pronoun 'it' per 1,000 tokens}
#' \item{f_10_demonstrative_pronoun}{Rate of demonstrative pronouns per 1,000 tokens}
#' \item{f_11_indefinite_pronouns}{Rate of indefinite pronouns per 1,000 tokens}
#' \item{f_12_proverb_do}{Rate of proverb 'do' per 1,000 tokens}
#' \item{f_13_wh_question}{Rate of wh-questions per 1,000 tokens}
#' \item{f_14_nominalizations}{Rate of nominalizations per 1,000 tokens}
#' \item{f_15_gerunds}{Rate of gerunds per 1,000 tokens}
#' \item{f_16_other_nouns}{Rate of other nouns per 1,000 tokens}
#' \item{f_17_agentless_passives}{Rate of agentless passives per 1,000 tokens}
#' \item{f_18_by_passives}{Rate of by-passives per 1,000 tokens}
#' \item{f_19_be_main_verb}{Rate of 'be' as main verb per 1,000 tokens}
#' \item{f_20_existential_there}{Rate of existential 'there' per 1,000 tokens}
#' \item{f_21_that_verb_comp}{Rate of that-verb complements per 1,000 tokens}
#' \item{f_22_that_adj_comp}{Rate of that-adjective complements per 1,000 tokens}
#' \item{f_23_wh_clause}{Rate of wh-clauses per 1,000 tokens}
#' \item{f_24_infinitives}{Rate of infinitives per 1,000 tokens}
#' \item{f_25_present_participle}{Rate of present participles per 1,000 tokens}
#' \item{f_26_past_participle}{Rate of past participles per 1,000 tokens}
#' \item{f_27_past_participle_whiz}{Rate of past participle whiz-deletions per 1,000 tokens}
#' \item{f_28_present_participle_whiz}{Rate of present participle whiz-deletions per 1,000 tokens}
#' \item{f_29_that_subj}{Rate of that-subject clauses per 1,000 tokens}
#' \item{f_30_that_obj}{Rate of that-object clauses per 1,000 tokens}
#' \item{f_31_wh_subj}{Rate of wh-subject clauses per 1,000 tokens}
#' \item{f_32_wh_obj}{Rate of wh-object clauses per 1,000 tokens}
#' \item{f_33_pied_piping}{Rate of pied-piping per 1,000 tokens}
#' \item{f_34_sentence_relatives}{Rate of sentence relatives per 1,000 tokens}
#' \item{f_35_because}{Rate of 'because' per 1,000 tokens}
#' \item{f_36_though}{Rate of 'though' per 1,000 tokens}
#' \item{f_37_if}{Rate of 'if' per 1,000 tokens}
#' \item{f_38_other_adv_sub}{Rate of other adverbial subordinators per 1,000 tokens}
#' \item{f_39_prepositions}{Rate of prepositions per 1,000 tokens}
#' \item{f_40_adj_attr}{Rate of attributive adjectives per 1,000 tokens}
#' \item{f_41_adj_pred}{Rate of predicative adjectives per 1,000 tokens}
#' \item{f_42_adverbs}{Rate of adverbs per 1,000 tokens}
#' \item{f_43_type_token}{Type-token ratio (MATTR)}
#' \item{f_44_mean_word_length}{Mean word length}
#' \item{f_45_conjuncts}{Rate of conjuncts per 1,000 tokens}
#' \item{f_46_downtoners}{Rate of downtoners per 1,000 tokens}
#' \item{f_47_hedges}{Rate of hedges per 1,000 tokens}
#' \item{f_48_amplifiers}{Rate of amplifiers per 1,000 tokens}
#' \item{f_49_emphatics}{Rate of emphatics per 1,000 tokens}
#' \item{f_50_discourse_particles}{Rate of discourse particles per 1,000 tokens}
#' \item{f_51_demonstratives}{Rate of demonstratives per 1,000 tokens}
#' \item{f_52_modal_possibility}{Rate of possibility modals per 1,000 tokens}
#' \item{f_53_modal_necessity}{Rate of necessity modals per 1,000 tokens}
#' \item{f_54_modal_predictive}{Rate of predictive modals per 1,000 tokens}
#' \item{f_55_verb_public}{Rate of public verbs per 1,000 tokens}
#' \item{f_56_verb_private}{Rate of private verbs per 1,000 tokens}
#' \item{f_57_verb_suasive}{Rate of suasive verbs per 1,000 tokens}
#' \item{f_58_verb_seem}{Rate of 'seem' verbs per 1,000 tokens}
#' \item{f_59_contractions}{Rate of contractions per 1,000 tokens}
#' \item{f_60_that_deletion}{Rate of that-deletions per 1,000 tokens}
#' \item{f_61_stranded_preposition}{Rate of stranded prepositions per 1,000 tokens}
#' \item{f_62_split_infinitve}{Rate of split infinitives per 1,000 tokens}
#' \item{f_63_split_auxiliary}{Rate of split auxiliaries per 1,000 tokens}
#' \item{f_64_phrasal_coordination}{Rate of phrasal coordination per 1,000 tokens}
#' \item{f_65_clausal_coordination}{Rate of clausal coordination per 1,000 tokens}
#' \item{f_66_neg_synthetic}{Rate of synthetic negation per 1,000 tokens}
#' \item{f_67_neg_analytic}{Rate of analytic negation per 1,000 tokens}
#' }
#' @source Michigan Corpus of Upper-Level Student Papers,
#'   <https://elicorpora.info/main>, tagged with the pseudobibeR package.
"micusp_biber"
