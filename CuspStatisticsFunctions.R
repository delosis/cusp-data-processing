# CUSP Statistics Collection and Reporting Functions

library(data.table)

#' Parse dataTag to extract cohort and timepoint
#' @param dataTag Character vector of data tags
#' @return data.table with columns: dataTag, Cohort, Timepoint, CohortParsed, TimepointParsed
#' @export
parse_dataTag <- function(dataTag) {
  result <- data.table(
    dataTag = dataTag,
    Cohort = NA_character_,
    Timepoint = NA_character_,
    CohortParsed = FALSE,
    TimepointParsed = FALSE
  )
  
  # Extract cohort (c1-c4, case insensitive) - handle various formats like "c3-t1", "T1-C2", etc.
  cohort_pattern <- "c[1-4]"
  timepoint_pattern <- "t[1-3]"
  
  for (i in seq_along(dataTag)) {
    if (!is.na(dataTag[i]) && dataTag[i] != "") {
      tag_lower <- tolower(dataTag[i])
      
      # Try to extract cohort
      cohort_match <- regmatches(tag_lower, regexpr(cohort_pattern, tag_lower))
      if (length(cohort_match) > 0 && cohort_match != "") {
        result$Cohort[i] <- cohort_match
        result$CohortParsed[i] <- TRUE
      }
      
      # Try to extract timepoint
      timepoint_match <- regmatches(tag_lower, regexpr(timepoint_pattern, tag_lower))
      if (length(timepoint_match) > 0 && timepoint_match != "") {
        result$Timepoint[i] <- timepoint_match
        result$TimepointParsed[i] <- TRUE
      }
    }
  }
  
  return(result)
}

#' Initialize statistics collection structure
#' @return List structure for collecting statistics
#' @export
init_statistics <- function() {
  stats <- list(
    # Registration statistics
    registration = list(
      total = 0L,
      opfs_total = 0L,
      opfs_consent_yes = 0L,
      by_regsample = data.table(RegSample = character(), Count = integer()),
      consent_yes = 0L,
      consent_no = 0L,
      consent_missing = 0L,
      consent_by_regsample = data.table()
    ),
    
    # Questionnaire linkage (simplified - just linked or not)
    questionnaire_linkage = list(
      linked = 0L,
      not_linked = 0L,
      by_regsample_cohort_timepoint = data.table()
    ),
    
    # Exclusions tracking
    exclusions = list(
      discardIDs = 0L,
      opfs_removed = 0L,
      test_accounts = 0L,
      manualQC_exclude_usercode = 0L,
      manualQC_exclude_rid = 0L,
      total_excluded = 0L
    ),
    
    # Duplicates (from duplicate_map)
    duplicates = list(
      total_merged = 0L,
      by_regsample_cohort_timepoint = data.table()
    ),
    
    # Matching statistics
    matching = list(
      nameless_matches = 0L,
      by_cohort_timepoint = data.table(),
      cohort_progression = data.table()
    ),
    
    # DataTag parsing issues
    datatag_parsing = list(
      total_records = 0L,
      cohort_parsed = 0L,
      cohort_missing = 0L,
      timepoint_parsed = 0L,
      timepoint_missing = 0L,
      both_parsed = 0L,
      neither_parsed = 0L
    ),
    
    # Final counts by checkpoint (ordered by RegSample then Checkpoint)
    final_counts = list(
      by_regsample_cohort_timepoint = data.table()
    )
  )
  
  return(stats)
}

#' Calculate statistics at a checkpoint with cohort/timepoint breakdown
#' @param df data.table to analyze
#' @param checkpoint_name Name of the checkpoint
#' @param stats Existing statistics structure
#' @return Updated statistics structure
#' @export
calculate_checkpoint_stats <- function(df, checkpoint_name, stats) {
  if (is.null(df) || nrow(df) == 0) {
    return(stats)
  }
  
  setDT(df)
  
  # Filter out OPfS samples for reporting
  # Q1/Q2 have "Sample(s)" field, matching data has "RegSample" or "Sample"
  if ("RegSample" %in% names(df)) {
    df_cusp <- df[grepl('CUSP', RegSample, ignore.case = TRUE), ]
  } else if ("Sample(s)" %in% names(df)) {
    df_cusp <- df[grepl('CUSP', `Sample(s)`, ignore.case = TRUE), ]
  } else if ("Sample" %in% names(df)) {
    df_cusp <- df[grepl('CUSP', Sample, ignore.case = TRUE), ]
  } else {
    # No way to filter, use all data
    df_cusp <- df
  }
  
  # Parse dataTag if it exists
  if ("dataTag" %in% names(df_cusp)) {
    parsed <- parse_dataTag(df_cusp$dataTag)
    df_cusp <- cbind(df_cusp, parsed[, .(Cohort, Timepoint, CohortParsed, TimepointParsed)])
    
    # Update dataTag parsing stats
    stats$datatag_parsing$total_records <- nrow(df_cusp)
    stats$datatag_parsing$cohort_parsed <- sum(df_cusp$CohortParsed, na.rm = TRUE)
    stats$datatag_parsing$cohort_missing <- sum(!df_cusp$CohortParsed, na.rm = TRUE)
    stats$datatag_parsing$timepoint_parsed <- sum(df_cusp$TimepointParsed, na.rm = TRUE)
    stats$datatag_parsing$timepoint_missing <- sum(!df_cusp$TimepointParsed, na.rm = TRUE)
    stats$datatag_parsing$both_parsed <- sum(df_cusp$CohortParsed & df_cusp$TimepointParsed, na.rm = TRUE)
    stats$datatag_parsing$neither_parsed <- sum(!df_cusp$CohortParsed & !df_cusp$TimepointParsed, na.rm = TRUE)
    
    # Count by RegSample/Sample, Cohort, and Timepoint
    sample_col <- if ("RegSample" %in% names(df_cusp)) "RegSample" else if ("Sample(s)" %in% names(df_cusp)) "Sample(s)" else if ("Sample" %in% names(df_cusp)) "Sample" else NULL
    
    if (!is.null(sample_col)) {
      checkpoint_counts <- df_cusp[, .(Count = .N), by = c(sample_col, "Cohort", "Timepoint")]
      setnames(checkpoint_counts, sample_col, "RegSample")
      checkpoint_counts[, Checkpoint := checkpoint_name]
      stats$final_counts$by_regsample_cohort_timepoint <- rbind(
        stats$final_counts$by_regsample_cohort_timepoint,
        checkpoint_counts,
        fill = TRUE
      )
    }
  } else {
    # If no dataTag, still count by RegSample/Sample
    sample_col <- if ("RegSample" %in% names(df_cusp)) "RegSample" else if ("Sample(s)" %in% names(df_cusp)) "Sample(s)" else if ("Sample" %in% names(df_cusp)) "Sample" else NULL
    
    if (!is.null(sample_col)) {
      checkpoint_counts <- df_cusp[, .(Count = .N, Cohort = NA_character_, Timepoint = NA_character_), by = sample_col]
      setnames(checkpoint_counts, sample_col, "RegSample")
      checkpoint_counts[, Checkpoint := checkpoint_name]
      stats$final_counts$by_regsample_cohort_timepoint <- rbind(
        stats$final_counts$by_regsample_cohort_timepoint,
        checkpoint_counts,
        fill = TRUE
      )
    }
  }
  
  return(stats)
}

#' Calculate registration statistics
#' @param registrationDF Registration data.table
#' @param stats Statistics structure
#' @return Updated statistics structure
#' @export
calculate_registration_stats <- function(registrationDF, stats) {
  if (is.null(registrationDF) || nrow(registrationDF) == 0) {
    return(stats)
  }
  
  setDT(registrationDF)
  
  # Separate OPfS and CUSP
  opfs_data <- registrationDF[grepl('OPfS', RegSample, ignore.case = TRUE), ]
  cusp_data <- registrationDF[grepl('CUSP', RegSample, ignore.case = TRUE), ]
  
  stats$registration$total <- nrow(cusp_data)
  stats$registration$opfs_total <- nrow(opfs_data)
  
  # Count by RegSample (CUSP only)
  if (nrow(cusp_data) > 0 && "RegSample" %in% names(cusp_data)) {
    stats$registration$by_regsample <- cusp_data[, .(Count = .N), by = RegSample]
  }
  
  # Count consent status (CUSP only)
  if (nrow(cusp_data) > 0 && "consent" %in% names(cusp_data)) {
    stats$registration$consent_yes <- sum(toupper(cusp_data$consent) == "YES", na.rm = TRUE)
    stats$registration$consent_no <- sum(toupper(cusp_data$consent) == "NO", na.rm = TRUE)
    stats$registration$consent_missing <- sum(is.na(cusp_data$consent) | cusp_data$consent == "", na.rm = TRUE)
    
    # Consent by RegSample
    stats$registration$consent_by_regsample <- cusp_data[, .(
      Total = .N,
      Consent_Yes = sum(toupper(consent) == "YES", na.rm = TRUE),
      Consent_No = sum(toupper(consent) == "NO", na.rm = TRUE),
      Consent_Missing = sum(is.na(consent) | consent == "", na.rm = TRUE)
    ), by = RegSample]
  }
  
  # OPfS consent count
  if (nrow(opfs_data) > 0 && "consent" %in% names(opfs_data)) {
    stats$registration$opfs_consent_yes <- sum(toupper(opfs_data$consent) == "YES", na.rm = TRUE)
  }
  
  return(stats)
}

#' Calculate questionnaire linkage statistics (simplified - just linked or not)
#' @param matching Matching data.table (after merge with Q1/Q2)
#' @param Q1 Q1 data.table
#' @param Q2 Q2 data.table
#' @param stats Statistics structure
#' @return Updated statistics structure
#' @export
calculate_questionnaire_linkage_stats <- function(matching, Q1, Q2, stats) {
  if (is.null(matching) || nrow(matching) == 0) {
    return(stats)
  }
  
  setDT(matching)
  setDT(Q1)
  setDT(Q2)
  
  # Filter to CUSP only
  matching_cusp <- matching[grepl('CUSP', RegSample, ignore.case = TRUE), ]
  
  # Get unique User.codes from Q1 and Q2
  q1_users <- unique(Q1$User.code)
  q2_users <- unique(Q2$User.code)
  q_users <- unique(c(q1_users, q2_users))
  
  # Parse dataTag for cohort/timepoint breakdown
  if ("dataTag" %in% names(matching_cusp)) {
    parsed <- parse_dataTag(matching_cusp$dataTag)
    matching_cusp <- cbind(matching_cusp, parsed[, .(Cohort, Timepoint)])
    
    # Calculate linkage by RegSample, Cohort, Timepoint
    linkage_by_group <- matching_cusp[, {
      users <- unique(User.code[!is.na(User.code)])
      linked_count <- sum(users %in% q_users)
      not_linked_count <- length(users) - linked_count
      
      list(
        Linked = linked_count,
        Not_Linked = not_linked_count,
        Total = length(users)
      )
    }, by = .(RegSample, Cohort, Timepoint)]
    
    stats$questionnaire_linkage$by_regsample_cohort_timepoint <- linkage_by_group
    stats$questionnaire_linkage$linked <- sum(linkage_by_group$Linked)
    stats$questionnaire_linkage$not_linked <- sum(linkage_by_group$Not_Linked)
  }
  
  return(stats)
}

#' Calculate duplicate statistics from duplicate_map
#' @param duplicate_map Duplicate mapping data.table
#' @param matching Final matching data.table (for cohort/timepoint info)
#' @param stats Statistics structure
#' @return Updated statistics structure
#' @export
calculate_duplicate_stats <- function(duplicate_map, matching, stats) {
  if (is.null(duplicate_map) || nrow(duplicate_map) == 0) {
    return(stats)
  }
  
  setDT(duplicate_map)
  setDT(matching)
  
  stats$duplicates$total_merged <- nrow(duplicate_map)
  
  # Get cohort/timepoint info for kept_ids
  if ("dataTag" %in% names(matching)) {
    parsed <- parse_dataTag(matching$dataTag)
    matching <- cbind(matching, parsed[, .(Cohort, Timepoint)])
    
    # Merge duplicate_map with matching to get cohort/timepoint for kept_ids
    duplicate_with_info <- merge(
      duplicate_map,
      matching[, .(User.code, RegSample, Cohort, Timepoint)],
      by.x = "kept_id",
      by.y = "User.code",
      all.x = TRUE
    )
    
    # Count by RegSample, Cohort, Timepoint
    stats$duplicates$by_regsample_cohort_timepoint <- duplicate_with_info[, .(
      Count = .N
    ), by = .(RegSample, Cohort, Timepoint)]
  }
  
  return(stats)
}

#' Calculate nameless match statistics
#' @param nameless_matches Nameless matches data.table
#' @param matching Final matching data.table (for cohort/timepoint info)
#' @param stats Statistics structure
#' @return Updated statistics structure
#' @export
calculate_nameless_stats <- function(nameless_matches, matching, stats) {
  if (is.null(nameless_matches) || nrow(nameless_matches) == 0) {
    return(stats)
  }
  
  setDT(nameless_matches)
  setDT(matching)
  
  stats$matching$nameless_matches <- nrow(nameless_matches)
  
  return(stats)
}

#' Calculate matching statistics
#' @param matching Final matching data.table
#' @param stats Statistics structure
#' @return Updated statistics structure
#' @export
calculate_matching_stats <- function(matching, stats) {
  if (is.null(matching) || nrow(matching) == 0) {
    return(stats)
  }
  
  setDT(matching)
  
  # Filter to CUSP only
  matching_cusp <- matching[grepl('CUSP', RegSample, ignore.case = TRUE), ]
  
  # Parse dataTag
  if ("dataTag" %in% names(matching_cusp)) {
    parsed <- parse_dataTag(matching_cusp$dataTag)
    matching_cusp <- cbind(matching_cusp, parsed[, .(Cohort, Timepoint)])
    
    # Count by cohort and timepoint with match information
    stats$matching$by_cohort_timepoint <- matching_cusp[, .(
      Count = .N,
      Has_Y2_Match = sum(!is.na(Y2AutoMatch)),
      Has_Y3_Match = sum(!is.na(Y3AutoMatch)),
      Has_Both_Matches = sum(!is.na(Y2AutoMatch) & !is.na(Y3AutoMatch))
    ), by = .(RegSample, Cohort, Timepoint)]
    
    # Calculate cohort progression (T1 to T2/T3)
    # C2-t1 should match to C2-t2 (Y2) and C2-t3 (Y3)
    # C3-t1 should match to C3-t2 (Y2)
    # C4-t1 has no T2 - only T1 (accelerated longitudinal design)
    cohort_progression <- matching_cusp[
      !is.na(Cohort) & !is.na(Timepoint) & Timepoint == "t1",
      .(
        Total_T1 = .N,
        Matched_to_T2 = sum(!is.na(Y2AutoMatch)),
        Matched_to_T3 = sum(!is.na(Y3AutoMatch)),
        Matched_to_Both = sum(!is.na(Y2AutoMatch) & !is.na(Y3AutoMatch)),
        Not_Matched = sum(is.na(Y2AutoMatch) & is.na(Y3AutoMatch))
      ),
      by = .(RegSample, Cohort)
    ]
    
    stats$matching$cohort_progression <- cohort_progression
  }
  
  return(stats)
}

#' Check manual QC matches not picked up by auto matching
#' @param matching Final matching data.table
#' @param manualQCMatches Manual QC matches data.table
#' @param stats Statistics structure
#' @return Updated statistics structure
#' @export
check_manualQC_matches <- function(matching, manualQCMatches, stats) {
  if (is.null(manualQCMatches) || nrow(manualQCMatches) == 0) {
    return(stats)
  }
  
  setDT(matching)
  setDT(manualQCMatches)
  
  # This would need to be implemented based on the structure of manualQCMatches
  # For now, just count the number of suggested matches
  stats$matching$manualQC_suggested_not_auto <- nrow(manualQCMatches)
  
  return(stats)
}
