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
    
    # Registration by cohort timepoint
    registration_by_cohort_timepoint = data.table(),
    
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
    
    # Count by RegSample, Cohort, and Timepoint if dataTag exists
    if ("dataTag" %in% names(cusp_data)) {
      parsed <- parse_dataTag(cusp_data$dataTag)
      cusp_data <- cbind(cusp_data, parsed[, .(Cohort, Timepoint)])
      
      stats$registration$by_regsample_cohort_timepoint <- cusp_data[, .(
        Count = .N,
        Consent_Yes = sum(toupper(consent) == "YES", na.rm = TRUE),
        Consent_No = sum(toupper(consent) == "NO", na.rm = TRUE),
        Consent_Missing = sum(is.na(consent) | consent == "", na.rm = TRUE)
      ), by = .(RegSample, Cohort, Timepoint)]
    }
  }
  
  # Count consent status (CUSP only)
  if (nrow(cusp_data) > 0 && "consent" %in% names(cusp_data)) {
    stats$registration$consent_yes <- sum(toupper(cusp_data$consent) == "YES", na.rm = TRUE)
    stats$registration$consent_no <- sum(toupper(cusp_data$consent) == "NO", na.rm = TRUE)
    stats$registration$consent_missing <- sum(is.na(cusp_data$consent) | cusp_data$consent == "", na.rm = TRUE)
    
    # Consent by RegSample (if not already done above)
    if (is.null(stats$registration$consent_by_regsample) || nrow(stats$registration$consent_by_regsample) == 0) {
      stats$registration$consent_by_regsample <- cusp_data[, .(
        Total = .N,
        Consent_Yes = sum(toupper(consent) == "YES", na.rm = TRUE),
        Consent_No = sum(toupper(consent) == "NO", na.rm = TRUE),
        Consent_Missing = sum(is.na(consent) | consent == "", na.rm = TRUE)
      ), by = RegSample]
    }
  }
  
  # OPfS consent count
  if (nrow(opfs_data) > 0 && "consent" %in% names(opfs_data)) {
    stats$registration$opfs_consent_yes <- sum(toupper(opfs_data$consent) == "YES", na.rm = TRUE)
  }
  
  return(stats)
}

#' Calculate questionnaire linkage statistics (simplified - just linked or not)
#' @param matching Matching data.table (after merge with Q1/Q2)
#' @param registrationDF Registration data.table (to get all registered participants)
#' @param Q1 Q1 data.table
#' @param Q2 Q2 data.table
#' @param stats Statistics structure
#' @return Updated statistics structure
#' @export
calculate_questionnaire_linkage_stats <- function(matching, registrationDF, Q1, Q2, stats) {
  if (is.null(matching) || nrow(matching) == 0) {
    return(stats)
  }
  
  setDT(matching)
  setDT(Q1)
  setDT(Q2)
  setDT(registrationDF)
  
  # Filter to CUSP only for registration
  registration_cusp <- registrationDF[grepl('CUSP', RegSample, ignore.case = TRUE), ]
  
  # Get unique User.codes from Q1 and Q2
  q1_users <- unique(Q1$User.code)
  q2_users <- unique(Q2$User.code)
  q_users <- unique(c(q1_users, q2_users))
  
  # Calculate linkage from registration data (all registered participants)
  if (nrow(registration_cusp) > 0 && "dataTag" %in% names(registration_cusp)) {
    parsed <- parse_dataTag(registration_cusp$dataTag)
    registration_cusp <- cbind(registration_cusp, parsed[, .(Cohort, Timepoint)])
    
    # Calculate linkage by RegSample, Cohort, Timepoint
    linkage_by_group <- registration_cusp[, {
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

#' Calculate matching statistics using union-find to identify unique participants
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
    
    # Use union-find to identify unique participants (same approach as matching_summary.R)
    # Collect ALL User.codes that might ever be part of a chain
    all_relevant_users <- unique(c(matching_cusp$User.code,
                                   matching_cusp$Y2AutoMatch[!is.na(matching_cusp$Y2AutoMatch)],
                                   matching_cusp$Y3AutoMatch[!is.na(matching_cusp$Y3AutoMatch)]))
    
    # Initialize the parent map in a new environment for mutability
    parent_env <- new.env(hash = TRUE, parent = emptyenv())
    for (id in all_relevant_users) {
      if (!is.na(id) && id != "") {
        assign(id, id, envir = parent_env)
      }
    }
    
    # Union-Find helper functions
    find_root_uf <- function(id_val) {
      if (is.na(id_val) || id_val == "" || !exists(id_val, envir = parent_env)) {
        return(id_val)
      }
      path <- c()
      current_id <- id_val
      while (exists(current_id, envir = parent_env) && get(current_id, envir = parent_env) != current_id) {
        path <- c(path, current_id)
        current_id <- get(current_id, envir = parent_env)
      }
      # Path compression
      for (node in path) {
        assign(node, current_id, envir = parent_env)
      }
      return(current_id)
    }
    
    union_sets_uf <- function(id1, id2) {
      if (is.na(id1) || is.na(id2) || id1 == "" || id2 == "" ||
          !exists(id1, envir = parent_env) || !exists(id2, envir = parent_env)) {
        return(invisible(NULL))
      }
      root1 <- find_root_uf(id1)
      root2 <- find_root_uf(id2)
      if (root1 != root2) {
        new_root <- min(root1, root2)
        assign(root1, new_root, envir = parent_env)
        assign(root2, new_root, envir = parent_env)
      }
    }
    
    # Perform all Union operations
    for (i in seq_len(nrow(matching_cusp))) {
      current_user <- matching_cusp[i, User.code]
      y2_link <- matching_cusp[i, Y2AutoMatch]
      y3_link <- matching_cusp[i, Y3AutoMatch]
      
      # Extract actual user code from nameless matches (format: #NAMELESS#_UserCode)
      if (!is.na(y2_link) && y2_link != "") {
        y2_clean <- gsub("^#NAMELESS(MULTIPLE)?#_", "", y2_link)
        if (y2_clean != "" && exists(current_user, envir = parent_env) && exists(y2_clean, envir = parent_env)) {
          union_sets_uf(current_user, y2_clean)
        }
      }
      if (!is.na(y3_link) && y3_link != "") {
        y3_clean <- gsub("^#NAMELESS(MULTIPLE)?#_", "", y3_link)
        if (y3_clean != "" && exists(current_user, envir = parent_env) && exists(y3_clean, envir = parent_env)) {
          union_sets_uf(current_user, y3_clean)
        }
      }
    }
    
    # Assign Participant_ID to each row
    matching_cusp[, Participant_ID := sapply(User.code, function(x) {
      if (is.na(x) || x == "" || !exists(x, envir = parent_env)) {
        return(ifelse(is.na(x), NA_character_, x))
      }
      return(find_root_uf(x))
    }, USE.NAMES = FALSE)]
    
    # Count by cohort and timepoint - now counting unique participants, not rows
    stats$matching$by_cohort_timepoint <- matching_cusp[, .(
      Count_Rows = .N,
      Unique_Participants = length(unique(Participant_ID[!is.na(Participant_ID)])),
      Has_Y2_Match = sum(!is.na(Y2AutoMatch) & Y2AutoMatch != ""),
      Has_Y3_Match = sum(!is.na(Y3AutoMatch) & Y3AutoMatch != ""),
      Has_Both_Matches = sum(!is.na(Y2AutoMatch) & !is.na(Y3AutoMatch) & Y2AutoMatch != "" & Y3AutoMatch != "")
    ), by = .(RegSample, Cohort, Timepoint)]
    
    # Calculate cohort progression (T1 to T2/T3) - counting unique participants
    # Count T1 participants by cohort
    t1_participants <- matching_cusp[
      !is.na(Cohort) & !is.na(Timepoint) & Timepoint == "t1",
      .(Participant_ID, RegSample, Cohort, Y2AutoMatch, Y3AutoMatch, User.code)
    ]
    
    # Get unique T1 participants
    t1_unique <- unique(t1_participants[, .(Participant_ID, RegSample, Cohort)])
    
    # Count T2 participants by cohort
    t2_participants <- matching_cusp[
      !is.na(Cohort) & !is.na(Timepoint) & Timepoint == "t2",
      .(Participant_ID, RegSample, Cohort, User.code)
    ]
    t2_unique <- unique(t2_participants[, .(Participant_ID, RegSample, Cohort)])
    
    # Count T3 participants by cohort
    t3_participants <- matching_cusp[
      !is.na(Cohort) & !is.na(Timepoint) & Timepoint == "t3",
      .(Participant_ID, RegSample, Cohort, User.code)
    ]
    t3_unique <- unique(t3_participants[, .(Participant_ID, RegSample, Cohort)])
    
    # Create lookup for matched User.codes to their cohort/timepoint
    # This helps us validate if auto-matches are in the correct cohort/timepoint
    match_lookup <- matching_cusp[
      !is.na(Cohort) & !is.na(Timepoint),
      .(User.code, Match_Cohort = Cohort, Match_Timepoint = Timepoint)
    ]
    setkey(match_lookup, User.code)
    
    # For each T1 participant, check if their Y2/Y3 matches are in the correct cohort/timepoint
    t1_with_validated_matches <- t1_participants[, {
      # Check Y2 matches - collect all Y2AutoMatch codes for this participant
      y2_match_codes <- unique(Y2AutoMatch[!is.na(Y2AutoMatch) & Y2AutoMatch != ""])
      has_y2_valid <- FALSE
      if (length(y2_match_codes) > 0) {
        # Clean nameless match prefixes
        y2_clean <- unique(gsub("^#NAMELESS(MULTIPLE)?#_", "", y2_match_codes))
        # Look up matched records
        matched_info <- match_lookup[y2_clean, nomatch = NULL]
        # Check if any are in the correct cohort and timepoint (T2)
        if (nrow(matched_info) > 0) {
          has_y2_valid <- any(matched_info$Match_Cohort == Cohort[1] & 
                              matched_info$Match_Timepoint == "t2", na.rm = TRUE)
        }
      }
      
      # Check Y3 matches - collect all Y3AutoMatch codes for this participant
      y3_match_codes <- unique(Y3AutoMatch[!is.na(Y3AutoMatch) & Y3AutoMatch != ""])
      has_y3_valid <- FALSE
      if (length(y3_match_codes) > 0) {
        # Clean nameless match prefixes
        y3_clean <- unique(gsub("^#NAMELESS(MULTIPLE)?#_", "", y3_match_codes))
        # Look up matched records
        matched_info <- match_lookup[y3_clean, nomatch = NULL]
        # Check if any are in the correct cohort and timepoint (T3)
        if (nrow(matched_info) > 0) {
          has_y3_valid <- any(matched_info$Match_Cohort == Cohort[1] & 
                              matched_info$Match_Timepoint == "t3", na.rm = TRUE)
        }
      }
      
      list(
        Has_Y2_Valid = has_y2_valid,
        Has_Y3_Valid = has_y3_valid
      )
    }, by = .(Participant_ID, RegSample, Cohort)]
    
    if (nrow(t1_unique) > 0) {
      # Merge T1 counts with validated matches
      cohort_progression <- merge(t1_unique, t1_with_validated_matches, 
                                  by = c("Participant_ID", "RegSample", "Cohort"), all.x = TRUE)
      
      # Fill in NA values
      cohort_progression[is.na(Has_Y2_Valid), Has_Y2_Valid := FALSE]
      cohort_progression[is.na(Has_Y3_Valid), Has_Y3_Valid := FALSE]
      
      # Merge with T2 and T3 counts
      t2_counts <- t2_unique[, .(T2_Participants = .N), by = .(RegSample, Cohort)]
      t3_counts <- t3_unique[, .(T3_Participants = .N), by = .(RegSample, Cohort)]
      
      cohort_progression <- merge(cohort_progression, t2_counts, 
                                  by = c("RegSample", "Cohort"), all.x = TRUE)
      cohort_progression <- merge(cohort_progression, t3_counts, 
                                  by = c("RegSample", "Cohort"), all.x = TRUE)
      
      # Fill in missing T2/T3 counts with 0
      cohort_progression[is.na(T2_Participants), T2_Participants := 0L]
      cohort_progression[is.na(T3_Participants), T3_Participants := 0L]
      
      # Aggregate by RegSample and Cohort
      cohort_progression <- cohort_progression[, .(
        Total_T1_Participants = .N,
        T2_Participants = max(T2_Participants),  # Should be same for all rows in group
        T2_Valid_Matches = sum(Has_Y2_Valid, na.rm = TRUE),  # Count of unique T1 participants with valid Y2 matches
        T3_Participants = max(T3_Participants),  # Should be same for all rows in group
        T3_Valid_Matches = sum(Has_Y3_Valid, na.rm = TRUE),  # Count of unique T1 participants with valid Y3 matches
        Matched_to_Both = sum(Has_Y2_Valid & Has_Y3_Valid, na.rm = TRUE)
      ), by = .(RegSample, Cohort)]
      
      stats$matching$cohort_progression <- cohort_progression
    } else {
      # No T1 participants found - create empty table
      stats$matching$cohort_progression <- data.table(
        RegSample = character(),
        Cohort = character(),
        Total_T1_Participants = integer(),
        T2_Participants = integer(),
        T2_Valid_Matches = integer(),
        T3_Participants = integer(),
        T3_Valid_Matches = integer(),
        Matched_to_Both = integer()
      )
    }
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
