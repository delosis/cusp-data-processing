library(data.table)

get_matching_summary <- function(matching_df) {
  
  # Ensure the input is a data.table and required columns are character
  matching <- as.data.table(matching_df)
  matching[, c("User.code", "Y2AutoMatch", "Y3AutoMatch", "AutoDuplicate", "RegSample", "dataTag") := lapply(.SD, as.character),
           .SDcols = c("User.code", "Y2AutoMatch", "Y3AutoMatch", "AutoDuplicate", "RegSample", "dataTag")]
  
  # --- Add a numerical timepoint column ---
  # Extract the number after 't' from dataTag
  matching[, TimepointNum := as.integer(gsub(".*-t(\\d+)$", "\\1", dataTag))]
  
  # Check for NAs introduced if dataTag format isn't as expected
  if(any(is.na(matching$TimepointNum))) {
    warning("Some dataTag values could not be parsed into TimepointNum. Check format.")
    # You might want to filter these out or handle them specifically if they're problematic
  }
  
  
  # --- 1. Identify Unique Participants and their chains using an environment-based Union-Find ---
  
  # Collect ALL User.codes that might ever be part of a chain
  all_relevant_users <- unique(c(matching$User.code,
                                 matching$Y2AutoMatch[!is.na(matching$Y2AutoMatch)],
                                 matching$Y3AutoMatch[!is.na(matching$Y3AutoMatch)]))
  
  # Initialize the parent map in a new environment for mutability
  parent_env <- new.env(hash = TRUE, parent = emptyenv())
  for (id in all_relevant_users) {
    assign(id, id, envir = parent_env) # Each ID initially points to itself
  }
  
  # --- Define Union-Find helper functions (closure over parent_env) ---
  find_root_uf <- function(id_val) {
    path <- c()
    current_id <- id_val
    # Ensure current_id exists in the environment before attempting get()
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
    # Ensure both IDs exist in the environment before finding roots
    if (!exists(id1, envir = parent_env) || !exists(id2, envir = parent_env)) {
      # This case should ideally not happen if all_relevant_users is correctly populated
      # and the environment initialized. Adding this check for robustness.
      warning(paste("Attempting to union unknown IDs:", id1, "or", id2))
      return(invisible(NULL))
    }
    
    root1 <- find_root_uf(id1)
    root2 <- find_root_uf(id2)
    if (root1 != root2) {
      new_root <- min(root1, root2) # Pick lexicographically smaller root for consistency
      assign(root1, new_root, envir = parent_env)
      assign(root2, new_root, envir = parent_env)
    }
  }
  
  # --- Perform all Union operations first ---
  for (i in seq_len(nrow(matching))) {
    current_user <- matching[i, User.code]
    y2_link <- matching[i, Y2AutoMatch]
    y3_link <- matching[i, Y3AutoMatch]
    
    # Only attempt to union if both current_user and the link exist in our map
    # (Checking existence in parent_env for robustness, though all_relevant_users should cover this)
    if (!is.na(y2_link) && exists(current_user, envir = parent_env) && exists(y2_link, envir = parent_env)) {
      union_sets_uf(current_user, y2_link)
    }
    if (!is.na(y3_link) && exists(current_user, envir = parent_env) && exists(y3_link, envir = parent_env)) {
      union_sets_uf(current_user, y3_link)
    }
  }
  
  # --- After all Unions are done, then assign final Participant_ID by finding roots ---
  # Apply find_root_uf to every User.code in the original 'matching' data.table
  matching[, Participant_ID := sapply(User.code, find_root_uf, USE.NAMES = FALSE)]
  
  
  # --- 2. Characterize Each Unique Participant Chain ---
  participant_details <- matching[, {
    # All *numerical* timepoints associated with this participant chain
    actual_timepoints_present <- unique(TimepointNum)
    
    # Calculate num_linked_time_points based on the number of distinct timepoints
    # The number of additional time points is simply `(number of distinct timepoints) - 1`
    # if there's more than one distinct timepoint.
    num_distinct_timepoints <- length(actual_timepoints_present)
    num_linked_time_points <- max(0, num_distinct_timepoints - 1)
    
    
    # Check if this participant chain includes any record that had AutoDuplicate != NA
    is_duplicated_participant_chain <- any(!is.na(AutoDuplicate))
    
    list(
      RegSample = unique(RegSample),
      NumLinkedTimePoints = num_linked_time_points,
      IsDuplicatedParticipantChain = is_duplicated_participant_chain
    )
  }, by = Participant_ID]
  
  # --- 3. Generate the Final Summary ---
  # You do not want to split by IsDuplicatedParticipantChain for the main summary.
  final_summary_table <- participant_details[, .(
    Count = .N
  ), by = .(RegSample, NumLinkedTimePoints)]
  
  setorder(final_summary_table, RegSample, NumLinkedTimePoints)
  
  message("--- Summary of Participants by Linked Time Points and RegSample ---")
  print(final_summary_table)
  
  message("\nExplanation of Summary Table Columns:")
  message("- RegSample: The registration sample.")
  message("- NumLinkedTimePoints: The number of *additional* time points a participant's chain includes beyond their earliest recorded year.")
  message("  - 0: Participant only has a record for their earliest timepoint.")
  message("  - 1: Participant has records for two distinct timepoints (e.g., t1 and t2, or t1 and t3, etc.).")
  message("  - 2: Participant has records for three distinct timepoints (e.g., t1, t2, and t3).")
  message("- Count: The number of unique participants (chains of User.codes) that fit the given RegSample and NumLinkedTimePoints criteria.")
  
  # Summary of records explicitly flagged by AutoDuplicate (still useful as an extra piece of info)
  records_flagged_as_primary_for_duplicates <- matching[!is.na(AutoDuplicate), .(NumFlaggedRecords = .N), by = RegSample]
  message("\n--- Number of Records (Rows) that were the Primary Match for a Duplicate Set (AutoDuplicate != NA) ---")
  print(records_flagged_as_primary_for_duplicates)
  
  invisible(list(
    summary_by_participant_type = final_summary_table,
    records_flagged_duplicates_summary = records_flagged_as_primary_for_duplicates
  ))
}

# --- Dummy Data adjusted for "c3-t1" format ---
test_matching_data_correct_datatag <- data.table(
  User.code = c("A_1", "A_2", "A_3",  # Full 3-timepoint participant A
                "B_1", "B_2",        # 2-timepoint participant B
                "C_1", "C_3",        # 2-timepoint participant C (t1 to t3 skip)
                "D_1",                # 1-timepoint participant D
                "E_2", "E_3",        # 2-timepoint participant E (starting t2)
                "F_3",                # 1-timepoint participant F (starting t3)
                "G_1"),               # Participant G, t1 primary of a duplicate
  dataTag = c("c1-t1", "c1-t2", "c1-t3",
              "c1-t1", "c1-t2",
              "c2-t1", "c2-t3",
              "c2-t1",
              "c1-t2", "c1-t3",
              "c2-t3",
              "c1-t1"),
  RegSample = c("R1", "R1", "R1",
                "R1", "R1",
                "R2", "R2",
                "R2",
                "R1", "R1",
                "R2",
                "R1"),
  AutoDuplicate = c(NA, NA, NA,
                    NA, NA,
                    NA, NA,
                    NA,
                    NA, NA,
                    NA,
                    "G_1_master_of_dup_flag"), # G was a master of a duplicate (flagged)
  Y2AutoMatch = c("A_2", NA, NA,
                  "B_2", NA,
                  NA, NA,
                  NA,
                  "E_3", NA,
                  NA,
                  NA),
  Y3AutoMatch = c("A_3", NA, NA,
                  NA, NA,
                  "C_3", NA,
                  NA,
                  NA, NA,
                  NA,
                  NA)
)

# Example call:
# my_summary_output <- get_matching_summary(test_matching_data_correct_datatag)
