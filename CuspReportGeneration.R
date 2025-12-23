# CUSP Report Generation Functions

library(data.table)

#' Generate HTML report from statistics
#' @param stats Statistics structure from collection functions
#' @param output_file Path to output HTML file
#' @param run_date Date of the run (optional)
#' @export
generate_html_report <- function(stats, output_file = "CUSP_Run_Statistics.html", run_date = NULL) {
  if (is.null(run_date)) {
    run_date <- format(Sys.Date(), "%Y-%m-%d")
  }
  
  html_content <- c()
  
  # HTML header
  html_content <- c(html_content, '<!DOCTYPE html>')
  html_content <- c(html_content, '<html lang="en">')
  html_content <- c(html_content, '<head>')
  html_content <- c(html_content, '  <meta charset="UTF-8">')
  html_content <- c(html_content, '  <meta name="viewport" content="width=device-width, initial-scale=1.0">')
  html_content <- c(html_content, '  <title>CUSP Data Processing Run Statistics</title>')
  html_content <- c(html_content, '  <style>')
  html_content <- c(html_content, '    body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }')
  html_content <- c(html_content, '    .container { max-width: 1200px; margin: 0 auto; background-color: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }')
  html_content <- c(html_content, '    h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }')
  html_content <- c(html_content, '    h2 { color: #34495e; margin-top: 30px; border-bottom: 2px solid #ecf0f1; padding-bottom: 5px; }')
  html_content <- c(html_content, '    h3 { color: #7f8c8d; margin-top: 20px; }')
  html_content <- c(html_content, '    table { width: 100%; border-collapse: collapse; margin: 15px 0; }')
  html_content <- c(html_content, '    th { background-color: #3498db; color: white; padding: 12px; text-align: left; font-weight: bold; }')
  html_content <- c(html_content, '    td { padding: 10px; border-bottom: 1px solid #ecf0f1; }')
  html_content <- c(html_content, '    tr:hover { background-color: #f8f9fa; }')
  html_content <- c(html_content, '    .summary-box { background-color: #ecf0f1; padding: 15px; border-radius: 5px; margin: 15px 0; }')
  html_content <- c(html_content, '    .number { font-weight: bold; color: #2c3e50; font-size: 1.2em; }')
  html_content <- c(html_content, '    .section { margin: 30px 0; }')
  html_content <- c(html_content, '    .note { font-style: italic; color: #7f8c8d; margin: 10px 0; }')
  html_content <- c(html_content, '  </style>')
  html_content <- c(html_content, '</head>')
  html_content <- c(html_content, '<body>')
  html_content <- c(html_content, '  <div class="container">')
  html_content <- c(html_content, sprintf('    <h1>CUSP Data Processing Run Statistics</h1>'))
  html_content <- c(html_content, sprintf('    <p><strong>Run Date:</strong> %s</p>', run_date))
  
  # OPfS Exclusion Note
  html_content <- c(html_content, '    <div class="section">')
  html_content <- c(html_content, '      <h2>OPfS Sample Exclusion</h2>')
  html_content <- c(html_content, '      <div class="summary-box">')
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> OPfS registrations excluded from CUSP reporting</p>', stats$registration$opfs_total))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> OPfS registrations with consent (YES)</p>', stats$registration$opfs_consent_yes))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> OPfS records removed from matching data</p>', stats$exclusions$opfs_removed))
  html_content <- c(html_content, '        <p class="note">Note: OPfS samples are excluded from all subsequent tables and statistics.</p>')
  html_content <- c(html_content, '      </div>')
  html_content <- c(html_content, '    </div>')
  
  # Registration Statistics
  html_content <- c(html_content, '    <div class="section">')
  html_content <- c(html_content, '      <h2>CUSP Registration Statistics</h2>')
  html_content <- c(html_content, '      <div class="summary-box">')
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> total CUSP registrations</p>', stats$registration$total))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> with consent (YES)</p>', stats$registration$consent_yes))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> without consent (NO)</p>', stats$registration$consent_no))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> with missing consent data</p>', stats$registration$consent_missing))
  html_content <- c(html_content, '      </div>')
  
  if (!is.null(stats$registration$by_regsample) && nrow(stats$registration$by_regsample) > 0) {
    html_content <- c(html_content, '      <h3>Registrations by Site</h3>')
    html_content <- c(html_content, '      <table>')
    html_content <- c(html_content, '        <tr><th>RegSample</th><th>Count</th></tr>')
    for (i in 1:nrow(stats$registration$by_regsample)) {
      html_content <- c(html_content, sprintf('        <tr><td>%s</td><td>%d</td></tr>',
                                               stats$registration$by_regsample$RegSample[i],
                                               stats$registration$by_regsample$Count[i]))
    }
    html_content <- c(html_content, '      </table>')
  }
  
  if (!is.null(stats$registration$consent_by_regsample) && nrow(stats$registration$consent_by_regsample) > 0) {
    html_content <- c(html_content, '      <h3>Consent Status by Site</h3>')
    html_content <- c(html_content, '      <table>')
    html_content <- c(html_content, '        <tr><th>RegSample</th><th>Total</th><th>Yes</th><th>No</th><th>Missing</th></tr>')
    for (i in 1:nrow(stats$registration$consent_by_regsample)) {
      html_content <- c(html_content, sprintf('        <tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>',
                                               stats$registration$consent_by_regsample$RegSample[i],
                                               stats$registration$consent_by_regsample$Total[i],
                                               stats$registration$consent_by_regsample$Consent_Yes[i],
                                               stats$registration$consent_by_regsample$Consent_No[i],
                                               stats$registration$consent_by_regsample$Consent_Missing[i]))
    }
    html_content <- c(html_content, '      </table>')
  }
  html_content <- c(html_content, '    </div>')
  
  # Questionnaire Linkage (Simplified)
  html_content <- c(html_content, '    <div class="section">')
  html_content <- c(html_content, '      <h2>Questionnaire Linkage</h2>')
  html_content <- c(html_content, '      <div class="summary-box">')
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> linked to questionnaire data (Q1 or Q2)</p>', stats$questionnaire_linkage$linked))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> not linked to questionnaire data</p>', stats$questionnaire_linkage$not_linked))
  html_content <- c(html_content, '      </div>')
  
  if (!is.null(stats$questionnaire_linkage$by_regsample_cohort_timepoint) && 
      nrow(stats$questionnaire_linkage$by_regsample_cohort_timepoint) > 0) {
    # Sort by RegSample, then Cohort, then Timepoint
    setDT(stats$questionnaire_linkage$by_regsample_cohort_timepoint)
    setorder(stats$questionnaire_linkage$by_regsample_cohort_timepoint, RegSample, Cohort, Timepoint)
    
    html_content <- c(html_content, '      <h3>Questionnaire Linkage by Site, Cohort, and Timepoint</h3>')
    html_content <- c(html_content, '      <table>')
    html_content <- c(html_content, '        <tr><th>RegSample</th><th>Cohort</th><th>Timepoint</th><th>Total</th><th>Linked</th><th>Not Linked</th></tr>')
    for (i in 1:nrow(stats$questionnaire_linkage$by_regsample_cohort_timepoint)) {
      html_content <- c(html_content, sprintf('        <tr><td>%s</td><td>%s</td><td>%s</td><td>%d</td><td>%d</td><td>%d</td></tr>',
                                               stats$questionnaire_linkage$by_regsample_cohort_timepoint$RegSample[i],
                                               ifelse(is.na(stats$questionnaire_linkage$by_regsample_cohort_timepoint$Cohort[i]), "N/A", stats$questionnaire_linkage$by_regsample_cohort_timepoint$Cohort[i]),
                                               ifelse(is.na(stats$questionnaire_linkage$by_regsample_cohort_timepoint$Timepoint[i]), "N/A", stats$questionnaire_linkage$by_regsample_cohort_timepoint$Timepoint[i]),
                                               stats$questionnaire_linkage$by_regsample_cohort_timepoint$Total[i],
                                               stats$questionnaire_linkage$by_regsample_cohort_timepoint$Linked[i],
                                               stats$questionnaire_linkage$by_regsample_cohort_timepoint$Not_Linked[i]))
    }
    html_content <- c(html_content, '      </table>')
  }
  html_content <- c(html_content, '    </div>')
  
  # Exclusions
  html_content <- c(html_content, '    <div class="section">')
  html_content <- c(html_content, '      <h2>Exclusions</h2>')
  html_content <- c(html_content, '      <div class="summary-box">')
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> removed via discardIDs</p>', stats$exclusions$discardIDs))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> test accounts removed</p>', stats$exclusions$test_accounts))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> removed via manualQC exclude (User.code)</p>', stats$exclusions$manualQC_exclude_usercode))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> removed via manualQC exclude (Rid)</p>', stats$exclusions$manualQC_exclude_rid))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> total excluded</p>', stats$exclusions$total_excluded))
  html_content <- c(html_content, '      </div>')
  html_content <- c(html_content, '    </div>')
  
  # Duplicates (from duplicate_map)
  html_content <- c(html_content, '    <div class="section">')
  html_content <- c(html_content, '      <h2>Duplicate Resolution</h2>')
  html_content <- c(html_content, '      <div class="summary-box">')
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> total duplicates merged</p>', stats$duplicates$total_merged))
  html_content <- c(html_content, '      </div>')
  
  if (!is.null(stats$duplicates$by_regsample_cohort_timepoint) && 
      nrow(stats$duplicates$by_regsample_cohort_timepoint) > 0) {
    # Sort by RegSample, then Cohort, then Timepoint
    setDT(stats$duplicates$by_regsample_cohort_timepoint)
    setorder(stats$duplicates$by_regsample_cohort_timepoint, RegSample, Cohort, Timepoint)
    
    html_content <- c(html_content, '      <h3>Duplicates Merged by Site, Cohort, and Timepoint</h3>')
    html_content <- c(html_content, '      <table>')
    html_content <- c(html_content, '        <tr><th>RegSample</th><th>Cohort</th><th>Timepoint</th><th>Count</th></tr>')
    for (i in 1:nrow(stats$duplicates$by_regsample_cohort_timepoint)) {
      html_content <- c(html_content, sprintf('        <tr><td>%s</td><td>%s</td><td>%s</td><td>%d</td></tr>',
                                               stats$duplicates$by_regsample_cohort_timepoint$RegSample[i],
                                               ifelse(is.na(stats$duplicates$by_regsample_cohort_timepoint$Cohort[i]), "N/A", stats$duplicates$by_regsample_cohort_timepoint$Cohort[i]),
                                               ifelse(is.na(stats$duplicates$by_regsample_cohort_timepoint$Timepoint[i]), "N/A", stats$duplicates$by_regsample_cohort_timepoint$Timepoint[i]),
                                               stats$duplicates$by_regsample_cohort_timepoint$Count[i]))
    }
    html_content <- c(html_content, '      </table>')
  }
  html_content <- c(html_content, '    </div>')
  
  # Matching Statistics
  html_content <- c(html_content, '    <div class="section">')
  html_content <- c(html_content, '      <h2>Matching Statistics</h2>')
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> nameless matches</p>', stats$matching$nameless_matches))
  
  if (!is.null(stats$matching$cohort_progression) && nrow(stats$matching$cohort_progression) > 0) {
    # Sort by RegSample, then Cohort
    setDT(stats$matching$cohort_progression)
    setorder(stats$matching$cohort_progression, RegSample, Cohort)
    
    html_content <- c(html_content, '      <h3>Cohort Progression (T1 to T2/T3)</h3>')
    html_content <- c(html_content, '      <p class="note">Note: C4 participants only have T1 (no T2 expected). C2 should progress to T2 and T3, C3 should progress to T2.</p>')
    html_content <- c(html_content, '      <table>')
    html_content <- c(html_content, '        <tr><th>RegSample</th><th>Cohort</th><th>Total T1</th><th>Matched to T2</th><th>Matched to T3</th><th>Matched to Both</th><th>Not Matched</th></tr>')
    for (i in 1:nrow(stats$matching$cohort_progression)) {
      html_content <- c(html_content, sprintf('        <tr><td>%s</td><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>',
                                               stats$matching$cohort_progression$RegSample[i],
                                               stats$matching$cohort_progression$Cohort[i],
                                               stats$matching$cohort_progression$Total_T1[i],
                                               stats$matching$cohort_progression$Matched_to_T2[i],
                                               stats$matching$cohort_progression$Matched_to_T3[i],
                                               stats$matching$cohort_progression$Matched_to_Both[i],
                                               stats$matching$cohort_progression$Not_Matched[i]))
    }
    html_content <- c(html_content, '      </table>')
  }
  
  if (!is.null(stats$matching$by_cohort_timepoint) && nrow(stats$matching$by_cohort_timepoint) > 0) {
    # Sort by RegSample, then Cohort, then Timepoint
    setDT(stats$matching$by_cohort_timepoint)
    setorder(stats$matching$by_cohort_timepoint, RegSample, Cohort, Timepoint)
    
    html_content <- c(html_content, '      <h3>Participants by Cohort and Timepoint</h3>')
    html_content <- c(html_content, '      <table>')
    html_content <- c(html_content, '        <tr><th>RegSample</th><th>Cohort</th><th>Timepoint</th><th>Count</th><th>Has Y2 Match</th><th>Has Y3 Match</th><th>Has Both</th></tr>')
    for (i in 1:nrow(stats$matching$by_cohort_timepoint)) {
      html_content <- c(html_content, sprintf('        <tr><td>%s</td><td>%s</td><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>',
                                               stats$matching$by_cohort_timepoint$RegSample[i],
                                               ifelse(is.na(stats$matching$by_cohort_timepoint$Cohort[i]), "N/A", stats$matching$by_cohort_timepoint$Cohort[i]),
                                               ifelse(is.na(stats$matching$by_cohort_timepoint$Timepoint[i]), "N/A", stats$matching$by_cohort_timepoint$Timepoint[i]),
                                               stats$matching$by_cohort_timepoint$Count[i],
                                               stats$matching$by_cohort_timepoint$Has_Y2_Match[i],
                                               stats$matching$by_cohort_timepoint$Has_Y3_Match[i],
                                               stats$matching$by_cohort_timepoint$Has_Both_Matches[i]))
    }
    html_content <- c(html_content, '      </table>')
  }
  html_content <- c(html_content, '    </div>')
  
  # DataTag Parsing Issues
  html_content <- c(html_content, '    <div class="section">')
  html_content <- c(html_content, '      <h2>DataTag Parsing</h2>')
  html_content <- c(html_content, '      <div class="summary-box">')
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> total records</p>', stats$datatag_parsing$total_records))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> cohort parsed successfully</p>', stats$datatag_parsing$cohort_parsed))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> cohort missing/unparseable</p>', stats$datatag_parsing$cohort_missing))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> timepoint parsed successfully</p>', stats$datatag_parsing$timepoint_parsed))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> timepoint missing/unparseable</p>', stats$datatag_parsing$timepoint_missing))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> both parsed successfully</p>', stats$datatag_parsing$both_parsed))
  html_content <- c(html_content, sprintf('        <p><span class="number">%d</span> neither parsed</p>', stats$datatag_parsing$neither_parsed))
  html_content <- c(html_content, '      </div>')
  html_content <- c(html_content, '    </div>')
  
  # Final Counts by Checkpoint (ordered by RegSample then Checkpoint)
  if (!is.null(stats$final_counts$by_regsample_cohort_timepoint) && 
      nrow(stats$final_counts$by_regsample_cohort_timepoint) > 0) {
    html_content <- c(html_content, '    <div class="section">')
    html_content <- c(html_content, '      <h2>Participant Counts by Processing Stage</h2>')
    html_content <- c(html_content, '      <p class="note">Ordered by RegSample then Checkpoint. Split by Cohort and Timepoint.</p>')
    
    # Filter out Q1/Q2 load checkpoints (they count data rows, not participants)
    setDT(stats$final_counts$by_regsample_cohort_timepoint)
    final_counts_filtered <- stats$final_counts$by_regsample_cohort_timepoint[
      !Checkpoint %in% c("After_Q1_Load", "After_Q1_Load_Checkpoint", "After_Q2_Load", "After_Q2_Load_Checkpoint")
    ]
    
    # Order by RegSample then Checkpoint
    setorder(final_counts_filtered, RegSample, Checkpoint, Cohort, Timepoint)
    
    html_content <- c(html_content, '      <table>')
    html_content <- c(html_content, '        <tr><th>RegSample</th><th>Checkpoint</th><th>Cohort</th><th>Timepoint</th><th>Count</th></tr>')
    for (i in 1:nrow(final_counts_filtered)) {
      html_content <- c(html_content, sprintf('        <tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%d</td></tr>',
                                               final_counts_filtered$RegSample[i],
                                               final_counts_filtered$Checkpoint[i],
                                               ifelse(is.na(final_counts_filtered$Cohort[i]), "N/A", final_counts_filtered$Cohort[i]),
                                               ifelse(is.na(final_counts_filtered$Timepoint[i]), "N/A", final_counts_filtered$Timepoint[i]),
                                               final_counts_filtered$Count[i]))
    }
    html_content <- c(html_content, '      </table>')
    html_content <- c(html_content, '    </div>')
  }
  
  # Close HTML
  html_content <- c(html_content, '  </div>')
  html_content <- c(html_content, '</body>')
  html_content <- c(html_content, '</html>')
  
  # Write to file
  writeLines(html_content, output_file)
  message(sprintf("HTML report generated: %s", output_file))
  
  return(invisible(NULL))
}
