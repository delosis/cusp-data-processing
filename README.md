# CUSP Data Processing Functions

This repository contains R functions for processing CUSP data. The code includes functions for:

- Data quality control (QC) and validation
- Questionnaire scoring and analysis
- Task performance metrics
- Data anonymization
- School-level analysis

## Quality Control (QC) Flags

The system implements several QC flags to identify potential data quality issues:

### Questionnaire QC Flags

1. **DEPAPO Foil QC** (`QC_DEPAPO_FOIL`)
   - Flagged when participant responds to the foil item (C2_W)
   - Indicates potential inattentive responding

2. **DEPAPO Uncommon Drugs QC** (`QC_DEPAPO_UNCOMMON2.5`)
   - Flagged when mean score on uncommon drugs > 2.5
   - Uncommon drugs include: SPICE, STEROIDS, STIMULANTS, SEDATIVES, COCAINE, HEROIN, LSD
   - Indicates potential over-reporting or inattentive responding

3. **SURPS One Answer QC** (`QC_SURPS_ONE_ANSWER`)
   - Flagged when all SURPS items have the same response
   - Indicates potential straight-lining or inattentive responding

4. **SDQ One Answer QC** (`QC_SDQ_ONE_ANSWER`)
   - Flagged when all SDQ items have the same response
   - Indicates potential straight-lining or inattentive responding

### Task Performance QC Flags

1. **PALP Task QC**
   - `PALP_QC_PALP_WORSE_THAN_CHANCE`: Performance worse than chance at 5% significance level
   - `PALP_QC_PALP_TOO_FAST`: Response times consistently below 200ms

2. **CFT Task QC**
   - `CFT_QC_CFT_ONE_RESPONSE`: All responses are identical
   - Indicates potential inattentive responding or task misunderstanding

## School Anonymization Procedure

The school anonymization process follows these steps:

1. **School ID Processing**
   - All school IDs are converted to lowercase
   - Test schools are filtered out
   - School IDs are mapped to anonymized 3-digit codes

2. **Anonymization Process**
   - Uses a persistent mapping stored in `school_id_keys.env`
   - Each school receives a unique 3-digit code
   - Mapping is maintained across sessions
   - New schools are automatically assigned available codes

3. **Data Protection**
   - Original school IDs are never stored in output files
   - School codes are randomly assigned
   - Mapping file is kept separate from data files

4. **School Year Assignment**
   - Automatically assigned based on processed timestamp:
     - 20-21: After 2020-08-01
     - 21-22: After 2021-08-01
     - 22-23: After 2022-08-01
     - 23-24: After 2023-08-01

5. **District Assignment**
   - Schools are grouped into districts based on their IDs
   - District information is preserved while maintaining anonymity

## Duplicate and Inter-Year Matching

The system includes procedures for identifying and handling duplicate records and matching participants across different school years:

### Duplicate Detection

1. **Manual QC Flags**
   - Uses `ManualQC.xlsx` to identify potential duplicates
   - Flags are set based on manual review of participant data
   - Includes `ManualQC.SuggestedDuplicate` field for potential matches

2. **Automatic Matching**
   - Matches are based on multiple criteria:
     - School ID (if present)
     - Name (with fuzzy matching using string distance)
     - Date of birth
     - Gende
     - Time window between records
   - Handles fake names and potential fake names appropriately
   - Supports nameless matches when name is not available

3. **Duplicate Resolution**
   - When duplicates are confirmed:
     - Keeps the earliest record by date
     - Transfers any Y2/Y3 matches from removed records to kept records
     - Maintains a mapping of kept to removed IDs
     - Saves duplicate resolution mapping to a separate file

### Inter-Year Matching

1. **Matching Process**
   - Uses separate fields for Y2 and Y3 matches (Y2AutoMatch, Y3AutoMatch)
   - Matches are stored in the same data structure as the original records
   - Original IDs are preserved in their respective years
   - **Note: Matching is forward-only - a participant in Year 3 will not be matched with their earlier timepoints, but their earlier timepoints will be linked to it**
   - Matching is based on:
     - School ID (if present)
     - Name similarity (with fuzzy matching)
     - Date of birth
     - Gender
     - Time window between records

2. **Time Windows**
   - Time windows are strictly defined for different matching scenarios:
     - Duplicate detection: -10 to +10 days
     - Year 2 matching: 150 to 551 days (approximately 5-18 months)
     - Year 3 matching: 550 to 950 days (approximately 18-31 months)
   - Name matching uses adaptive string distance:
     - Distance threshold is 1/4 of first name length (minimum 0.1)
     - Uses Damerau-Levenshtein distance with weighted operations:
       - Deletions and insertions: 0.5 weight
       - Substitutions and transpositions: 1.0 weight

3. **Unexpected Delays**
   - Matches with date discrepancies are saved to a separate review file when:
     - Year 2 matches: Testing dates differ by more than 30 days from expected 1-year interval
     - Year 3 matches: Testing dates differ by more than 30 days from expected 2-year interval
   - The date discrepancy file includes:
     - Original user code
     - Original testing date
     - Matched user codes (Y2 and Y3)
     - Matched testing dates
   - These discrepancies require manual verification before being accepted
   - Helps identify potential data quality issues or unusual testing patterns

4. **Quality Control**
   - Identifies nameless matches for manual review
   - Detects date discrepancies between matched records
   - Flags inconsistent matches for manual review
   - Supports manual override through QC files

5. **Data Protection**
   - Maintains separate fields for automatic and manual matches
   - Preserves original IDs while tracking matches
   - Supports anonymized school codes for matching
   - Handles missing or refused demographic information

## File Structure

- `CuspTaskFunctions.R`: Core functions for task data processing
- `CuspMatchingFunctions.R`: Functions for matching and anonymizing participant data
- `entryPoint.R`: Main script for processing data
- `school_id_keys.env`: Stores the school ID mapping (not included in repository)

## Dependencies

- data.table
- Other R packages as specified in the code

## Files

- `CuspDownloadFunctions.R`: Functions for downloading and processing CUSP data
- `CuspMatchingFunctions.R`: Functions for matching and anonymizing participant data
- `CuspTaskFunctions.R`: Functions for processing task-specific data (PALP, CMS, CFT, SWM)
- `entryPoint.R`: Main script for downloading and processing CUSP data

## Required External Files

The following files are required for processing but are not included in the repository for privacy reasons:

- `discardIDs.xlsx`: Contains participant IDs to be excluded from processing
- `ManualQC.xlsx`: Contains manual quality control flags and decisions

These files should be placed in the same directory as the R scripts.

## Usage

See individual function documentation for usage details. The main workflow is:

1. Download data using `entryPoint.R`
2. Process questionnaires using functions in `CuspDownloadFunctions.R`
3. Process task data using functions in `CuspTaskFunctions.R`
4. Match and anonymize data using functions in `CuspMatchingFunctions.R` 