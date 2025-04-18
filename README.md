# CUSP Data Processing

This repository contains R functions for processing CUSP (Cambridge University School Project) data. The code includes functions for:

- Data downloading and processing
- Task-specific summary calculations
- Quality control flagging
- Data anonymization

## Files

- `CuspDownloadFunctions.R`: Functions for downloading and processing CUSP data
- `CuspMatchingFunctions.R`: Functions for matching and anonymizing participant data
- `CuspTaskFunctions.R`: Functions for processing task-specific data (PALP, CMS, CFT, SWM)
- `GrabCuspDataMine.R`: Main script for downloading and processing CUSP data

## Usage

See individual function documentation for usage details. The main workflow is:

1. Download data using `GrabCuspDataMine.R`
2. Process questionnaires using functions in `CuspDownloadFunctions.R`
3. Process task data using functions in `CuspTaskFunctions.R`
4. Match and anonymize data using functions in `CuspMatchingFunctions.R` 