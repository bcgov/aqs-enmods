# EDT Benchmarking Dashboard

An R Shiny dashboard for monitoring the BC Ministry of Environment **Electronic Data Transfer (EDT) and Helpdesk** pipeline. It ingests importer benchmark logs and AQS job-count exports, computes submission and performance KPIs, and analyses the EnMoDS Help Desk mailbox (Outlook Inbox and Outbox `.pst`) to report request volumes and response times. The dashboard can export filtered data and generate a self-contained periodic HTML report.

---

## Features

- **Submission & performance KPIs** — unique users and organizations, total submissions, validated/submitted/rejected percentages, rejection follow-up breakdowns, total/median observation and sample counts, and time per 200 observations.
- **Interactive charts** — submission counts over time (filterable by status and agency), Time per 200 observations for SUBMITTED files, and AQS sync/speed tracking (raw time series plus background/import event summary statistics).
- **Help Desk mailbox analysis** — parses incoming and outgoing `.pst` files, links requests to responses by resolved contact, and computes requests addressed, oldest pending requests, and statistical response-time metrics (median/mean/p10/p90).
- **Exports** — download filtered submissions, performance, and joined CSVs, and generate a periodic HTML summary report.

---

## Requirements

- **R** (4.x recommended)
- **Java runtime (JDK/JRE)** — required by `rJava`/`freepst` for `.pst` parsing. The app requests a 4 GB heap (`-Xmx4g`).
- **RStudio** — the app uses `rstudioapi` to set the working directory to the script location.

### R packages

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "readr", "stringr", "lubridate",
  "ggplot2", "DT", "scales", "plotly", "tidyr", "purrr",
  "stringdist", "rJava", "freepst", "rstudioapi"
))
```

> **Java note:** `rJava` and `freepst` require a working Java installation. If `library(rJava)` fails, ensure Java is installed and run `R CMD javareconf` (or set `JAVA_HOME`) before retrying.

---

## Configuration

On startup the app reads a `.Renviron` file from the working directory and downloads two AQS processor-stats CSVs from configured URLs. Create a `.Renviron` file next to the script:

```
URL_Background=https://your-host/path/to/background-processor-stats.csv
URL_Import=https://your-host/path/to/import-processor-stats.csv
```

Both variables are required — the app stops with an error if either is unset or the download fails.

Other configurable constants near the top of the script:

- `data_start_date` — earliest submission date included in the analysis (currently same as EnMoDS Launch Date).
- `options(shiny.maxRequestSize = ...)` — upload size cap.
- `options(java.parameters = "-Xmx4g")` — Java heap size for `.pst` parsing.

---

## Running

Open the script in RStudio and click **Run App**, or:

```r
shiny::runApp("EDT_Dashboard_integrated.R")
```

The app sets its working directory to the script's location, so keep `.Renviron` in the same folder.

---

## Uploads

All inputs are uploaded through the sidebar at runtime; nothing is hardcoded to a local path.

### Benchmark / performance data (CSV, multiple files allowed)

| Upload | Required columns |
|---|---|
| Submission metadata | `submission_id`, `file_name`, `original_file_name`, `submission_date`, `submitter_user_id`, `submitter_agency_name`, `submission_status_code`, `sample_count`, `results_count`, `results_count_old` |
| Importer performance | `submission_id`, `submission_date`, `local_validation_time`, `obs_validation_time`, `local_import_time`, `obs_import_time`, `total_time` |
| AQS background metadata | `startTime`, `jobCount` |
| AQS import metadata | `startTime`, `jobCount` |

### Help Desk mailbox analysis

| Upload | Format | Notes |
|---|---|---|
| Help Desk Inbox PST | `.pst` | Incoming mail |
| Help Desk Sent PST | `.pst` | Outgoing mail |
| EDT users CSV | `.csv` | `Name`, `Email` columns — all users for whom the helpdesk volumes and response rates are tracked |
| Sender exclusion CSV | `.csv` | `Name`, `Email` columns — help desk staff / system addresses to exclude from resolved contacts |
| Name to email translator CSV | `.csv` | `sender_name`, `email` columns — verbatim sender-name → email overrides to raise the inbox match rate |

The Help Desk section only runs once **all five** of its inputs are uploaded and pass file-type validation.

---

## Outputs

- **Download submissions CSV** — filtered submission records.
- **Download performance CSV** — filtered performance records.
- **Download joined CSV** — submissions joined to performance.
- **Generate Periodic Report (HTML)** — a self-contained `Periodic_EnMoDS_Data_and_Issues_Summary_<date>.html`, combining the benchmark KPIs, rejection follow-up, and Help Desk metrics. Each section is computed defensively, so the report still generates if Help Desk inputs are absent.

Downloads reflect the active date range and zero-time filter.

---

## How it works

1. **Ingestion** — uploaded CSVs are validated against required-column lists, parsed with robust readers, type-coerced, and filtered from `data_start_date`.
2. **KPIs & charts** — reactive expressions compute the headline metrics and feed the `plotly`/`ggplot2` visualizations.
3. **Help Desk pipeline** (`hd_run_pipeline`) — stages each `.pst`, reads mailboxes via `freepst`, resolves contacts (using the users, exclusion, and translator files), links inbox requests to sent responses, and returns `linked` and `inbox_missing` sets used for the response-time metrics.
4. **Reporting** — `generate_report_html()` assembles the periodic report from the filtered data and Help Desk analysis.

---

## Notes & limitations

- `.pst` parsing depends on Java and can be memory-intensive on large mailboxes; adjust the heap size if needed.
- The app expects to run in RStudio (uses `rstudioapi` to locate itself); to run headless, replace the `setwd()` line with an explicit path.
- The two AQS processor-stats files are fetched from `URL_Background` / `URL_Import` at startup rather than uploaded.
