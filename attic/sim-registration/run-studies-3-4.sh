#!/usr/bin/env bash
# run-studies-3-4.sh — Production runs for Studies 3 (outlier) and 4 (real-data)
#
# Usage:
#   bash attic/sim-registration/run-studies-3-4.sh [CORES]
#
# Runs both studies sequentially. Each saves incremental per-DGP batch files,
# then combines into results_combined_outlier.rds / results_combined_realdata.rds.
#
# Resume-safe: checks for existing combined result files and skips completed studies.
# Partial batch files from pilots (10 reps) will be overwritten by full runs.
#
# Expected run counts:
#   Study 3: 87 cells × 50 reps  =  4,350 tasks
#   Study 4: 80 cells × 100 reps =  8,000 tasks
#   Total:                          12,350 tasks
#
# Estimated wall time (serial): ~12h total
# With 6 cores: ~2-3h total

set -euo pipefail

CORES="${1:-6}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RESULTS_DIR="$SCRIPT_DIR/results"
LOG_DIR="$RESULTS_DIR/logs"
mkdir -p "$LOG_DIR"

TIMESTAMP=$(date +%Y%m%d_%H%M%S)

echo "============================================="
echo "  Studies 3 & 4 — Production Runs"
echo "============================================="
echo "Cores:      $CORES"
echo "Results:    $RESULTS_DIR"
echo "Log dir:    $LOG_DIR"
echo "Started:    $(date)"
echo "============================================="
echo ""

# --- Study 3: Outlier Contamination -------------------------------------------

STUDY3_COMBINED="$RESULTS_DIR/results_combined_outlier.rds"

if [ -f "$STUDY3_COMBINED" ]; then
    STUDY3_ROWS=$(Rscript -e "cat(nrow(readRDS('$STUDY3_COMBINED')))")
    if [ "$STUDY3_ROWS" -ge 4300 ]; then
        echo "[Study 3] Already complete ($STUDY3_ROWS rows). Skipping."
        echo ""
    else
        echo "[Study 3] Combined file exists but only $STUDY3_ROWS rows. Re-running."
        rm -f "$STUDY3_COMBINED"
    fi
fi

if [ ! -f "$STUDY3_COMBINED" ]; then
    echo "[Study 3] Starting outlier contamination study (4,350 tasks)..."
    STUDY3_LOG="$LOG_DIR/study3_${TIMESTAMP}.log"

    Rscript "$SCRIPT_DIR/sim-run.R" outlier "$CORES" 2>&1 | tee "$STUDY3_LOG"

    if [ -f "$STUDY3_COMBINED" ]; then
        STUDY3_ROWS=$(Rscript -e "cat(nrow(readRDS('$STUDY3_COMBINED')))")
        echo ""
        echo "[Study 3] Complete: $STUDY3_ROWS rows saved."
    else
        echo "[Study 3] ERROR: Combined file not created!"
        exit 1
    fi
    echo ""
fi

# --- Study 4: Real-Data Templates ---------------------------------------------

STUDY4_COMBINED="$RESULTS_DIR/results_combined_realdata.rds"

if [ -f "$STUDY4_COMBINED" ]; then
    STUDY4_ROWS=$(Rscript -e "cat(nrow(readRDS('$STUDY4_COMBINED')))")
    if [ "$STUDY4_ROWS" -ge 7900 ]; then
        echo "[Study 4] Already complete ($STUDY4_ROWS rows). Skipping."
        echo ""
    else
        echo "[Study 4] Combined file exists but only $STUDY4_ROWS rows. Re-running."
        rm -f "$STUDY4_COMBINED"
    fi
fi

if [ ! -f "$STUDY4_COMBINED" ]; then
    echo "[Study 4] Starting real-data template study (8,000 tasks)..."
    STUDY4_LOG="$LOG_DIR/study4_${TIMESTAMP}.log"

    Rscript "$SCRIPT_DIR/sim-run.R" realdata "$CORES" 2>&1 | tee "$STUDY4_LOG"

    if [ -f "$STUDY4_COMBINED" ]; then
        STUDY4_ROWS=$(Rscript -e "cat(nrow(readRDS('$STUDY4_COMBINED')))")
        echo ""
        echo "[Study 4] Complete: $STUDY4_ROWS rows saved."
    else
        echo "[Study 4] ERROR: Combined file not created!"
        exit 1
    fi
    echo ""
fi

# --- Summary -------------------------------------------------------------------

echo "============================================="
echo "  All Production Runs Complete"
echo "============================================="
echo "Finished:   $(date)"
echo ""
echo "Result files:"
for f in "$STUDY3_COMBINED" "$STUDY4_COMBINED"; do
    if [ -f "$f" ]; then
        ROWS=$(Rscript -e "cat(nrow(readRDS('$f')))")
        SIZE=$(du -h "$f" | cut -f1)
        echo "  $(basename $f): $ROWS rows ($SIZE)"
    fi
done
echo ""
echo "Logs in: $LOG_DIR"
echo ""

# Quick sanity check
Rscript -e '
cat("\n=== Quick Sanity Check ===\n\n")
for (study in c("outlier", "realdata")) {
  f <- file.path("'"$RESULTS_DIR"'", paste0("results_combined_", study, ".rds"))
  if (!file.exists(f)) next
  r <- readRDS(f)
  cat(sprintf("Study: %s\n", study))
  cat(sprintf("  Rows: %d | DGPs: %s\n", nrow(r), paste(sort(unique(r$dgp)), collapse=", ")))
  cat(sprintf("  Methods: %s\n", paste(sort(unique(r$method)), collapse=", ")))
  cat(sprintf("  Failure rate: %.1f%%\n", 100 * mean(r$failure)))
  cat(sprintf("  Warp MISE range: [%.4f, %.4f]\n", min(r$warp_mise, na.rm=TRUE), max(r$warp_mise, na.rm=TRUE)))
  cat("\n")
}
'
