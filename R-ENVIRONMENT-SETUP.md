# R Environment Setup for Claude Code

This document explains how to set up R automatically for Claude Code sessions.

## Automatic R Setup

The `setup-r-environment.sh` script installs R and all required dependencies for the tf package.

### Making it Automatic for Future Sessions

To make Claude Code automatically run this setup script in new sessions, you have a few options:

#### Option 1: User-level Hook (Recommended)

Create a hook in your Claude Code settings that runs at startup:

```bash
# Create the hooks directory if it doesn't exist
mkdir -p ~/.claude/hooks

# Create a startup hook
cat > ~/.claude/hooks/startup.sh << 'EOF'
#!/bin/bash
# Startup hook for Claude Code sessions

# Check if we're in an R project
if [ -f "DESCRIPTION" ] && grep -q "^Package:" DESCRIPTION 2>/dev/null; then
    echo "Detected R package project"

    # Check if R is installed
    if ! command -v R &> /dev/null; then
        echo "R not found, running setup..."
        if [ -f "setup-r-environment.sh" ]; then
            bash setup-r-environment.sh
        fi
    else
        echo "R is already available: $(R --version | head -1)"
    fi
fi
EOF

# Make it executable
chmod +x ~/.claude/hooks/startup.sh
```

#### Option 2: Environment Variable

Set an environment variable that Claude Code reads:

```bash
export CLAUDE_R_AUTO_SETUP=true
```

Add this to your `~/.bashrc` or `~/.zshrc` to make it permanent.

#### Option 3: Repository-specific Script

If you're using a specific repository repeatedly, you can tell Claude Code to run the setup script by mentioning it in the CLAUDE.md file (which is already done in this repo).

## Manual Setup

If you need to run the setup manually in a new session:

```bash
bash setup-r-environment.sh
```

## What Gets Installed

The setup script installs:

**System packages:**
- r-base, r-base-dev (R 4.3.3)
- Build tools (libcurl, libssl, etc.)

**R packages (via apt):**
- devtools (2.4.5) - Development tools
- testthat (3.2.1) - Testing framework
- roxygen2 - Documentation generator
- checkmate, cli, mgcv, mvtnorm, pracma - tf dependencies
- purrr, rlang, vctrs, zoo - More tf dependencies
- dplyr - Data manipulation (in Suggests)
- tidyverse - Full tidyverse (optional)

## Disk Space

The full installation requires approximately **500 MB** of disk space.

## Testing After Setup

Once R is installed, you can run:

```bash
# Load the package
Rscript -e "devtools::load_all()"

# Run tests
Rscript -e "devtools::test()"

# Full package check
Rscript -e "devtools::check()"
```

## Troubleshooting

### R not found after setup

```bash
# Verify R installation
which R
R --version

# If not found, run setup again
bash .claude/setup-r.sh
```

### Package installation failures

If individual R packages fail to install, you can install them manually:

```bash
Rscript -e "install.packages('package_name', repos='https://cloud.r-project.org')"
```

### Permission issues

The setup script requires root access. If you see permission errors, make sure you're running in an environment where you have sudo/root privileges.
