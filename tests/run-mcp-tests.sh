#!/bin/sh
set -eu

# Hard-coded list of test names (basenames without "-mcp-server-tests.el").
# Update this space-separated list when adding/removing tests.
TESTS="project"

# Resolve repository root (parent of this script's directory)
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

for x in $TESTS; do
  test_file="${x}-mcp-server-tests.el"
  impl_file="mcp-servers/${x}-mcp-server.el"

  echo "=== Running ${test_file} ==="

  # Ensure required files exist relative to repo root
  if [ ! -f "${REPO_ROOT}/mcp-server.el" ]; then
    echo "Error: ${REPO_ROOT}/mcp-server.el not found." >&2
    exit 1
  fi
  if [ ! -f "${REPO_ROOT}/${impl_file}" ]; then
    echo "Error: ${REPO_ROOT}/${impl_file} not found." >&2
    exit 1
  fi
  if [ ! -f "${REPO_ROOT}/tests/${test_file}" ]; then
    echo "Error: test file ${REPO_ROOT}/tests/${test_file} not found." >&2
    exit 1
  fi

  # Run Emacs from the repository root so default-directory and load-paths
  # inside Emacs reflect the project root.
  (cd "${REPO_ROOT}" && \
    emacs -Q --batch \
      -L "." \
      -l "mcp-server.el" \
      -l "${impl_file}" \
      -l "tests/${test_file}" \
      -f ert-run-tests-batch-and-exit) || exit 1

done
