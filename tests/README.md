Running tests

From the repository root, run a single test file in batch mode:

emacs -Q --batch -L . -l tests/project-mcp-server-tests.el -f ert-run-tests-batch-and-exit

To run all test files in tests/ at once (shell expands the list):

emacs -Q --batch -L . $(printf -- '-l %s ' tests/*.el) -f ert-run-tests-batch-and-exit

Notes:
- Run commands from the project root so -L . loads this repository.
- Tests derive the project root from the test file's location, so keeping tests under tests/ works.
- Requires Emacs with ERT available (emacs --version).
