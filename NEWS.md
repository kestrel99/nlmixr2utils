# nlmixr2utils 0.2

* Promoted the shared raw-results and run-cache helper APIs to stable status now that downstream bootstrap and SIR packages use the common output, restart, and seeding infrastructure.

# nlmixr2utils 0.1

* Added experimental shared raw-results helpers (`rawResultsSchema()`, `rawResultsRow()`, `writeRawResults()`, `readRawResults()`, `parseRawResultsParams()`, and `setupRawResultsFilter()`) plus run-cache helpers (`resolveRunDir()`, `readRunState()`, `writeRunState()`, `taskCache()`, `pendingTasks()`, `withRunSeed()`, and `deriveFitName()`) for downstream nlmixr2 extension packages.

* Initial package largely cloned from `nlmixr2extra`, providing shared worker-plan
  helpers, covariance utilities, equation-printing methods, reexports, and the
  `theoFitOde` package data used by the new extension packages.
