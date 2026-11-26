# Fleet coverage census (babashka)

Card 20261014203048-078fa409 (COV-TOOL-accuracy), epic 20261014202833-4b60159e.
Moved here from `~/.local/share/hive/briefs/`, where they were not in any repo.

- `census3.clj`: static census of malli contract coverage and hive-test facet coverage.
  It reads forms with edamame, not regexes.
  - `bb script/census/census3.clj <root> [repo ...]`: table output
  - `bb script/census/census3.clj --edn <root> repo`: per-ns detail as EDN
- `census-accuracy.clj`: error bars for each blind spot of census2 (regex) against census3 (reader).
  It finds `census3.clj` beside itself.
  - `bb script/census/census-accuracy.clj <root> repo ...`: one EDN map per repo

`<root>` is the directory that holds the repos, e.g. `/home/klein/PP/hive`.
