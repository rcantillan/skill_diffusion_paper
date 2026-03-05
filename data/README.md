# Data layout

This repo is **code-first** and keeps large data out of git.

## Expected large derived input (local only)

Most estimation scripts assume the triadic dataset is available as:

- `./dt_con_cs_nestedness.rds`

Do **not** commit this file to GitHub.

## Recommended archival workflow (for Science Advances)

1) Deposit the derived dataset in an open archive (Zenodo/Dataverse/OSF).
2) Record:
   - DOI
   - file name
   - SHA256 checksum
   in `data/metadata/`.
3) Add a small download helper (optional) that fetches the archive and places
   the file in the repo root.

## Raw public sources

- O*NET releases (2015–2024)
- BLS OEWS wages (May 2015)

Document exact versions and URLs in `docs/DATA_PROVENANCE.md`.
