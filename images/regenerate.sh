#!/usr/bin/env bash
# Rebuild the README's method-output images: run the five method
# scripts on the bundled synthetic data (import-data.R's default), then convert each
# section's figure to PNG (GitHub embeds only committed images; output/
# is gitignored). Run from the repo root:
#   bash images/regenerate.sh
set -euo pipefail

rm -f output/local_types.rds output/synthetic_local_types.rds   # fresh run of record for this build
for s in methods/01-measure-consolidation/measure-consolidation.R \
         methods/02-association-rule-mining/mine-attribute-bundles.R \
         methods/03-latent-class-analysis/recover-local-types.R \
         methods/04-regression/predict-recognition.R \
         methods/05-status-hierarchy/build-status-hierarchy.R; do
  Rscript "$s" > /dev/null
done

to_png() {  # pdftoppm renders the vectors at target size (sharp);
            # sips rasterizes low-res first and upscales (blurry) --
            # fallback only
  if command -v pdftoppm > /dev/null; then
    pdftoppm -png -scale-to-x 1400 -scale-to-y -1 -singlefile \
      "$1" "${2%.png}"
  else
    sips -s format png --resampleWidth 1400 "$1" --out "$2" > /dev/null
  fi
}
for f in initial_assessment rule_network class_count_sweep \
         type_by_race_mosaic status_hierarchy_respected; do
  to_png "output/synthetic_$f.pdf" "images/synthetic_$f.png"
  echo "images/synthetic_$f.png"
done
