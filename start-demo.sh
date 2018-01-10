#!/bin/bash
python -m pip install --upgrade google-api-python-client
Rscript ./scripts/requiredPackages.R
Rscript ./app.R &
