#!/bin/bash
python -m pip install --upgrade google-api-python-client
sed -i "s/PYTHONEXEC <- \".*\"/PYTHONEXEC <- \"$(echo `which python` | sed 's/\//\\\//g')\"/g" app.R
Rscript ./scripts/requiredPackages.R
Rscript ./app.R &
