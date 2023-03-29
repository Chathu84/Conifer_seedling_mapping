#!/usr/bin/env bash

# Run this script with the name of the new project as 
# an argument, like so: `bash make_project.sh my_project`
# It will generate a project with the following structure:

#.
#|-- README.md
#|-- data
#|   |-- cleaned
#|   `-- raw
#|-- images
#|-- reports
#`-- scripts

mkdir "$1"

cd "$1" || exit

echo "# $1" >> README.md

mkdir data

mkdir data/raw

mkdir data/cleaned

mkdir scripts

mkdir images

mkdir reports
