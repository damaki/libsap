#!/bin/bash

PROFILE=validation
EXIT_CODE=0

usage() {
  echo "Usage: $0 --profile={release|development|validation}"
  exit 1
}

# Parse arguments
for i in "$@"; do
  case $i in
    --profile=release|--profile=development|--profile=validation)
      PROFILE="${i#*=}"
      ;;
    --profile=*)
      echo "Error: Invalid profile value."
      usage
      ;;
    *)
      usage
      ;;
  esac
done

for d in */; do
    (
        cd "$d" || exit
        echo "===================="
        echo "Building example: $(basename $d)"
        alr build --profiles=*=$PROFILE
    ) || EXIT_CODE=1
done

exit $EXIT_CODE
