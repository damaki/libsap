#!/bin/bash

LEVEL=1
EXIT_CODE=0

usage() {
  echo "Usage: $0 --level=[1-4]"
  exit 1
}

# Parse arguments
for i in "$@"; do
  case $i in
    --level=*)
      LEVEL="${i#*=}"

      # Validate: Check if the value is strictly an integer
      if [[ ! "$LEVEL" =~ ^[1-4]$ ]]; then
        echo "Error: --level must be a number in the range 1 to 4."
        usage
      fi
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
        echo "Cleaning example: $(basename $d)"
        alr exec -- gnatprove -P $(basename $d).gpr --clean
        echo "Proving example: $(basename $d)"
        alr exec -- gnatprove \
          -P "$(basename "$d").gpr" \
          --level="$LEVEL" \
          -j0 \
          --warnings=error \
          --checks-as-errors=on
    ) || EXIT_CODE=1
done

exit $EXIT_CODE
