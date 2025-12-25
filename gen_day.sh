#!/bin/bash

# Check if a day number argument is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <day_number>"
  echo "Example: $0 1"
  exit 1
fi

DAY_NUM="$1"
SRC_DIR="src"
DUNE_FILE="$SRC_DIR/dune"
ML_FILE="$SRC_DIR/day${DAY_NUM}.ml"

# Check if src directory exists
if [ ! -d "$SRC_DIR" ]; then
  echo "Error: Directory '$SRC_DIR' not found."
  exit 1
fi

# 1. Append the executable stanza to src/dune
cat <<EOF >> "$DUNE_FILE"

(executable
 (name day${DAY_NUM})
 (modules day${DAY_NUM})
 (public_name day${DAY_NUM})
 (libraries aoc))
EOF

echo "Appended configuration to $DUNE_FILE"

# 2. Create the new .ml file with the template code
cat <<EOF > "$ML_FILE"
let lines = Aoc.Parser.read_day ${DAY_NUM}

let part1 lines =

let part2 lines =

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" (part1 lines) (part2 lines)
EOF

echo "Created $ML_FILE"

touch "inputs/day${DAY_NUM}.in"
echo "Created inputs/day${DAY_NUM}.in"
