#!/bin/sh
#
#  adoc-fake.sh 	-- A simple command called when asciidoctor is not installed
#
# Copyright © 2022-2023 Erick Gallesio <eg@stklos.net>
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 27-Jan-2022 10:51

cat >&2 <<EOF
*** NOTE: Documentation was not rebuilt (install the asciidoctor
*** package if you modified it or need to rebuild it)
EOF

# Anyway, we still have a (not updated) documentation in the STklos tree, so it
# it not really a problem.
exit 0
