#!/bin/sh
#
#  adoc-fake.sh 	-- A simple command called when asciidoctor is not installed
#
# Copyright © 2022 Erick Gallesio <eg@unice.fr>
#
#           Author: Erick Gallesio [eg@unice.fr]
#    Creation date: 27-Jan-2022 10:51
# Last file update: 27-Jan-2022 10:59 (eg)

cat >&2 <<EOF
***
*** You need to install the asciidoctor package to rebuild the documentation.
***
EOF

# Anyway, we still have a (not updated) documentation in the STklos tree, so it
# it not really a problem.
exit 0
