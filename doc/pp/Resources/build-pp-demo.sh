#!/bin/sh
#
#  build-full.sh -- Build the stklos-pp demo example
#
# Copyright © 2023 Erick Gallesio <eg@stklos.net>
#
#           Author: Erick Gallesio [eg@stklos.net]
#    Creation date: 22-Jul-2023 18:13
# Last file update: 22-Jul-2023 18:40 (eg)

../../../src/stklos -q -I ../../../lib -f ../../../utils/stklos-pp.stk \
                    -- -o ../../HTML/pp-demo.html pp-demo.md
