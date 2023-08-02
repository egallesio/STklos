#!/bin/sh
#
#  build-full.sh -- Build the stklos-pp demo example
#
# Copyright © 2023 Erick Gallesio <eg@stklos.net>
#
#           Author: Erick Gallesio [eg@stklos.net]
#    Creation date: 22-Jul-2023 18:13

TOP=../../..

${TOP}/src/stklos -q -I ${TOP}/lib -f ${TOP}/utils/stklos-pp.stk \
                    -- -o ${TOP}/doc/HTML/pp-demo.html pp-demo.md
