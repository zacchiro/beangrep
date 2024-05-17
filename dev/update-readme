#!/bin/bash

# SPDX-FileCopyrightText: 2024 Stefano Zacchiroli <zack@upsilon.cc>
# SPDX-License-Identifier: GPL-2.0-or-later

set -e

readme=README.md
usage_begin_line=$(grep --line-number "Usage: bean-grep" "$readme" | tail -n 1 | cut -f 1 -d:)
usage_end_line=$(grep --line-number "an error occurred" "$readme" | tail -n 1 | cut -f 1 -d:)

new_readme="${readme}.new"
head -n $(( "$usage_begin_line" - 1 )) "$readme" > "$new_readme"
bin/bean-grep --help >> "$new_readme"
tail -n +$(( "$usage_end_line" + 1 )) "$readme" >> $new_readme

mv "$new_readme" "$readme"
