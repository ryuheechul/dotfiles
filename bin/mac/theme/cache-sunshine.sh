#!/usr/bin/env bash

# assuming https://github.com/crescentrose/sunshine installed

cached_result="$(cat ~/.cache/sunshine-result)"

sunshine . --simple | { grep . || echo "$cached_result"; } > ~/.cache/sunshine-result
