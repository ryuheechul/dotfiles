#!/usr/bin/env bash

# `+k`: it means press `k` (scroll one line up) to to make sure the content shows up at the top
less +k --RAW-CONTROL-CHARS -I <&0
