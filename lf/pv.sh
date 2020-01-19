#!/usr/bin/env bash

unset COLORTERM
#bat --color=always "$@"
bat --color=always --theme=base16 "$@"

