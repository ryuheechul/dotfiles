#!/usr/bin/env bash

# this is to be used regardless of the bootstrapping happened or not

curr_d="$(dirname "$0")"
hm_wrapper="${curr_d}/../../bin/path/default/home-manager"
"${hm_wrapper}" $@
