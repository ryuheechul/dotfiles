# expected to be sourced from ~/.config/nushell/config.nu
$env.config.edit_mode = 'vi'

# commands
def is-linux [] { uname | get kernel-name | $in == Linux }
def git-status [] { pwd | print $in; zsh -c 'ls'; git status }

# aliases
alias j = zi # zoxide to fit my muscle memory with fasd+fzf
alias vi = nvim
alias l = ls -la
alias tree = ls **/*
alias gs = git-status
alias gl = git pull
alias ggpush = git push
alias gd = git diff
alias gdc = gd --cached
alias gap = git add -p
