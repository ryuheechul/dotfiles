# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
PATH=~/.rbenv/shims:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/Users/heechul/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zshmarks zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

if [ $commands[kubectl] ]; then
  source <(kubectl completion zsh)
fi

source ~/icd/dotfiles/tmuxinator/completion/tmuxinator.zsh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

export TERM="xterm-256color"
export EDITOR=vim

bindkey -v
export KEYTIMEOUT=1

alias gs="git status"
alias gd="git diff"
alias gdc="gd --cached"

alias vim="nvim"
alias vi="vim"

alias g="jump"
alias s="bookmark"
alias d="deletemark"
alias p="showmarks"
alias rg="ranger"

alias ccat="ccat -G Keyword=\"darkgreen\" -G Plaintext=\"ellow\" -G Tag=\"fuchsia\" -G HTMLAttrName=\"darkteal\" -G Decimal=\"yellow\" -G Punctuation=\"lightgray\" --color=always"
alias dc="docker-compose"

alias lein="docker run --rm -it clojure lein"
alias boot="docker run --rm -it clojure:boot boot"

alias gmacs="open -a /Applications/Emacs.app"
alias cmacs="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n ."
alias muxl="mux local"
alias muxk="tmux kill-session -t"
alias muxle="vi .tmuxinator.yml"
alias muxs="mux start"
alias muxe="mux open"
