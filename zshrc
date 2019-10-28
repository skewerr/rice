autoload -Uz promptinit
autoload -Uz compinit
autoload -Uz vcs_info
autoload -U zmv

precmd() {
  vcs_info
}

+vi-git-untracked() {
  if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) == 'true' ]] && \
      git status --porcelain | grep -q -m 1 '??'; then
    hook_com[misc]=' %F{9}!%f'
  fi
}

zstyle ':vcs_info:git*+set-message:*' hooks git-untracked
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr ' %F{10}+%f'
zstyle ':vcs_info:git:*' unstagedstr ' %F{9}•%f'
zstyle ':vcs_info:git:*' formats '→ %F{14}%b%f%m%u%c'
zstyle ':vcs_info:git:*' actionformats '→ %F{14}%b %F{13}%a%f'

# {{{ prompt
setopt prompt_subst

PROMPT='%F{11}%n %F{12}%M %F{10}%~%f ${vcs_info_msg_0_}
%(!.#.») '

PS2='  » '

promptinit
# }}}
# {{{ completion
zstyle ':completion:*' completer _complete
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
compinit
# }}}

HISTFILE=~/.zsh_history
HISTSIZE=1500
SAVEHIST=3000

stty start '^-' stop '^-'
setopt interactivecomments appendhistory autocd extendedglob nomatch
unsetopt beep

# {{{ keybinds
bindkey -e

bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
bindkey '\e[5~' beginning-of-history
bindkey '\e[3~' delete-char
bindkey '\e[2~' quoted-insert
bindkey '\eu' backward-kill-line
bindkey '\ew' backward-kill-word
bindkey '^[^[[D' backward-word
bindkey '^[^[[C' forward-word
# }}}
# {{{ aliases
alias git=hub
alias ls='ls --color=auto'
alias g='BROWSER=links googler --noua'
alias nc='nvim ~/.config/nvim/init.vim'
# }}}
# {{{ functions
share() {
  scp "$1" chiba:/var/www/share/"$2"
}

sprunge() {
  [[ -n "$1" ]] && local body="$(< $1)"  || local body="$(cat)"
  if [[ $(wc -c <<< "$body" | sed 's/^ *//') -gt 0 ]]; then
    curl -\# -F 'sprunge=<-' sprunge.us <<< "$body" | \
      tr -d '\n' | xclip -selection c
  fi
}

ghc() {
  if [[ -e ".ghc" ]]; then
    ./.ghc "$@"
  else
    command ghc "$@"
  fi
}

mosh() {
  case "$1" in
    (akita|chiba|yamagata) )
      command mosh "$1" -- tmuxinator start master
      ;;
    odroid)
      command mosh root@"$1" -- tmuxinator start master
      ;;
    *)
      echo "Unrecognized target. Available: yamagata akita odroid chiba"
  esac
}

mpv() {
  currentDesktop="$(wmdesk)"
  if [ "$currentDesktop" = "anime" ]; then
    command mpv --fs "$@"
  else
    command mpv "$@"
  fi
}
# }}}
# {{{ xdg
export XDG_DESKTOP_DIR="$HOME"
export XDG_DOCUMENTS_DIR="$HOME/documents"
export XDG_DOWNLOAD_DIR="$HOME/downloads"
export XDG_MUSIC_DIR="$HOME/music"
export XDG_PICTURES_DIR="$HOME/pictures"
export XDG_VIDEOS_DIR="$HOME/videos"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_RUNTIME_DIR="/run/user/$UID"
export XDG_DATA_HOME="$HOME/.local/share"
# }}}
# {{{ exports
export CC=/usr/bin/gcc
export CXX=/usr/bin/g++

export MAKEFLAGS=-j5
export CFLAGS=" -O3 -march=native "
export CXXFLAGS=" -O3 -march=native "

# less related
export LESSHISTFILE=-
export LESS="-R -S -# 4"
export LESSOPEN="|lesspipe.sh %s"

# gpg tty
export GPG_TTY=$(tty)

# go path
export GOPATH=~/.go

# nvim is my EDITOR
export EDITOR=nvim

# firefox is my BROWSER
export BROWSER=firefox

# fixes blank windows in java programs
export _JAVA_AWT_WM_NONREPARENTING=1

# java rendering a shit
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd -Dswing.aatext=true'

# gets rid of unnecessary accessibility thingamajigs
export NO_AT_BRIDGE=1

# mosh escape character
export MOSH_ESCAPE_KEY='~'
# }}}

path+=(
  "/home/spoonm/.local/bin"
  "/home/spoonm/.cabal/bin"
  "/home/spoonm/.go/bin"
)

export PATH

source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh

source /usr/lib/ruby/gems/2.6.0/gems/tmuxinator-1.1.1/completion/tmuxinator.zsh

source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# changes zsh-autosuggestions highlight
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'

# vim: set et ts=2 sw=2 :
