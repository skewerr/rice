autoload -Uz promptinit
autoload -Uz compinit
autoload -U zmv

# {{{ prompt
PROMPT='%n@%M %~
%(!.#.») '

promptinit
# }}}
# {{{ completion
zstyle ':completion:*' completer _complete
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'
compinit
# }}}

HISTFILE="/home/spoonm/.cache/zsh/history"
HISTSIZE=5000
SAVEHIST=5000

stty start '^-' stop '^-'
setopt interactivecomments

# {{{ keybinds
bindkey -e

bindkey '\e[A' history-beginning-search-backward
bindkey '\e[B' history-beginning-search-forward

bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
bindkey '\e[5~' beginning-of-history
bindkey '\e[3~' delete-char
bindkey '\e[2~' quoted-insert
bindkey '^[^[[D' backward-word
bindkey '^[^[[C' forward-word
# }}}
# {{{ aliases
alias git=hub
alias mpv='DRI_PRIME=1 mpv'
alias wttr='curl wttr.in/São+Paulo'
alias ls='ls --color=auto'
alias fetch='neofetch'

# XDG related
alias weechat='weechat -d ~/.config/weechat'

# pacaur aliases
alias prcnsu='pacaur -Rcnsu'
alias prdd='pacaur -Rdd'
alias psyu='pacaur -Syu'
alias pqdtq='pacaur -Qdtq'
alias pql='pacaur -Ql'
alias pqs='pacaur -Qs'
alias pqo='pacaur -Qo'
alias pqi='pacaur -Qi'
alias pd='pacaur -d'
# }}}
# {{{ functions
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
# }}}
# {{{ xdg
export XDG_DESKTOP_DIR="$HOME/"
export XDG_DOCUMENTS_DIR="$HOME/docs/"
export XDG_DOWNLOAD_DIR="$HOME/dl/"
export XDG_MUSIC_DIR="$HOME/mus/"
export XDG_PICTURES_DIR="$HOME/pic/"
export XDG_VIDEOS_DIR="$HOME/vid/"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_RUNTIME_DIR="/run/user/$UID"
export XDG_DATA_HOME="$HOME/.local/share"
# }}}
# {{{ exports
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++

export MAKEFLAGS=-j5
export CFLAGS=" -O3 -march=native "
export CXXFLAGS=" -O3 -march=native "

# less related
export LESSHISTFILE=-
export LESS="-R -S -# 4"

# gpg tty
export GPG_TTY=$(tty)

# go path
export GOPATH=~/.go

# nvim is my EDITOR
export EDITOR=nvim

# fixes blank windows in java programs
export _JAVA_AWT_WM_NONREPARENTING=1

# java rendering a shit
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=lcd -Dswing.aatext=true'

# gets rid of unnecessary accessibility thingamajigs
export NO_AT_BRIDGE=1

#export GTK_IM_MODULE=ibus
#export QT_IM_MODULE=ibus
#export XMODIFIERS=@im=ibus

#export GTK_IM_MODULE=uim
#export QT_IM_MODULE=uim
#export XMODIFIERS=@im=uim

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

#export GTK_IM_MODULE=scim
#export QT_IM_MODULE=scim
#export XMODIFIERS=@im=SCIM

# XDG related
export WEECHAT_HOME="/home/spoonm/.config/weechat"
# }}}

path+=(
  "/home/spoonm/.local/bin"
  "/home/spoonm/.cabal/bin"
  "/home/spoonm/.go/bin"
)

export PATH

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
source /usr/lib/ruby/gems/2.5.0/gems/tmuxinator-0.11.1/completion/tmuxinator.zsh
