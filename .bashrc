# Don't run when not an interactive shell
case $- in
  *i*) ;;
  *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=10000
HISTFILESIZE=20000

shopt -s checkwinsize

export LESS=-R
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
if [[ -x /usr/local/bin/lesspipe.sh ]]; then
  if [[ -x /Users/pdr/.local/bin/mylesspipe ]]; then
    export LESSOPEN="|mylesspipe %s"
  else
    export LESSOPEN="|lesspipe.sh %s"
  fi
  export LESSQUIET=1
fi

if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

if [ -r "$HOME/.bash_aliases" ]; then
  . "$HOME/.bash_aliases"
fi

case "$OSTYPE" in
  darwin*)
    export LSCOLORS=ExFxBxDxCxegedabagacad
    alias ls='ls -Gh'
    md5s() { md5 "$@"; }
    statm() { stat -f %m "$@"; }
    ;;
  linux*)
    if [ -x /usr/bin/dircolors ]; then
      test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
      alias ls='ls --color=auto'
      alias dir='dir --color=auto'
      alias vdir='vdir --color=auto'

      alias grep='grep --color=auto'
      alias fgrep='fgrep --color=auto'
      alias egrep='egrep --color=auto'
    fi
    md5s() { md5sum "$@"; }
    statm() { stat --printf %Y "$@"; }
    ;;
  *)
    statm() { return 0; }
esac

rgb=$(hostname -s | md5s | cut -c 1-6 | tr a-f A-F)
red=$(printf %02x 0x$(bc <<< "ibase=obase=16; $(cut -c 1-2 <<< "$rgb") / 4"))
green=$(printf %02x 0x$(bc <<< "ibase=obase=16; $(cut -c 3-4 <<< "$rgb") / 4"))
blue=$(printf %02x 0x$(bc <<< "ibase=obase=16; $(cut -c 5-6 <<< "$rgb") / 4"))
rgb="$red$green$blue"
unset red green blue

if [ "$UID" -eq 0 ]; then
  usercol=1
else
  usercol=6
fi
export PS1='${debian_chroot:+(\[\e[31m\]$debian_chroot\[\e[m\]) }\[\e[3'"$usercol"'m\]\u\[\e[m\]@\[\e[32m\]\h:\[\e[33m\]\w\[\e[m\]${bashrc_git_status:+[${bashrc_git_branch:+\[\e[34m\]$bashrc_git_branch}${bashrc_git_ahead:+\[\e[32m\]↑$bashrc_git_ahead}${bashrc_git_behind:+\[\e[31m\]↓$bashrc_git_behind}${bashrc_git_extrastatus:+\[\e[33m\]$bashrc_git_extrastatus}\[\e[m\]]}\$ '
unset usercol

bashrc_term_title() {
  # set terminal tab title
  printf "\e]1;$(id -un)@$(hostname -s)\a"
  # set terminal window title
  printf "\e]2;$(id -un)@$(hostname -s):${PWD/$HOME/~}\a"
}

case "$TERM_PROGRAM" in
  xterm-color|*-256color|iTerm*)
    export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
    bashrc_term_title_and_colours() {
      bashrc_term_title
      # set background colour
      printf "\e]Ph$rgb\e\\"
    }
    ;;
  Apple_Terminal)
    export CLICOLOR=1
    bashrc_term_title_and_colours() { bashrc_term_title; }
    unset rgb
    ;;
  *)
    export PS1="${debian_chroot:+($debian_chroot) }\u@\h:\w$(bashrc_git_status)\$ "
    bashrc_term_title_and_colours() { return 0; }
    ;;
esac

bashrc_check_repo() {
  local repo status status1
  unset bashrc_git_branch bashrc_git_ahead bashrc_git_behind

  if ! type -p git > /dev/null; then
    return
  fi

  # if we're in a repo with a readable .git dir (but not in $HOME) # && $HOME != "$(git rev-parse --show-toplevel)"
  if [[ -e "$(git rev-parse --git-dir 2>/dev/null)" ]]; then
    # get the repo dir
    repo=$(git rev-parse --show-toplevel)
    # do a fetch if we haven't done one for more than a minute
    if [[ ! ( -r "$repo"/.git/FETCH_HEAD ) || ( $(( $(date +%s) - $(statm "$repo"/.git/FETCH_HEAD) )) -gt 60 ) ]]; then
      (
        git fetch --quiet &> /dev/null & disown -a
      )
    fi
    status="$(git status --porcelain=1 -b)"
    status1="$(head -1 <<< "$status")"
    bashrc_git_branch="$(cut -c4- <<< "$status1" | cut -d\. -f1)"
    if [[ $bashrc_git_branch == master ]]; then
      bashrc_git_branch=
    fi
    bashrc_git_ahead="$(grep ahead <<< "$status1" | sed 's/.*ahead \([0-9]*\).*/\1/')"
    bashrc_git_behind="$(grep behind <<< "$status1" | sed 's/.*behind \([0-9]*\).*/\1/')"
    bashrc_git_extrastatus=$(grep -q '^[AM]' <<< "$status" && echo -n S; grep -q ^.M <<< "$status" && echo -n M) #; grep -q ^\?\? <<< "$status" && echo -n U)
  fi
  bashrc_git_status="$git_branch${bashrc_git_ahead:+↑$bashrc_git_ahead}${bashrc_git_behind:+↓$bashrc_git_behind}$bashrc_git_extrastatus"
}

bashrc_prompt() {
  bashrc_check_repo
  bashrc_term_title_and_colours
}

PROMPT_COMMAND=bashrc_prompt
bashrc_prompt

alias pg='ps -aef | grep'
alias pj='pg java'
alias np='netstat -plant'
alias ng='np | grep'
alias ll='ls -al'
alias lt='ls -altr'
alias ghci='ghci -v0 -ignore-dot-ghci -ghci-script ~/.ghci.standalone'

bashrc_path_add() {
  local dir
  while [[ $1 ]]; do
    dir="$1"
    if [[ -d $dir ]]; then
      PATH="$dir:$PATH"
    fi
    shift
  done
}
PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
bashrc_path_add "/usr/local/go/bin" "$HOME/Library/Haskell/bin"
bashrc_path_add "$HOME/.cabal/bin" "$HOME/.cabal/sbin"
bashrc_path_add "$HOME/.local/bin" "$HOME/.local/sbin"
bashrc_path_add "$HOME/local/bin" "$HOME/local/sbin"
bashrc_path_add "$HOME/.bin" "$HOME/.sbin"
bashrc_path_add "$HOME/bin" "$HOME/sbin"

export PATH

if [[ -r ~/.inputrc ]]; then
  export INPUTRC=~/.inputrc
fi

. "$HOME"/.local/bin/color-dark

if [[ -r .bashrc_local ]]; then
  . .bashrc_local
fi

if ! shopt -oq posix; then
  if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  elif [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  fi
  if type -p stack > /dev/null; then
    eval "$(stack --bash-completion-script stack)"
  fi
fi

[[ -r "$HOME/.nix-profile/etc/profile.d/nix.sh" ]] && . "$HOME/.nix-profile/etc/profile.d/nix.sh"
[[ -r "$HOME/.mylocale" ]] && . "$HOME/.mylocale"

# The following is for Go development
#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
# [[ -s "$HOME/.gvm/bin/gvm-init.sh" ]] && source "$HOME/.gvm/bin/gvm-init.sh"
