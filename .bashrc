# Don't run when not an interactive shell
case $- in
  *i*) ;;
  *) return;;
esac

for bashrc_profile in /etc/profile.d/*.sh /usr/local/etc/profile.d/*.sh "$HOME"/.bashrc_local "$HOME"/.bashrc_local.d/*; do
  [[ -r $bashrc_profile ]] && . "$bashrc_profile"
done
unset bashrc_profile

bashrc_hostname() {
  local hostname

  if hostname="$(hostname -s 2> /dev/null)"; then
    printf "$hostname"
  elif hostname="$(hostname 2> /dev/null)"; then
    printf "$hostname"
  else
    printf "$HOSTNAME"
  fi
}

if command -v fzf >& /dev/null; then
  bashrc_fzfver="$(fzf --version)"; bashrc_fzfver="${bashrc_fzfver#*.}"; bashrc_fzfver="${bashrc_fzfver%%[. ]*}"
  if [[ $bashrc_fzfver -ge 48 ]]; then
    eval "$(fzf --bash)"
    bind \\C-r:reverse-search-history  # disable fzf ctrl-r for now (until I can fix its use with ctrl-o)
  fi
  unset bashrc_fzfver
fi

if ! shopt -oq posix && [[ -z $BASH_COMPLETION_VERSINFO ]]; then
  if [[ -r ~/.bash_completion ]]; then
    . ~/.bash_completion
  fi

  if [[ -r /etc/bash_completion ]]; then
    . /etc/bash_completion
  elif [[ -r /usr/local/etc/bash_completion ]]; then
    . /usr/local/etc/bash_completion
  elif [[ -r /usr/local/share/bash-completion/bash_completion ]]; then
    . /usr/local/share/bash-completion/bash_completion
  elif [[ -r /usr/share/bash-completion/bash_completion ]]; then
    . /usr/share/bash-completion/bash_completion
  fi

  if ! command -v _fzf_complete >& /dev/null; then
    if [[ -r /usr/share/doc/fzf/examples/completion.bash ]]; then
      . /usr/share/doc/fzf/examples/completion.bash
    else
      f=( /opt/homebrew/Cellar/fzf/*/shell/completion.bash )
      if [[ -r $f ]]; then
        . "$f"
      fi
      unset f
    fi
  fi

  if type -p stack > /dev/null; then
    eval "$(stack --bash-completion-script stack)"
  fi
fi

# Hide recommendation to switch to zsh in MacOS Catalina.
export BASH_SILENCE_DEPRECATION_WARNING=1
# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=10000
HISTFILESIZE=20000
HISTTIMEFORMAT="%F %T "
export EDITOR=vim

shopt -s checkwinsize

export LESS="-R -F"
if [[ -x ~/.lessfilter ]]; then
  export LESSOPEN="|~/.lessfilter %s"
  export LESSQUIET=1
fi

if [[ -z ${debian_chroot:-} ]] && [[ -r /etc/debian_chroot ]]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

if [[ -r "$HOME/.bash_aliases" ]]; then
  . "$HOME/.bash_aliases"
fi

if [[ -n $WSLENV ]]; then
  iswindows=1 # show (also) the Windows icon when under WSL
fi

case "$OSTYPE" in
  darwin*)
    ismac=1
    stty discard undef # Give me my ctrl-o back!
    export CLICOLOR=1
    export LSCOLORS=ExFxBxDxCxegedabagacad
    export GREP_COLOR='1;3;34'
    alias ls='ls -G'
    alias dfh='df -h /System/Volumes/Data'
    alias pps='ps -ef'
    if command -v netstat > /dev/null; then
      alias np='sudo netstat -plant'
    else
      alias np='sudo lsof -nP -iudp -itcp -stcp:LISTEN | grep -v -- "->"'
    fi
    alias ng='np | grep'
    md5s() { md5 -- "$@"; }
    statm() { stat -f %m -- "$@"; }
    export LOCATE_PATH=~/.local/var/locate/locatedb
    if [[ ! -r "$LOCATE_PATH" ]]; then
      mkdir -p "$(dirname "$LOCATE_PATH")"
      # This should be put into cron as well
      sudo -n find "$HOME" -path "$HOME"/Library -prune -or -path "$HOME"/.Trash -prune -or -path "$HOME"/.vim/undo -prune -or -path "$HOME"/.local/share -prune -or -name .git -type d -prune -or -print 2> /dev/null | /usr/libexec/locate.mklocatedb > "$LOCATE_PATH"
    fi
    if [[ ! -r ~/.gitconfig ]]; then ln -nfs ~/.gitconfig.mac ~/.gitconfig; fi
    ;;
  msys*)
    iswindows=1
    if [[ -x /usr/bin/dircolors ]]; then
      test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
      export GREP_COLORS='mt=1;3;34'
      alias ls='ls --color=auto --hyperlink=auto'
      alias dir='dir --color=auto --hyperlink=auto'
      alias vdir='vdir --color=auto --hyperlink=auto'
    fi
    alias dfh='df -lh -x tmpfs -x devtmpfs -x efivarfs -x rootfs -x overlay -x 9p'
    alias st='systemctl status'
    alias pps='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,stime,tty=TTY,time,cmd'
    alias ppsc='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,cgname:120,stime,tty=TTY,time,cmd'
    if command -v netstat > /dev/null; then
      alias np='sudo netstat -plant'
    else
      alias np='sudo lsof -nP -iudp -itcp -stcp:^CLOSED,^ESTABLISHED,^SYN_SENT,^CLOSE_WAIT,^FIN_WAIT1,^CLOSING,^LAST_ACK,^TIME_WAIT | grep -v -- "->"'
    fi
    alias ng='np | grep'
    if [[ -r ~/.xmonad/xmonad.hs ]]; then
      alias xme='"$EDITOR" ~/.xmonad/xmonad.hs'
    fi
    md5s() { md5sum -- "$@"; }
    statm() { stat --printf %Y -- "$@"; }
    export LOCATE_PATH=~/.local/var/locate/locatedb
    if [[ ! -r "$LOCATE_PATH" ]]; then
      mkdir -p "$(dirname "$LOCATE_PATH")"
      # This should be put into cron as well
      updatedb --localpaths="$HOME" --findoptions="-path .cache -prune -or -path .cabal -prune -or -path .ghcup -prune -or -path .vim/undo -prune -or -path .local/share -prune -or -name .git -type d -prune -or -path go/pkg -prune" --output="$HOME"/.local/var/locate/locatedb
    fi
    if [[ ! -r ~/.gitconfig ]]; then ln -nfs ~/.gitconfig.linux ~/.gitconfig; fi
    ;;
  linux*)
    islinux=1
    case "$(lsb_release -s -i)" in
      Debian|Raspbian)
        isdeb=1
        islinux=
        ;;
    esac
    case "$(uname -m)" in
      armv7l)
        isrpi=1
        islinux=
        isdeb=
        ;;
    esac
    if [[ -x /usr/bin/dircolors ]]; then
      test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
      export GREP_COLORS='mt=1;3;34'
      alias ls='ls --color=auto --hyperlink=auto'
      alias dir='dir --color=auto --hyperlink=auto'
      alias vdir='vdir --color=auto --hyperlink=auto'
    fi
    alias dfh='df -lh -x tmpfs -x devtmpfs -x efivarfs -x rootfs -x overlay -x 9p'
    alias st='systemctl status'
    alias pps='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,stime,tty=TTY,time,cmd'
    alias ppsc='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,cgname:120,stime,tty=TTY,time,cmd'
    if command -v netstat > /dev/null; then
      alias np='sudo netstat -plant'
    else
      alias np='sudo lsof -nP -iudp -itcp -stcp:^CLOSED,^ESTABLISHED,^SYN_SENT,^CLOSE_WAIT,^FIN_WAIT1,^CLOSING,^LAST_ACK,^TIME_WAIT | grep -v -- "->"'
    fi
    alias ng='np | grep'
    if [[ -r ~/.xmonad/xmonad.hs ]]; then
      alias xme='"$EDITOR" ~/.xmonad/xmonad.hs'
    fi
    md5s() { md5sum -- "$@"; }
    statm() { stat --printf %Y -- "$@"; }
    export LOCATE_PATH=~/.local/var/locate/locatedb
    if [[ ! -r "$LOCATE_PATH" ]]; then
      mkdir -p "$(dirname "$LOCATE_PATH")"
      # This should be put into cron as well
      updatedb --localpaths="$HOME" --findoptions="-path .cache -prune -or -path .cabal -prune -or -path .ghcup -prune -or -path .vim/undo -prune -or -path .local/share -prune -or -name .git -type d -prune -or -path go/pkg -prune" --output="$HOME"/.local/var/locate/locatedb
    fi
    if [[ ! -r ~/.gitconfig ]]; then ln -nfs ~/.gitconfig.linux ~/.gitconfig; fi
    ;;
  *)
    statm() { return 0; }
esac

if [[ -r "$HOME"/.local/bin/color-dark ]]; then
  . "$HOME"/.local/bin/color-dark
fi

if [[ $UID -eq 0 ]]; then
  usercol=1
else
  usercol=6
fi
export PS1="$platformlogo"'${iswindows:+\[\e[38;5;39m\]\[\e[m\] }${isrpi:+\[\e[38;5;125m\]\[\e[m\] }${islinux:+ }${isdeb:+\[\e[38;5;162m\]\[\e[m\] }${ismac:+ }'"$applogo"'${debian_chroot:+(\[\e[38;5;66m\]$debian_chroot\[\e[m\]) }\[\e[3'"$usercol"'m\]\u\[\e[m\]@\[\e[32m\]\h:\[\e[33m\]\w\[\e[m\]${bashrc_git_status:+\[\e[38;5;239m\]\[\e[48;5;239m\]${bashrc_git_branch:+\[\e[34m\]$bashrc_git_branch}${bashrc_git_ahead:+\[\e[32m\]↑$bashrc_git_ahead}${bashrc_git_behind:+\[\e[31m\]↓$bashrc_git_behind}${bashrc_git_tag:+\[\e[38;5;66m\]$bashrc_git_tag}${bashrc_git_extrastatus:+\[\e[33m\]$bashrc_git_extrastatus}\[\e[m\e[38;5;239m\]\[\e[m\]}\$ '
unset usercol

bashrc_term_title() {
  local title="$(id -un)@$(bashrc_hostname):${PWD/#$HOME/\~}"
  # set terminal tab title
  printf "\e]1;$title\a"
  # set terminal window title
  printf "\e]2;$title\a"
  if [[ $TERM =~ ^(tmux|screen)- ]]; then
    # set tmux window title (allow-rename must be on for this to work)
    printf "\ek$title\e\\"
  fi
}

if [[ $LC_TERMINAL ]]; then
  termprog="$LC_TERMINAL"
elif [[ $TERM_PROGRAM && $TERM_PROGRAM != tmux ]]; then
  termprog="$TERM_PROGRAM"
else
  termprog="$TERM"
fi

case "$termprog" in
  iTerm*|xterm-color|*-256color)
    export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
    bashrc_term_title_and_colours() {
      if [[ -z $VIM_TERMINAL ]]; then
        bashrc_term_title
        # set background colour
        printf "\e]Ph${bashrc_bgcolour}\e\\"
        printf "\e]11;#${bashrc_bgcolour}\e\\"
      fi
    }
    ;;
  xterm-kitty)
    alias icat="kitty +kitten icat"
    export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
#     export TERM=xterm-256color
    bashrc_term_title_and_colours() {
      if [[ -z $VIM_TERMINAL ]]; then
        bashrc_term_title
        # set background colour
        printf "\e]11;#${bashrc_bgcolour}\e\\"
      fi
    }
    ;;
  Apple_Terminal)
    bashrc_term_title_and_colours() {
      if [[ -z $VIM_TERMINAL ]]; then
        bashrc_term_title
        # set background colour
        printf "\e]11;#${bashrc_bgcolour}\e\\"
      fi
    }
    ;;
  linux)
    bashrc_term_title_and_colours() {
      if [[ -z $VIM_TERMINAL ]]; then
        # set background colour
        printf "\e]P0${bashrc_bgcolour}"
        # switch consoles and back to reset the colours for the whole screen
      fi
    }
    ;;
  *)
    bashrc_term_title_and_colours() { return 0; }
    ;;
esac
unset termprog

bashrc_check_repo() {
  local repo status status1
  unset bashrc_git_branch bashrc_git_ahead bashrc_git_behind

  bashrc_git_status=

  if ! type -p git > /dev/null; then
    return
  fi

  [[ $(git rev-parse --is-inside-git-dir) != true ]] || return

  # if we're in a repo with a readable .git dir (but not in $HOME) # && $HOME != "$(git rev-parse --show-toplevel)"
  if [[ -e $(git rev-parse --git-dir 2>/dev/null) && $(git rev-parse --is-bare-repository) != true ]]; then
    # get the repo dir
    repo=$(git rev-parse --show-toplevel)
    # do a fetch if we haven't done one for more than a minute
    if [[ ! ( -r "$repo"/.git/FETCH_HEAD ) || ( $(( $(date +%s) - $(statm "$repo"/.git/FETCH_HEAD) )) -gt 60 ) ]]; then
      (
        GIT_TERMINAL_PROMPT=0 git fetch -ap --quiet &> /dev/null & disown -a
      )
    fi
    bashrc_git_tag="$(git log --pretty=%d -1 2> /dev/null | tr , \\n | grep '^ tag: ' | head -1 | sed 's/^.* tag: \([^)]*\))\?$/\1/')"
    status="$(git status --porcelain=1 -b)"
    status1="$(head -1 <<< "$status")"
    bashrc_git_branch="$(cut -c4- <<< "$status1" | cut -d\. -f1)"
    if [[ $bashrc_git_branch =~ ^(master|main)$ ]]; then
      bashrc_git_branch=
    fi
    bashrc_git_ahead="$(grep ahead <<< "$status1" | sed 's/.*ahead \([0-9]*\).*/\1/')"
    bashrc_git_behind="$(grep behind <<< "$status1" | sed 's/.*behind \([0-9]*\).*/\1/')"
    if git stash list | grep . > /dev/null 2>&1; then
      bashrc_git_hasstash='#'
    else
      bashrc_git_hasstash=
    fi
    bashrc_git_extrastatus="$(grep -q '^[ADM]' <<< "$status" && printf S; grep -q ^.M <<< "$status" && printf M)$bashrc_git_hasstash" #; grep -q ^\?\? <<< "$status" && printf U)
    bashrc_git_status="$bashrc_git_branch${bashrc_git_ahead:+↑$bashrc_git_ahead}${bashrc_git_behind:+↓$bashrc_git_behind}${bashrc_git_tag:+ $bashrc_git_tag }$bashrc_git_extrastatus"
  fi
}

if [[ $(type -t pg) == alias ]]; then unalias pg; fi
pg() {
  pids=($(pgrep -f "$@"))
  if [[ -n $pids ]]; then
    ps -fp "${pids[@]}"
  fi
}

alias grep='grep --exclude-dir={.Trash,.cache,.git,.cabal,.ghcup,.idea,undo,.m2} --color=auto'
alias fgrep='grep -F --color=auto'
alias egrep='grep -E --color=auto'
alias rgrep='grep -r --color=auto'
alias glocate='locate -d :'
alias pj='pg java'
if command -v eza >& /dev/null; then
#   export EZA_COLORS='ur=38;5;100:gr=38;5;100:tr=38;5;100'
  export EZA_COLORS='ur=0:uw=0:ux=0:ue=0:gr=0:gw=0:gx=0:ge=0:tr=0:tw=0:tx=0:te=0:uu=0:gu=0:da=0'
  alias ls='eza -B --git --icons=auto'
  alias ll='ls -aal'
  alias lt='ll -snew'
elif command -v exa >& /dev/null; then
#   export EZA_COLORS='ur=38;5;100:gr=38;5;100:tr=38;5;100'
  export EXA_COLORS='ur=0:uw=0:ux=0:ue=0:gr=0:gw=0:gx=0:ge=0:tr=0:tw=0:tx=0:te=0:uu=0:gu=0:da=0'
  alias ls='exa -B --git --icons'
  alias ll='ls -aal'
  alias lt='ll -snew'
else
  alias ll='ls -al'
  alias lt='ll -tr'
fi
alias ghci='ghci -v0 -ignore-dot-ghci -ghci-script ~/.ghci.standalone'

# Git aliases and functions

alias ga='git add'
alias gd='git d'
alias gds='git ds'
alias gl='git la'
alias gca='git ca'
alias gpf='git pf'
alias gcapf='git capf'
alias gsh='git show'
alias gst='git s'
alias gsu='git su'
alias gf='git f'
alias gb='git b'
alias gco='git co'
alias gpu='git pull'

_gitline_to_hash() {
  sed 's/^.* \([a-f0-9]\{7\}\) .*$/\1/' <<< "$1"
}

ggl() {
  local h
  if h="$(gl --format='%C(auto)%h %Cgreen%aL %Cblue%as%Creset: %s%C(auto)%d' --color | \
          fzf --ansi --reverse --preview "git show --color \"\$(sed 's/^.* \\([a-f0-9]\\{7,9\\}\\) .*\$/\\1/' <<< {})\"")"; then
    h="$(_gitline_to_hash "$h")"
    printf "$h"
  else
    return $?
  fi
}

gai() {
  local cmd='git -c color.status=always status -s'
  $cmd | fzf -d '' --ansi --reverse --preview "git-dd {4..}" --bind="enter:execute-silent(git add -- {4..})+reload($cmd)+down,tab:execute(git add -p -- {4..})+reload($cmd)+down,ctrl-\\:execute-silent(git restore --staged -- {4..})+reload($cmd)+down,ctrl-l:refresh-preview+clear-screen"
  return 0
}

gci() {
  gai
  git c
}

mv. () {
  oldname="$(pwd)"
  newname="$1"
  if [[ -z $newname || -e $newname ]]; then
    if [[ -d $newname ]]; then
      newname="$newname/"
    else
      echo "Usage: mv. newname" >&2
      return 1
    fi
  fi
  cd ..
  if mv -i "$oldname" "$newname"; then
    cd "$newname"
    if [[ $newname == */ ]]; then
      cd "$oldname"
    fi
  fi
}

rm. () {
  flag="$1"
  dirname="$(pwd)"
  if [[ $flag != '-f' ]]; then
    if [[ $flag ]]; then
      echo "Usage: rm. [-f]" >&2
      return 1
    fi
    read -p "Really remove current directory [yN]? " ans
    if [[ $ans != 'y' ]]; then
      return
    fi
  fi
  cd ..
  rm -r "$dirname"
}

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
PATH=/usr/games:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
[[ -x /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"
bashrc_path_add /c/appl/scoop/shims
bashrc_path_add /usr/local/texlive/2017/bin/x86_64-darwin
bashrc_path_add /usr/local/go/bin "$HOME/Library/Haskell/bin"
bashrc_path_add "$HOME/.ghcup/bin"
# bashrc_path_add "$HOME/.nix-profile/bin" "$HOME/.nix-profile/sbin"
bashrc_path_add "$HOME/.cabal/bin" "$HOME/.cabal/sbin"
bashrc_path_add "$HOME/.local/bin" "$HOME/.local/sbin"
bashrc_path_add "$HOME/local/bin" "$HOME/local/sbin"
bashrc_path_add "$HOME/.bin" "$HOME/.sbin"
bashrc_path_add "$HOME/bin" "$HOME/sbin"

export PATH

if [[ -r ~/.inputrc ]]; then
  export INPUTRC=~/.inputrc
fi

spf() {
  while [[ $1 ]]; do
    host -t TXT "$1" | cut -d\" -f2 | grep --color=never '^v=spf'
    shift
  done
}

SCRATCHDIR="$HOME/src/scratch"
scr() {
  mkdir -p "$SCRATCHDIR"
  d="$(mktemp -q -d "$SCRATCHDIR"/tmpXXX)"
  cd "$d"
  git init
  if [[ $1 ]]; then
    if [[ -r ~/.templates/$1 ]]; then
      cp ~/.templates/"$1" t."$1"
    else
      touch t."$1"
    fi
    vi +\$ t."$1"
  fi
}

mkdir -p "${LOOKINGDIR:="$HOME/src/looking"}"
l() {
  local dir="${1#*/}"
  if [[ $1 ]]; then
    if [[ $1 == s-* ]]; then
      opts="${1/s}"
      shift
      echo "ls $opts $@" >&2
      ls "$opts" "$@"
    elif [[ -n $dir && -d "$LOOKINGDIR"/$dir ]]; then
      cd -- "$LOOKINGDIR"/"$dir"
    elif [[ $1 == ?*/?* ]]; then
      # TODO: should use ls-remote instead (like in the wk function below)?
      if [[ $(curl -Ls -w '%{response_code}' -o/dev/null https://gitlab.com/"$1") == 200 ]]; then
        cd -- "$LOOKINGDIR"
        git clone https://gitlab.com/"$1"
      elif [[ $(curl -Ls -w '%{response_code}' -o/dev/null https://github.com/"$1") == 200 ]]; then
        cd -- "$LOOKINGDIR"
        git clone https://github.com/"$1"
      else
        echo "l: $1 not found" >&2
        return 1
      fi
      cd -- "$dir"
    else
      echo "l: Error parsing arguments" >&2
      return 1
    fi
  else
    cd -- "$LOOKINGDIR"
  fi
}

_l() {
  local compreply=($(compgen -d -- "$LOOKINGDIR"/ | grep "^$LOOKINGDIR/$2"))
  COMPREPLY=(${compreply[@]#$LOOKINGDIR/})
}
complete -F _l l

mkdir -p "${WORKDIR:="$HOME/src/work"}"
wk() {
  local dir="$1"
  if [[ $dir ]]; then
    if [[ -d $WORKDIR/$dir ]]; then
      cd -- "$WORKDIR/$dir"
    elif [[ $dir == ?*/?* ]]; then
      if git ls-remote -qh git@"$WORKGITLABHOST":"$1" non-existant-repo-head-name\! >& /dev/null; then
        mkdir -p -- "$WORKDIR/${1%/*}"
        cd -- "$WORKDIR/${1%/*}"
        git clone git@"$WORKGITLABHOST":"$1"
      else
        echo "wk: $1 not found" >&2
        return 1
      fi
      cd -- "$WORKDIR/$dir"
    else
      echo "wk: Error parsing arguments" >&2
      return 1
    fi
  else
    cd -- "$WORKDIR"
  fi
}

_wk() {
  local i compreply=($(compgen -d -- "$WORKDIR/$2"))
  COMPREPLY=($(
    (
      for i in "${compreply[@]}"; do
        find "$i" -type d -name .git
      done | while read i; do
        i="${i#$WORKDIR/}"
        echo "${i%/.git}"
      done
      fetch_work_repos | grep "^$2" # | sed "s/^\(${2//\//\\\/}[^/]*\/\?\).*/\1/"
    ) | sort -u
  ))
}
complete -F _wk wk

n() {
  local f n
  if [[ $PWD == */adventofcode/* ]]; then
    diryear="$(($(basename "$PWD")))"
    for ((n=1; n<=25; n++)); do
      if [[ -r ${n}a.hs || -r ${n}a.c ]]; then
        if [[ -r ${n}b.hs || -r ${n}b.c ]]; then
          continue
        else
          # mkaoc for part 1 tries to create a parser for the input file and for part 2 simply copies over part 1
          vi +$ "$("$HOME"/src/games/coding/adventofcode/mkaoc "$n" b)"
          return
        fi
      else
        # Don't fetch anything automatically if we're not solving this year's puzzles
        if [[ $(date +%Y) -eq $diryear ]]; then
          export TZ=EST
          while [[ $(date +%d) -lt $n ]]; do
            sleep 1
          done
          # Don't hit the server immediately, you need time to read the problem anyway
          sleep 2
          if [[ -x $HOME/src/games/coding/adventofcode/get_input ]]; then
            "$HOME"/src/games/coding/adventofcode/get_input
          fi
        fi
        vi +$ "$("$HOME"/src/games/coding/adventofcode/mkaoc "$n" a)"
        return
      fi
    done
  fi
}

do_submit() {
  if [[ -r $HOME/src/games/coding/adventofcode/.cookie ]]; then
    . "$HOME"/src/games/coding/adventofcode/.cookie
  fi
  curl -sS -b "$MYCOOKIE" -d level="$3" -d answer="$(cat "$4")" "https://adventofcode.com/$1/day/$2/answer" \
    | awk '/<main>/ {t=1}; /<\/main>/ {t=0;print}; t==1 {print}' \
    | tr '\n' ' ' \
    | sed 's/\<[^>]*>/ /g; s/  */ /g; s/ \([].]\)/\1/g; s/^ *\(.*\) *$/\1\n/'
}

sub() {
  local f n
  if [[ $PWD == */adventofcode/* ]]; then
    local year="$(basename "$PWD")"
    for ((n=1; n<=25; n++)); do
      if [[ -r ${n}a.hs || -r ${n}a.c ]]; then
        if [[ -r ${n}b.hs || -r ${n}b.c ]]; then
          continue
        else
          if [[ -r ${n}ah.output ]]; then
            do_submit "$year" "$n" 1 "$n"ah.output
          elif [[ -r ${n}ac.output ]]; then
            do_submit "$year" "$n" 1 "$n"ac.output
          else
            echo "Couldn't find anything to submit." >&2
            exit 1
          fi
          return
        fi
      else
        ((n--))
        if [[ -r ${n}bh.output ]]; then
          do_submit "$year" "$n" 2 "$n"bh.output
        elif [[ -r ${n}bc.output ]]; then
          do_submit "$year" "$n" 2 "$n"bc.output
        else
          echo "Couldn't find anything to submit." >&2
          exit 1
        fi
        return
      fi
    done
  fi
}

fcd() {
  local t
  for t in ~/src/*/"$1" ~/src/*/*/"$1" ~/src/*/*/*/"$1" ~/src/*/*/*/*/"$1" ~/src/*/*/*/*/*/"$1" ~/src/*/*/*/*/*/*/"$1"; do
    if [[ -d "$t" ]]; then
      cd "$t"
      return
    fi
  done
  cd "$1"
}
_fcd() {
  local t=($(find ~/src -maxdepth 8 -type d -name .git -print0 | xargs -0 -n 1 basename_dirname))
  for i in "${!t[@]}"; do [[ ${t[i]} =~ ^$2 ]] || unset -v 't[$i]'; done
  COMPREPLY=("${t[@]##*/}")
}
complete -F _fcd fcd

fzf-d() {
  if ! command -v fzf-tmux > /dev/null; then echo 'Please install fzf-tmux' >&2; return 1; fi
  local path="$1"; shift
  local maxdepth="$1"; shift
  local query="$1"; shift
  if [[ -z $path ]]; then path=.; fi
  if [[ -z $maxdepth ]]; then maxdepth=5; fi
  if [[ -z $query ]]; then query=''; fi

  printf "$path"/

  local fzfopts fzfver="$(fzf --version)"; fzfver="${fzfver#*.}"; fzfver="${fzfver%%[. ]*}"
  if [[ $fzfver -ge 33 ]]; then
    fzfopts+=(--scheme=path)
  fi

  local x
  for d in "$@"; do
    x+=(-path */"$d" -prune -o)
  done
  x+=(-false)
  (
    printf "\0"
    find "$path" -mindepth 1 -maxdepth "$maxdepth" -type d -not \( "${x[@]}" \) -print0 2> /dev/null | while IFS= read -r -d $'\0' line; do
      printf "${line#$path/}\0"
    done
  ) | fzf-tmux -0 -1 -p -q "$query" --reverse --read0 "${fzfopts[@]}" --bind backward-eof:abort --preview 'ls -al --color '"$path"'/{}'
}

fzf-r() {
  if ! command -v fzf-tmux > /dev/null; then echo "Please install fzf-tmux" >&2; return 1; fi
  local fzfopts fzfver="$(fzf --version)"; fzfver="${fzfver#*.}"; fzfver="${fzfver%%[. ]*}"
  if [[ $fzfver -ge 33 ]]; then
    fzfopts+=(--scheme=path)
  fi
  printf "$HOME"/
  (
    printf "\0"
    find ~/src -maxdepth 8 -type d -name .git -print0 2> /dev/null | while IFS= read -r -d $'\0' line; do
      line="${line#~/}"
      printf "${line%/.git}\0"
    done
  ) | fzf-tmux -p 90%,90% -0 -1 -p -q "$1" --reverse --read0 "${fzfopts[@]}" --bind backward-eof:abort --preview 'git -C ~/{} la --color=always'
}

f() {
  local d
  if d="$(fzf-d . 8 "$1" .git/\*)"; then
    if [[ $d != "./" ]]; then
      cd "$d"
      ls -al
    fi
  fi
}

ff() {
  local d
  if d="$(fzf-d ~ 4 "$1" Library '.*' 'Pictures/Photos Library.photoslibrary' '*.localized' '*.fcpbundle')"; then
    if [[ $d != ~/ ]]; then
      cd "$d"
      f "$2"
    fi
  fi
}

tmux_switchc() {
  if [[ -z $1 || $1 == "~" ]]; then
    tmux switchc -t '\~'
  else
    tmux switchc -t "$1"
  fi
}

fs() {
  local d s t
  # local tmuxver="$(tmux -V)"; tmuxver="$(tr -cd 0-9 <<< "${tmuxver#* }")"
  if d="$(fzf-r "$@")"; then
    s="${d#~/}"
    s="${s#src/}"
    s="$(tr -cd '[:print:]' <<< "$s")"
    if [[ -z $s ]]; then s=\~; fi
    t="$(tmux -q ls -f "#{==:#{session_group},$s}" -F '#{session_group}' 2> /dev/null)"
    if [[ $t ]]; then
      if [[ $TMUX ]]; then
        tmux_switchc "$s"
      else
        tmux new -t "$s" \; set-option destroy-unattached
      fi
    else
      pushd "$d" > /dev/null
      if [[ $TMUX ]]; then
        tmux new -d -t "$s"
        tmux_switchc "$s"
      else
        tmux new -s "$s" -t "$s"
#         tmux new -t "$s" \; set-option destroy-unattached
      fi
      popd > /dev/null
    fi
  else
    return -1
  fi
}

fssh() {
  if ! fs "$@"; then
    exec bash -il
  fi
}

j() {
  local verbose= ver

  if [[ $1 == -v ]]; then
    verbose=1
    shift
  fi

  if ! /usr/libexec/java_home > /dev/null 2>&1; then
    [[ $verbose ]] && echo "No Java installed." >&2
    return 1
  fi

  if [[ -z $1 ]]; then
    echo "JAVA_HOME=$JAVA_HOME"
    /usr/libexec/java_home -V
  else
    ver="$1"
    if [[ $ver == latest ]]; then
      ver="$(/usr/libexec/java_home -V 2>&1 | grep -A1 ^Match | tail -1 | sed 's/^ *\([^,]*\),.*$/\1/')"
    fi
    export JAVA_HOME="$(/usr/libexec/java_home -v "$ver")"
    [[ $verbose ]] && echo "JAVA_HOME=$JAVA_HOME"
    return 0
  fi
}

j latest

# The following is for working with NixOS
[[ -r $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . "$HOME/.nix-profile/etc/profile.d/nix.sh"
[[ -r $HOME/.mylocale ]] && . "$HOME/.mylocale"

# The following is for Go development
#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
# [[ -s "$HOME/.gvm/bin/gvm-init.sh" ]] && source "$HOME/.gvm/bin/gvm-init.sh"

setbg() {
  bashrc_bgcolour="$("$HOME/.local/bin/setbg" "$@")"
}

setbg "${bggamma}"

# Set bashrc_cmd to 1 every time a new command is executed
trap '[[ $BASH_COMMAND != $PROMPT_COMMAND ]] && bashrc_cmd=1' DEBUG

bashrc_prompt() {
  local bashrc_exit_status="${?#0}" bashrc_row bashrc_col bashrc_nonl=
  if [[ $bashrc_cmd ]]; then
    IFS=\; read -sdR -p $'\e[6n' bashrc_row bashrc_col
    bashrc_row="$(cut -c3- <<< "$bashrc_row")"
    (( bashrc_col != 1 )) && bashrc_nonl=1
    if [[ $bashrc_nonl || $bashrc_exit_status ]]; then
      [[ $bashrc_nonl ]] || printf '\e[A'
      printf "\e[s\e[$((COLUMNS - bashrc_nonl - $(wc -c <<< "$bashrc_exit_status")))G\e[31m\e[41m\e[30m\e[1m${bashrc_exit_status}${bashrc_nonl:+}\e[m\e[31m\e[m\e[u\n"
    fi
  fi
  unset bashrc_cmd
#   tput sc; tput home; printf "%*s" $COLUMNS "$(date)"; tput rc
  bashrc_check_repo
  bashrc_term_title_and_colours
}

[[ -r "$HOME/.bashrc_localafter" ]] && . "$HOME/.bashrc_localafter"

PROMPT_COMMAND=bashrc_prompt
