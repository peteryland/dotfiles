# Don't run when not an interactive shell
case $- in
  *i*) ;;
  *) return;;
esac

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

export LESS=-R
if [[ -x ~/.lessfilter ]]; then
  export LESSOPEN="|~/.lessfilter %s"
  export LESSQUIET=1
fi

if [ -z "${debian_chroot:-}" ] && [[ -r /etc/debian_chroot ]]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

if [[ -r "$HOME/.bash_aliases" ]]; then
  . "$HOME/.bash_aliases"
fi

if [[ -r "$HOME/.bashrc_local" ]]; then
  . "$HOME/.bashrc_local"
fi

case "$OSTYPE" in
  darwin*)
    export LSCOLORS=ExFxBxDxCxegedabagacad
    alias ls='ls -Gh'
    alias dfh='df -h /System/Volumes/Data'
    alias pps='ps -ef'
    md5s() { md5 "$@"; }
    statm() { stat -f %m "$@"; }
    stty discard undef # Give me my ctrl-o back!
    ;;
  linux*)
    if [[ -x /usr/bin/dircolors ]]; then
      test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
      alias ls='ls --color=auto'
      alias dir='dir --color=auto'
      alias vdir='vdir --color=auto'

      alias grep='grep --color=auto'
      alias fgrep='fgrep --color=auto'
      alias egrep='egrep --color=auto'
    fi
    alias dfh='df -lh -x tmpfs -x devtmpfs'
    alias st='systemctl status'
    alias pps='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,stime,tty=TTY,time,cmd'
    alias ppsc='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,cgname:120,stime,tty=TTY,time,cmd'
    alias np='sudo netstat -plant'
    alias ng='np | grep'
    md5s() { md5sum "$@"; }
    statm() { stat --printf %Y "$@"; }
    ;;
  *)
    statm() { return 0; }
esac

# Set rgb in .bashrc_local for a fixed background colour
if [[ -z "$rgb" ]]; then
  # Set devmachine=1 in non-prod .bashrc_local to ensure brighter colours for prod machines
  if [[ $devmachine ]]; then
    bgfactor=4
  else
    bgfactor=2
  fi

  rgb=$(hostname -s | md5s | cut -c 1-6 | tr a-f A-F)
  red=$(printf %02x 0x$(bc <<< "ibase=obase=16; $(cut -c 6 <<< "$rgb")$(cut -c 2 <<< "$rgb") / $bgfactor"))
  green=$(printf %02x 0x$(bc <<< "ibase=obase=16; $(cut -c 5 <<< "$rgb")$(cut -c 4 <<< "$rgb") / $bgfactor"))
  blue=$(printf %02x 0x$(bc <<< "ibase=obase=16; $(cut -c 3 <<< "$rgb")$(cut -c 1 <<< "$rgb") / $bgfactor"))
  rgb="$red$green$blue"
  unset red green blue
fi

if [ "$UID" -eq 0 ]; then
  usercol=1
else
  usercol=6
fi
export PS1='${isrpi:+\[\e[38;5;125m\]\[\e[m\] }${islinux:+ }${isdeb:+\[\e[38;5;162m\]\[\e[m\] }${ismac:+ }${bashrc_exit_status:+\[\e[31m\]$bashrc_exit_status \[\em\]}\[\e[m\]${debian_chroot:+(\[\e[31m\]$debian_chroot\[\e[m\]) }\[\e[3'"$usercol"'m\]\u\[\e[m\]@\[\e[32m\]\h:\[\e[33m\]\w\[\e[m\]${bashrc_git_status:+[${bashrc_git_branch:+\[\e[34m\]$bashrc_git_branch}${bashrc_git_ahead:+\[\e[32m\]↑$bashrc_git_ahead}${bashrc_git_behind:+\[\e[31m\]↓$bashrc_git_behind}${bashrc_git_extrastatus:+\[\e[33m\]$bashrc_git_extrastatus}\[\e[m\]]}\$ '
unset usercol

bashrc_term_title() {
  # set terminal tab title
  printf "\e]1;$(id -un)@$(hostname -s)\a"
  # set terminal window title
  printf "\e]2;$(id -un)@$(hostname -s):${PWD/$HOME/~}\a"
}

if [ "$TERM_PROGRAM" ]; then
  termprog="$TERM_PROGRAM"
else
  termprog="$TERM"
fi

case "$(uname -s)" in
  Darwin)
    ismac=1
    ;;
  Linux)
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
    ;;
esac

case "$termprog" in
  iTerm*)
    export CLICOLOR=1
    ;&
  xterm-color|*-256color|xterm-kitty)
    export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
    bashrc_term_title_and_colours() {
      if [ -z "$VIM_TERMINAL" ]; then
        bashrc_term_title
        # set background colour
        printf "\e]Ph$rgb\e"\\
        printf "\e]11;#$rgb\e"\\
      fi
    }
    ;;
  Apple_Terminal)
    export CLICOLOR=1
    bashrc_term_title_and_colours() {
      if [ -z "$VIM_TERMINAL" ]; then
        bashrc_term_title
        # set background colour
        printf "\e]11;#$rgb\e"\\
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

  # if we're in a repo with a readable .git dir (but not in $HOME) # && $HOME != "$(git rev-parse --show-toplevel)"
  if [[ -e "$(git rev-parse --git-dir 2>/dev/null)" ]]; then
    # get the repo dir
    repo=$(git rev-parse --show-toplevel)
    # do a fetch if we haven't done one for more than a minute
    if [[ ! ( -r "$repo"/.git/FETCH_HEAD ) || ( $(( $(date +%s) - $(statm "$repo"/.git/FETCH_HEAD) )) -gt 60 ) ]]; then
      (
        GIT_TERMINAL_PROMPT=0 git fetch --quiet &> /dev/null & disown -a
      )
    fi
    status="$(git status --porcelain=1 -b)"
    status1="$(head -1 <<< "$status")"
    bashrc_git_branch="$(cut -c4- <<< "$status1" | cut -d\. -f1)"
    if [[ $bashrc_git_branch =~ master|main ]]; then
      bashrc_git_branch=
    fi
    bashrc_git_ahead="$(grep ahead <<< "$status1" | sed 's/.*ahead \([0-9]*\).*/\1/')"
    bashrc_git_behind="$(grep behind <<< "$status1" | sed 's/.*behind \([0-9]*\).*/\1/')"
    bashrc_git_extrastatus=$(grep -q '^[AM]' <<< "$status" && echo -n S; grep -q ^.M <<< "$status" && echo -n M) #; grep -q ^\?\? <<< "$status" && echo -n U)
    bashrc_git_status="$bashrc_git_branch${bashrc_git_ahead:+↑$bashrc_git_ahead}${bashrc_git_behind:+↓$bashrc_git_behind}$bashrc_git_extrastatus"
  fi
}

bashrc_prompt() {
  bashrc_exit_status=${?#0}
  # tput sc; tput home; printf "%*s" $COLUMNS "$(date)"; tput rc
  bashrc_check_repo
  bashrc_term_title_and_colours
}

PROMPT_COMMAND=bashrc_prompt
bashrc_prompt

if [[ $(type -t pg) = alias ]]; then unalias pg; fi
pg() {
  ps -fp $(pgrep -f "$@")
}

alias pj='pg java'
alias ll='ls -al'
alias lt='ls -altr'
alias ghci='ghci -v0 -ignore-dot-ghci -ghci-script ~/.ghci.standalone'

mv. () {
  oldname="$(pwd)"
  newname="$1"
  if [[ -z $newname ]]; then
    echo "Usage: mv. newname" >&2
    return 1
  fi
  cd ..
  mv "$oldname" "$newname"
  cd "$newname"
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
PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
bashrc_path_add /usr/local/texlive/2017/bin/x86_64-darwin
bashrc_path_add /usr/local/go/bin "$HOME/Library/Haskell/bin"
# bashrc_path_add "$HOME/.nix-profile/bin" "$HOME/.nix-profile/sbin"
bashrc_path_add "$HOME/.cabal/bin" "$HOME/.cabal/sbin"
bashrc_path_add "$HOME/.local/bin" "$HOME/.local/sbin"
bashrc_path_add "$HOME/local/bin" "$HOME/local/sbin"
bashrc_path_add "$HOME/.bin" "$HOME/.sbin"
bashrc_path_add "$HOME/bin" "$HOME/sbin"

[[ -x /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"

export PATH

if [[ -r ~/.inputrc ]]; then
  export INPUTRC=~/.inputrc
fi

if [[ -r "$HOME"/.local/bin/color-dark ]]; then
  . "$HOME"/.local/bin/color-dark
fi

for i in /etc/profile.d/*.sh /usr/local/etc/profile.d/*.sh; do
  [[ -r $i ]] && . "$i"
done

if ! shopt -oq posix && [[ -z $BASH_COMPLETION_VERSINFO ]]; then
  if [[ -r ~/.bash_completion ]]; then
    . ~/.bash_completion
  elif [[ -r /etc/bash_completion ]]; then
    . /etc/bash_completion
  elif [[ -r /usr/local/etc/bash_completion ]]; then
    . /usr/local/etc/bash_completion
  elif [[ -r /usr/local/share/bash-completion/bash_completion ]]; then
    . /usr/local/share/bash-completion/bash_completion
  elif [[ -r /usr/share/bash-completion/bash_completion ]]; then
    . /usr/share/bash-completion/bash_completion
  fi

  if type -p stack > /dev/null; then
    eval "$(stack --bash-completion-script stack)"
  fi
fi

spf() {
  while [ "$1" ]; do
    host -t TXT "$1" | cut -d\" -f2 | grep '^v=spf'
    shift
  done
}

j() {
  if [ "$1" = "-v" ]; then
    verbose=1
    shift
  fi

  if [[ ! -x /usr/libexec/java_home ]] || ! /usr/libexec/java_home > /dev/null 2>&1; then
    if [ "$verbose" ]; then
      echo "No Java installed."
    fi
    return
  fi

  ver="$1"
  if [ -z "$ver" ]; then
    echo JAVA_HOME="$JAVA_HOME"
    /usr/libexec/java_home -V
  else
    if [ "$ver" = latest ]; then
      ver="$(/usr/libexec/java_home -V 2>&1 | grep -A1 ^Match | tail -1 | sed 's/^ *\([^,]*\),.*$/\1/')"
    fi
    export JAVA_HOME=$(/usr/libexec/java_home -v "$ver")
    if [ "$verbose" ]; then
      echo JAVA_HOME="$JAVA_HOME"
    fi
  fi
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

LOOKINGDIR="$HOME/src/looking"
l() {
  mkdir -p "$LOOKINGDIR"
  local dir="${1#*/}"
  if [[ $1 ]]; then
    if [[ $1 = s-* ]]; then
      opts="${1/s}"
      shift
      echo "ls $opts $@" >&2
      ls "$opts" "$@"
    elif [[ -n $dir && -d "$LOOKINGDIR"/$dir ]]; then
      cd "$LOOKINGDIR"/"$dir"
    elif [[ $1 == ?*/?* ]]; then
      if [ "$(curl -Ls -w '%{response_code}' -o/dev/null https://gitlab.com/"$1")" -eq 200 ]; then
        cd "$LOOKINGDIR"
        git clone https://gitlab.com/"$1"
      elif [ "$(curl -Ls -w '%{response_code}' -o/dev/null https://github.com/"$1")" -eq 200 ]; then
        cd "$LOOKINGDIR"
        git clone https://github.com/"$1"
      else
        echo "l: $1 not found" >&2
        return 1
      fi
      cd "$dir"
    else
      echo "l: Error parsing arguments" >&2
      return 1
    fi
  else
    cd "$LOOKINGDIR"
  fi
}

_l() {
  local compreply=($(compgen -d "$LOOKINGDIR"/ | grep "^$LOOKINGDIR/$2"))
  COMPREPLY=(${compreply[@]#$LOOKINGDIR/})
}
complete -F _l l

n() {
  local f n
  if [[ $PWD == */adventofcode/* ]]; then
    diryear="$(($(basename "$PWD")))"
    for ((n=1; n<=25; n++)); do
      if [ -r "$n"a.hs -o -r "$n"a.c ]; then
        if [ -r "$n"b.hs -o -r "$n"b.c ]; then
          continue
        else
          # mkaoc for part 1 tries to create a parser for the input file and for part 2 simply copies over part 1
          vi +$ "$(mkaoc "$n" b)"
          return
        fi
      else
        # Don't fetch anything automatically if we're not solving this year's puzzles
        if [ "$(date +%Y)" -eq "$diryear" ]; then
          export TZ=EST
          while [ "$(date +%d)" -lt "$n" ]; do
            sleep 1
          done
          # Don't hit the server immediately, you need time to read the problem anyway
          sleep 2
          if [ -x "$HOME"/src/games/coding/adventofcode/get_input ]; then
            "$HOME"/src/games/coding/adventofcode/get_input
          fi
        fi
        vi +$ "$(mkaoc "$n" a)"
        return
      fi
    done
  fi
}

do_submit() {
  if [ -r "$HOME"/src/games/coding/adventofcode/.cookie ]; then
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
      if [ -r "$n"a.hs -o -r "$n"a.c ]; then
        if [ -r "$n"b.hs -o -r "$n"b.c ]; then
          continue
        else
          if [ -r "$n"ah.output ]; then
            do_submit "$year" "$n" 1 "$n"ah.output
          elif [ -r "$n"ac.output ]; then
            do_submit "$year" "$n" 1 "$n"ac.output
          else
            echo "Couldn't find anything to submit." >&2
            exit 1
          fi
          return
        fi
      else
        ((n--))
        if [ -r "$n"bh.output ]; then
          do_submit "$year" "$n" 2 "$n"bh.output
        elif [ -r "$n"bc.output ]; then
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

j latest

# The following is for working with NixOS
[[ -r "$HOME/.nix-profile/etc/profile.d/nix.sh" ]] && . "$HOME/.nix-profile/etc/profile.d/nix.sh"
[[ -r "$HOME/.mylocale" ]] && . "$HOME/.mylocale"

# The following is for Go development
#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
# [[ -s "$HOME/.gvm/bin/gvm-init.sh" ]] && source "$HOME/.gvm/bin/gvm-init.sh"
