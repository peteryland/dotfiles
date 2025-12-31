# Don't run when not an interactive shell
case $- in
  *i*) ;;
  *) return;;
esac

for bashrc_profile in /etc/profile.d/*.sh /usr/local/etc/profile.d/*.sh "$HOME"/.bashrc_local "$HOME"/.bashrc_local.d/*; do
  [[ -r $bashrc_profile ]] && . "$bashrc_profile"
done
unset bashrc_profile

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

export LESS=-RFS
if [[ -x ~/.lessfilter ]]; then
  export LESSOPEN="|~/.lessfilter %s"
  export LESSQUIET=1
fi

if [[ -z ${debian_chroot:-} && -r /etc/debian_chroot ]]; then
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
    alias dfh='df -h /System/Volumes/Data'
    alias pps='ps -ef'
    if command -v netstat > /dev/null; then
      alias np='sudo netstat -plant'
    else
      alias np='sudo lsof -nP -iudp -itcp -stcp:LISTEN | /usr/bin/grep -v -- "->"'
    fi
    alias ng='np | /usr/bin/grep'
    md5s() { md5 -- "$@"; }
    statm() { stat -f %m -- "$@"; }
    export LOCATE_PATH=~/.local/var/locate/locatedb
    if [[ ! -r "$LOCATE_PATH" ]]; then
      mkdir -p "$(dirname "$LOCATE_PATH")"
      # This should be put into cron as well
      sudo -n find "$HOME" -path "$HOME"/Library -prune -or -path "$HOME"/.Trash -prune -or -path "$HOME"/.vim/undo -prune -or -path "$HOME"/.local/share -prune -or -name .cache -type d -prune -or -name .cabal -type d -prune -or -name .ghcup -type d -prune -or -name .git -type d -prune -or -path \*/go/pkg -prune -or -print 2> /dev/null | /usr/libexec/locate.mklocatedb > "$LOCATE_PATH"
    fi
    if [[ ! -r ~/.gitconfig ]]; then ln -nfs ~/.gitconfig.mac ~/.gitconfig; fi
    ;;
  msys*)
    iswindows=1
    alias dfh='df -lh -x tmpfs -x devtmpfs -x efivarfs -x rootfs -x overlay -x 9p'
    alias st='systemctl status'
    alias pps='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,stime,tty=TTY,time,cmd'
    alias ppsc='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,cgname:120,stime,tty=TTY,time,cmd'
    if command -v netstat > /dev/null; then
      alias np='sudo netstat -plant'
    else
      alias np='sudo lsof -nP -iudp -itcp -stcp:^CLOSED,^ESTABLISHED,^SYN_SENT,^CLOSE_WAIT,^FIN_WAIT1,^CLOSING,^LAST_ACK,^TIME_WAIT | /usr/bin/grep -v -- "->"'
    fi
    alias ng='np | /usr/bin/grep'
    if [[ -r ~/.xmonad/xmonad.hs ]]; then
      alias xme='"$EDITOR" ~/.xmonad/xmonad.hs'
    fi
    md5s() { md5sum -- "$@"; }
    statm() { stat --printf %Y -- "$@"; }
    export LOCATE_PATH=~/.local/var/locate/locatedb
    if [[ ! -r "$LOCATE_PATH" ]]; then
      mkdir -p "$(dirname "$LOCATE_PATH")"
      # This should be put into cron as well
      updatedb --localpaths="$HOME" --findoptions="-name .cache -type d -prune -or -name .cabal -type d -prune -or -name .ghcup -type d -prune -or -path */.vim/undo -prune -or -path */.local/share -prune -or -name .git -type d -prune -or -path */go/pkg -prune -or" --output="$HOME"/.local/var/locate/locatedb
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
    alias dfh='df -lh -x tmpfs -x devtmpfs -x efivarfs -x rootfs -x overlay -x 9p'
    alias st='systemctl status'
    alias pps='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,stime,tty=TTY,time,cmd'
    alias ppsc='ps --forest -N -p 2 --ppid 2 -o user:11,pid,ppid,c,cgname:120,stime,tty=TTY,time,cmd'
    if command -v netstat > /dev/null; then
      alias np='sudo netstat -plant'
    else
      alias np='sudo lsof -nP -iudp -itcp -stcp:^CLOSED,^ESTABLISHED,^SYN_SENT,^CLOSE_WAIT,^FIN_WAIT1,^CLOSING,^LAST_ACK,^TIME_WAIT | /usr/bin/grep -v -- "->"'
    fi
    alias ng='np | /usr/bin/grep'
    if [[ -r ~/.xmonad/xmonad.hs ]]; then
      alias xme='"$EDITOR" ~/.xmonad/xmonad.hs'
    fi
    md5s() { md5sum -- "$@"; }
    statm() { stat --printf %Y -- "$@"; }
    export LOCATE_PATH=~/.local/var/locate/locatedb
    if [[ ! -r "$LOCATE_PATH" ]]; then
      mkdir -p "$(dirname "$LOCATE_PATH")"
      # This should be put into cron as well
      updatedb --localpaths="$HOME" --findoptions="-name .cache -type d -prune -or -name .cabal -type d -prune -or -name .ghcup -type d -prune -or -path */.vim/undo -prune -or -path */.local/share -prune -or -name .git -type d -prune -or -path */go/pkg -prune -or" --output="$HOME"/.local/var/locate/locatedb
    fi
    if [[ ! -r ~/.gitconfig ]]; then ln -nfs ~/.gitconfig.linux ~/.gitconfig; fi
    ;;
  *)
    statm() { return 0; }
esac

if [[ $TERM = linux ]]; then
  if [[ $UID -eq 0 ]]; then
    usercol=1
  else
    usercol=6
  fi
  export PS1='${debian_chroot:+(\[\e[3em\]$debian_chroot\[\e[m\]) }\[\e[3'"$usercol"'m\]\u\[\e[m\]@\[\e[32m\]\h:\[\e[33m\]\w\[\e[m\]${bashrc_git_status:+\[\e[90m\]<${bashrc_git_branch}${bashrc_git_ahead:+\[\e[31m\]↑$bashrc_git_ahead}${bashrc_git_behind:+\[\e[32m\]↓$bashrc_git_behind}${bashrc_git_tag:+\[\e[32m\]$bashrc_git_tag}${bashrc_git_extrastatus:+\[\e[33m\]$bashrc_git_extrastatus}\[\e[m\e[90m\]>\[\e[m\]}\$ '
else
  if [[ $UID -eq 0 ]]; then
    usercol=17
  else
    usercol=16
  fi
  export PS1="$platformlogo"'${iswindows:+\[\e[38;5;200m\]\[\e[m\] }${isrpi:+\[\e[38;5;201;1m\]\[\e[m\] }${islinux:+ }${isdeb:+\[\e[38;5;202m\]\[\e[m\] }${ismac:+ }'"$applogo"'${debian_chroot:+(\[\e[38;5;22m\]$debian_chroot\[\e[m\]) }\[\e[38;5;'"$usercol"'m\]\u\[\e[m\]@\[\e[38;5;23m\]\h\[\e[m\]:\[\e[38;5;25m\]\w\[\e[m\]${bashrc_git_status:+\[\e[38;5;43m\]\[\e[48;5;43m\]${bashrc_git_branch:+\[\e[38;5;48m\]$bashrc_git_branch}${bashrc_git_ahead:+\[\e[38;5;45m\]↑$bashrc_git_ahead}${bashrc_git_behind:+\[\e[38;5;44m\]↓$bashrc_git_behind}${bashrc_git_tag:+\[\e[38;5;47m\]$bashrc_git_tag}${bashrc_git_extrastatus:+\[\e[38;5;46m\]$bashrc_git_extrastatus}\[\e[m\e[38;5;43m\]\[\e[m\]}\$ '
fi
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
  iTerm*)
    bashrc_term_title_and_colours() {
      # iterm is currently *very* slow to update the palette, so do it sparingly only
#       printf "$bashrc_theme"
      if [[ -z $VIM_TERMINAL ]]; then
        bashrc_term_title
        # set background colour
        printf "\e]Ph${bashrc_bgcolour}\e\\"
#         printf "\e]11;#${bashrc_bgcolour}\e\\"
      fi
    }
    ;;
  *) # Tested with 256-colour xterm, kitty, linux console, Apple Terminal, WSL Terminal
    if [[ $termprg == xterm-kitty ]]; then
      alias icat="kitty +kitten icat"
    fi
    bashrc_term_title_and_colours() {
      # set colour scheme
      printf "$bashrc_theme"
      if [[ -z $VIM_TERMINAL ]]; then
        if [[ $termprg == linux ]]; then
          # set background colour
          printf "\e]P0${bashrc_bgcolour}"
          # you'll need to switch consoles and back to reset the colours for the whole screen
        else
          bashrc_term_title
          # set background colour
          printf "\e]11;#${bashrc_bgcolour}\e\\"
        fi
      fi
    }
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

  [[ $(git rev-parse --is-inside-git-dir 2> /dev/null) != true ]] || return

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
    bashrc_git_tag="$(git log --pretty=%d -1 2> /dev/null | tr , \\n | /usr/bin/grep '^ tag: ' | head -1 | sed 's/^.* tag: \([^)]*\))\?$/\1/')"
    status="$(git status --porcelain=1 -b)"
    status1="$(head -1 <<< "$status")"
    bashrc_git_branch="$(cut -c4- <<< "$status1")"
    bashrc_git_branch="${bashrc_git_branch%%\.\.\.*}"
    if [[ $bashrc_git_branch =~ ^(master|main)$ ]]; then
      bashrc_git_branch=
    fi
    bashrc_git_ahead="$(/usr/bin/grep ahead <<< "$status1" | sed 's/.*ahead \([0-9]*\).*/\1/')"
    bashrc_git_behind="$(/usr/bin/grep behind <<< "$status1" | sed 's/.*behind \([0-9]*\).*/\1/')"
    if git stash list | /usr/bin/grep . > /dev/null 2>&1; then
      bashrc_git_hasstash='#'
    else
      bashrc_git_hasstash=
    fi
    bashrc_git_extrastatus="$(/usr/bin/grep -q '^[ACDMRT]' <<< "$status" && printf S; /usr/bin/grep -q '^.[CDMRT]' <<< "$status" && printf M)$bashrc_git_hasstash" #; /usr/bin/grep -q ^\?\? <<< "$status" && printf U)
    bashrc_git_status="$bashrc_git_branch${bashrc_git_ahead:+↑$bashrc_git_ahead}${bashrc_git_behind:+↓$bashrc_git_behind}${bashrc_git_tag:+ $bashrc_git_tag }$bashrc_git_extrastatus"
  fi
}

if [[ -x /usr/bin/dircolors ]]; then
  test -r ~/.dir_colors && eval "$(dircolors -b ~/.dir_colors)" || eval "$(dircolors -b)"
  alias dir='dir --color=auto --hyperlink=auto'
  alias vdir='vdir --color=auto --hyperlink=auto'
fi
if command -v eza >& /dev/null; then
  # unset LS_COLORS
  export EZA_COLORS='di=38;5;24:ex=38;5;28:fi=38;5;26:pi=38;5;32:so=38;5;31:bd=38;5;34:cd=38;5;33:ln=38;5;29:or=38;5;30:sp=38;5;35:uu=38;5;16:uR=38;5;17:un=38;5;18:gu=38;5;19:gR=38;5;20:gn=38;5;21:oc=38;5;56:ur=38;5;56:uw=38;5;56:ux=38;5;56:ue=38;5;56:gr=38;5;56:gw=38;5;56:gx=38;5;56:ge=38;5;56:tr=38;5;56:tw=38;5;56:tx=38;5;56:te=38;5;56:su=38;5;56:sf=38;5;56:xa=38;5;56:ga=38;5;36:gm=38;5;37:gd=38;5;38:gv=38;5;39:gt=38;5;40:gi=38;5;41:gc=38;5;42:Gm=38;5;53:Go=38;5;48:Gc=38;5;54:Gd=38;5;55:nb=38;5;57:ub=38;5;57:nk=38;5;58:uk=38;5;58:nm=38;5;59:um=38;5;59:ng=38;5;60:ug=38;5;60:nt=38;5;61:ut=38;5;61:df=38;5;62:ds=38;5;63:lc=38;5;64:lm=38;5;65:da=38;5;66:in=38;5;67:bl=38;5;68:hd=4;38;5;69:lp=38;5;70:bO=38;5;71:mp=4;38;5;72:im=38;5;73:vi=38;5;74:mu=38;5;75:lo=38;5;76:cr=38;5;77:do=38;5;78:co=38;5;79:tm=38;5;80:cm=38;5;81:bu=38;5;82:sc=38;5;83:ic=38;5;84:Sn=38;5;85:Su=38;5;86:Sr=38;5;87:St=38;5;88:Sl=38;5;89:ff=38;5;90:cc=38;5;97:xx=38;5;96'
  alias ls='eza --git --icons=auto'
  alias ll='ls -gl'
  alias la='ls -aagl'
  alias lt='ll -snew'
elif command -v exa >& /dev/null; then
  export EXA_COLORS='di=38;5;24:ex=38;5;28:fi=38;5;26:pi=38;5;32:so=38;5;31:bd=38;5;34:cd=38;5;33:ln=38;5;29:or=38;5;30:uu=38;5;16:un=38;5;18:gu=38;5;19:gn=38;5;21:ur=38;5;56:uw=38;5;56:ux=38;5;56:ue=38;5;56:gr=38;5;56:gw=38;5;56:gx=38;5;56:tr=38;5;56:tw=38;5;56:tx=38;5;56:su=38;5;56:sf=38;5;56:xa=38;5;56:ga=38;5;36:gm=38;5;37:gd=38;5;38:gv=38;5;39:gt=38;5;40:nb=38;5;57:ub=38;5;57:nk=38;5;58:uk=38;5;58:nm=38;5;59:um=38;5;59:ng=38;5;60:ug=38;5;60:nt=38;5;61:ut=38;5;61:df=38;5;62:ds=38;5;63:lc=38;5;64:lm=38;5;65:da=38;5;66:in=38;5;67:bl=38;5;68:hd=4;38;5;69:lp=38;5;70:bO=38;5;71:cc=38;5;97:xx=38;5;96'
  alias ls='exa --git --icons'
  alias ll='ls -gl'
  alias la='ls -aagl'
  alias lt='ll -snew'
else
  if [[ $OSTYPE = darwin* ]]; then
    alias ls='ls -G'
  else
    alias ls='ls --color=auto --hyperlink=auto'
  fi
  alias ll='ls -l'
  alias la='ls -al'
  alias lt='ll -tr'
fi

export GREP_COLORS='sl=38;5;100:cx=38;5;101:mt=1;38;5;102:fn=38;5;105:ln=38;5;96:bn=38;5;106:se=38;5;96'
if [[ $(type -t grep) == alias ]]; then unalias grep; fi
grep() {
  /usr/bin/grep --exclude-dir={.Trash,.cache,.git,.cabal,.ghcup,.idea,undo,.m2,dist-newstyle} --color=always "$@"
}
alias fgrep='/usr/bin/grep -F --color=always'
alias egrep='/usr/bin/grep -E --color=always'
alias rgrep='/usr/bin/grep -r --color=always'
if [[ $(type -t pg) == alias ]]; then unalias pg; fi
pg() {
  pids=($(pgrep -f "$@"))
  if [[ -n $pids ]]; then
    ps -fp "${pids[@]}"
  fi
}
alias pj='pg java'

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

alias glocate='locate -d ""'

if diff --version | grep -q GNU; then
  alias diff="/usr/bin/diff --color --palette='rs=0:hd=1:ad=38;5;36:de=38;5;38:ln=38;5;96'"
fi

alias ghci='ghci -v0 -ignore-dot-ghci -ghci-script ~/.ghci.standalone'

ssh() {
  command ssh "$@"
  printf "$bashrc_theme"
}

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
alias gc='git wc'
alias bd='git bd'

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

if [[ -r ~/.inputrc ]]; then
  export INPUTRC=~/.inputrc
fi

spf() {
  while [[ $1 ]]; do
    host -t TXT "$1" | cut -d\" -f2 | /usr/bin/grep --color=never '^v=spf'
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
  local opt dir="${1#*/}"
  if [[ $1 ]]; then
    if [[ $1 == s-* ]]; then
      opt="${1/s}"
      shift
      echo "ls $opt $@" >&2
      ls "$opt" "$@"
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
  local compreply=($(compgen -d -- "$LOOKINGDIR"/ | /usr/bin/grep "^$LOOKINGDIR/$2"))
  COMPREPLY=(${compreply[@]#$LOOKINGDIR/})
}
complete -F _l l

gi() {
  local opt
  if [[ $1 == t* ]]; then
    opt="${1/t}"
    shift
    echo "git $opt $@" >&2
    git "$opt" "$@"
  fi
}

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
      fetch_work_repos | /usr/bin/grep "^$2" # | sed "s/^\(${2//\//\\\/}[^/]*\/\?\).*/\1/"
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
      ver="$(/usr/libexec/java_home -V 2>&1 | /usr/bin/grep -A1 ^Match | tail -1 | sed 's/^ *\([^,]*\),.*$/\1/')"
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

dark() {
  if [[ $TERM = linux ]]; then
    bashrc_bgcolour=000000
  else
    setbg "${bggamma}"
  fi
  unset bashrc_theme
  if [[ -r $HOME/.local/bin/theme && -r $HOME/.config/themes/dark ]]; then
    bashrc_theme="$(theme "$bashrc_bgcolour" dark)"
  fi
}

light() {
  setbg 13
  unset bashrc_theme
  if [[ -r $HOME/.local/bin/theme && -r $HOME/.config/themes/light ]]; then
    bashrc_theme="$(theme "$bashrc_bgcolour" light)"
  fi
}

[[ $devmachine ]] && dark || light
printf "$bashrc_theme"

if command -v batcat >& /dev/null; then
  if [[ $HOME/.config/bat/themes/universal.tmTheme -nt $HOME/.cache/bat/themes.bin ]]; then
    batcat cache --build >& /dev/null
  fi
#   alias cat=/usr/bin/batcat
  alias bat='batcat --paging=never --wrap=never --color=always'
fi

cd() {
  builtin cd "$@"
  if [[ -r README.md ]]; then
    if command -v batcat >& /dev/null; then
      if [[ $TERM = xterm-256color ]]; then
        batcat --paging=never --wrap=never -r :30 README.md --color=always | sed 's/^\d27\[1;4;38;5;98m#\d27\[0m\d27\[1;4;38;5;98m \(.*\)$/\1\d27#3\n\1\d27#4/'
      else
        batcat --paging=never --wrap=never -r :30 README.md
      fi
    else
      head -30 README.md
    fi
    IFS=\; read -sdR -p $'\e[6n' bashrc_row bashrc_col
    bashrc_row="$(cut -c3- <<< "$bashrc_row")"
    (( bashrc_col != 1 )) && printf '\n'
  fi
}

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
      if [[ $TERM = linux ]]; then
        printf "\e[s\e[$((COLUMNS - bashrc_nonl - $(wc -c <<< "$bashrc_exit_status")))G\e[48;5;92m${bashrc_exit_status}${bashrc_nonl:+/}\e[m\e[u\n"
      else
        printf "\e[s\e[$((COLUMNS - bashrc_nonl - $(wc -c <<< "$bashrc_exit_status")))G\e[38;5;93m\e[48;5;93m\e[38;5;92m\e[1m${bashrc_exit_status}${bashrc_nonl:+}\e[m\e[38;5;93m\e[m\e[u\n"
      fi
    fi
  fi
  unset bashrc_cmd
#   tput sc; tput home; printf "%*s" $COLUMNS "$(date)"; tput rc
  bashrc_check_repo
  bashrc_term_title_and_colours
}

[[ -r "$HOME/.bashrc_localafter" ]] && . "$HOME/.bashrc_localafter"

PROMPT_COMMAND=bashrc_prompt
