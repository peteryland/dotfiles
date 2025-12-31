[user]
  name = Pete Ryland
  email = pdr@pdr.cx
[push]
  default = simple
  autoSetupRemote = true
[pretty]
  lf = format:%C(auto)%h%d %G? %C(18)%aN <%ae> %C(66)%aD%Creset: %s
  l = format:%C(auto)%h%d %G? %C(18)%aN <%ae> %C(66)%ar%Creset: %s
[format]
  pretty = l
[alias]
  b    = branch
  bd   = !b=$(git branch --show-current) && git switch main && git pull -p && git branch -D "$b" && :
  ca   = commit --amend -q --no-edit
  can  = commit --amend -q --no-edit --date=now
  capf = !git commit --amend -q --no-edit && git push -q --force-with-lease && :
  c    = commit
  co   = checkout
  d    = diff
  ds   = diff --staged
  f    = fetch -ap
  fp   = format-patch
  hist = log HEAD^^^..
  la   = log --graph --decorate --abbrev-commit --all
  lf   = log --graph --decorate --abbrev-commit --all --pretty=lf
  l    = log --graph --decorate --abbrev-commit
  pf   = push --force-with-lease
  rc   = rebase --continue
  ri   = rebase -i HEAD~15
  rn   = release-notes
  s    = status -uno
  su   = status
  sw   = switch
  w    = switch
  wc   = switch -c
[rerere]
  auto = true
[include]
  path = ~/.gitconfig.colour.inc
[column]
  ui = auto
  branch = never
[pull]
  rebase = true
[rebase]
  autoStash = true
  autoSquash = true
  updateRefs = true
[init]
  defaultBranch = main
[apply]
  whitespace = fix
[diff]
  algorithm = histogram
  wsErrorHighlight = all
[core]
  whitespace = cr-at-eol,tab-in-indent,tabwidth=2
  sshCommand = ssh -o RemoteCommand=none -o RequestTTY=no
  pager = "sed -e 's/\\t/┊ /g' -e 's/\\(..\\[.*m\\) \\(.\\[m\\)\\?$/\\1␣\\2/g' | ${PAGER:-/usr/bin/less -+S}"
[advice]
  skippedCherryPicks = false
  detachedHead = false
[fetch]
  writeCommitGraph = true

[includeIf "gitdir:~/src/debian/"]
  path = ~/src/debian/.gitconfig
