[user]
  name = Pete Ryland
  email = pdr@pdr.cx
[push]
  default = simple
[pretty]
  l = format:%C(auto)%h%d %G? %Cgreen%aN <%ae> %Cblue%ar%Creset: %s
[format]
  pretty = l
[alias]
  l    = log --graph --decorate --abbrev-commit
  la   = log --graph --decorate --abbrev-commit --all
  s    = status -uno
  su   = status
  f    = fetch
  rn   = release-notes
  d    = diff
  ds   = diff --staged
  c    = commit
  fp   = format-patch
  hist = log HEAD^^^..
  co   = checkout
  b    = branch
  w    = switch
  ca   = commit --amend -q --no-edit
  can  = commit --amend -q --no-edit --date=now
  pf   = push --force-with-lease
  capf = !git commit --amend -q --no-edit && git push -q --force-with-lease && :
[rerere]
  auto = true
[color]
  branch = auto
  diff = auto
  interactive = auto
  status = auto
  ui = auto
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
  indentHeuristic = true
  tool = vimdiff
[core]
  sshCommand = ssh -o RemoteCommand=none -o RequestTTY=no
[advice]
  skippedCherryPicks = false
[fetch]
  writeCommitGraph = true

[includeIf "gitdir:~/src/debian/"]
  path = ~/src/debian/.gitconfig
