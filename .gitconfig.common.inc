[user]
  name = Pete Ryland
  email = pdr@pdr.cx
[push]
  default = simple
  autoSetupRemote = true
[pretty]
  l = format:%C(auto)%h%d %G? %Cgreen%aN <%ae> %Cblue%ar%Creset: %s
[format]
  pretty = l
[alias]
  b    = branch
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
  l    = log --graph --decorate --abbrev-commit
  pf   = push --force-with-lease
  rc   = rebase --continue
  ri   = rebase -i HEAD~15
  rn   = release-notes
  s    = status -uno
  su   = status
  sw   = switch
  w    = switch
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
