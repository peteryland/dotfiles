[user]
  name = Pete Ryland
  email = pdr@pdr.cx
[push]
  default = simple
[pretty]
  l = format:%C(auto)%h%d %Cgreen%an <%ae> %Cblue%ar%Creset: %s
[format]
  pretty = l
[alias]
  lol = log --graph --decorate --abbrev-commit
  lola = log --graph --decorate --abbrev-commit --all
  s = status -uno
  su = status
  f = fetch
  rn = release-notes
  d = diff
  dc = diff --cached
  ds = diff --staged
  c = commit
  fp = format-patch
  hist = log HEAD^^^..
  co = checkout
  b = branch
  ca = commit --amend -q --no-edit
  pf = push --force-with-lease
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
[init]
  defaultBranch = main
[apply]
  whitespace = fix
[diff]
  algorithm = histogram
  indentHeuristic = true
  tool = vimdiff
