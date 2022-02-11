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
  d = diff
  ds = diff --staged
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
[pull]
  rebase = true
[rebase]
  autoStash = true
[core]
  whitespace = warn
[init]
  defaultBranch = main
