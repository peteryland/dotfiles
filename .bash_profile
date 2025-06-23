for bashrc_script in /etc/profile.d/*.sh /usr/local/etc/profile.d/*.sh "$HOME"/.bashrc; do
  [[ -r $bashrc_script ]] && . "$bashrc_script"
done
unset bashrc_script
