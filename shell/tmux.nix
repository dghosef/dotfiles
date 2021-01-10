{pkgs, config, ...}:
{
  programs.tmux.newSession = true;
  programs.tmux.enable = true;
  programs.tmux.baseIndex = 1;
  programs.tmux.escapeTime = 0;
  programs.tmux.keyMode = "vi";
  programs.tmux.extraConfig = ''
    set -g default-terminal "screen-256color"
    # List of plugins.
    set -g @plugin 'tmux-plugins/tpm' # plugin manager
    set -g @plugin 'sunaku/tmux-navigate' # navigation integration with vim
    set -g @plugin 'jabirali/tmux-tilish' # i3-like bindings ;)
    # Plugin options.
    set -g @tilish-navigate 'on'
    set -g @tilish-default 'main-vertical'
    
    # Plugin options.
    set -g @tilish-navigator 'on'
    
    # Install `tpm` if needed.
    if "test ! -d ~/.tmux/plugins/tpm" \
       "run 'git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm && ~/.tmux/plugins/tpm/bin/install_plugins'"
    
    # Activate the plugins.
    run -b "~/.tmux/plugins/tpm/tpm"
  '';
}
