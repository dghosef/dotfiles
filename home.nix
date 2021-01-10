# inspired by https://github.com/srid/nix-config/blob/705a70c094da53aa50cf560179b973529617eb31/nix/home.nix
{ config, pkgs, ... }:

let
  allPlatformImports = [
    ./shell/git.nix
    ./shell/fish.nix
    ./shell/tmux.nix
    ./shell/vim.nix
  ];
  nixImports = [
    ./gui/i3.nix
    ./gui/locker.nix
    ./gui/rofi.nix
    ./shell/termite.nix
  ];
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "dghosef";
  home.homeDirectory = "/home/dghosef";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
  imports = if builtins.pathExists /etc/nixos
            then (allPlatformImports ++ nixImports)
            else allPlatformImports;
  programs.fzf.enable = true;
  programs.fzf.defaultCommand = "fd --type f --hidden";
  nixpkgs.config.allowUnfree = true;
  home.packages = [
    # Web
    pkgs.firefox
    pkgs.qutebrowser
    pkgs.spotify
    pkgs.keepassxc
    # Images
    pkgs.feh
    pkgs.gimp
    # Emacs
    pkgs.emacs
    pkgs.irony-server
    # Python
    pkgs.black
    pkgs.python38
    pkgs.python38Packages.jedi
    pkgs.python38Packages.yapf
    pkgs.python38Packages.gnureadline
    pkgs.python38Packages.ipython
    pkgs.python38Packages.pep8
    pkgs.python
    # C-langs
    pkgs.bear
    pkgs.clang
    pkgs.llvm
    # Communication
    pkgs.zoom-us
    pkgs.discord
  ];
  # Config files
  home.file.".emacs".source = ./init.el;
  home.file.".config/qutebrowser/config.py".source = ./gui/qutebrowser/config.py;
}
