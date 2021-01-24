# inspired by https://github.com/srid/nix-config/blob/705a70c094da53aa50cf560179b973529617eb31/nix/home.nix
{ config, pkgs, ... }:

with pkgs;
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
  services.dropbox.enable = true;
  nixpkgs.config.allowUnfree = true;
  home.packages = [
    # Misc
    pkgs.zip
    pkgs.unzip
    # Web
    pkgs.firefox
    pkgs.google-chrome
    pkgs.qutebrowser
    pkgs.spotify
    pkgs.keepassxc
    pkgs.kpcli
    pkgs.wget
    pkgs.w3m
    # Images
    pkgs.feh
    pkgs.gimp
    pkgs.krita
    pkgs.drawing
    # Emacs(try to keep most of config in init.el so it's portable)
    pkgs.emacs
    pkgs.emacs26Packages.pdf-tools
    pkgs.emacs26Packages.elpy
    pkgs.zip
    pkgs.ripgrep
    pkgs.irony-server # c-type language autocomplete
    pkgs.texlive.combined.scheme-full # latex
    pkgs.ispell
    # C-langs
    pkgs.bear
    pkgs.gdb
    pkgs.clang
    # 107e
    pkgs.screen
    pkgs.binutils
    pkgs.dpkg
    pkgs.patchelf
    pkgs.readline
    pkgs.desktop-file-utils
    # Communication
    pkgs.zoom-us
    pkgs.discord
    # FPL team generator
    pkgs.cbc
    # Gaming
    pkgs.minecraft
  ];
  # Config files
  home.file.".emacs".source = ./emacs/init.el;
  home.file.".folders".source = ./emacs/folders;
  home.file.".config/qutebrowser/config.py".source = ./gui/qutebrowser/config.py;
}