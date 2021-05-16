# inspired by https://github.com/srid/nix-config/blob/705a70c094da53aa50cf560179b973529617eb31/nix/home.nix
{ config, pkgs, ... }:

with pkgs;
let
  my-python-packages = python-packages: with python-packages; [
    # FPL team generator
    pandas
    requests
    pulp
    # Emacs python dev
    black
    jedi
    yapf
    gnureadline
    ipython
    pep8
    flake8
    rope
    autopep8
  ]; 
    python-with-my-packages = python3.withPackages my-python-packages;
  allPlatformImports = [
    ./shell/git.nix
    # ./shell/fish.nix
    ./shell/tmux.nix
    ./shell/vim.nix
  ];
  linixImports = [
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
  # Check if we're in a linux system
  imports = if builtins.pathExists /home
            then (allPlatformImports ++ linixImports)
            else allPlatformImports;
  services.dropbox.enable = true;
  nixpkgs.config.allowUnfree = true;
  home.packages = [
    # -------------System-------------------
    # General
    pkgs.system-config-printer
    pkgs.lm_sensors
    pkgs.zip
    pkgs.unzip
    pkgs.xsane
    pkgs.xdg_utils
    pkgs.xdg-user-dirs
    pkgs.unclutter # hide mouse
    # Graphics
    pkgs.scrot
    pkgs.i3blocks-gaps
    pkgs.arandr
    pkgs.networkmanagerapplet
    # libvirt
    pkgs.virt-manager
    pkgs.qemu
    pkgs.kvm
    pkgs.spice
    pkgs.spice-gtk
    pkgs.spice-up
    # Steam
    pkgs.steam-run-native
    # Audio
    pkgs.audacity
    # Images
    pkgs.feh
    pkgs.gimp
    pkgs.drawing
    # Web
    pkgs.firefox-bin
    pkgs.google-chrome
    pkgs.qutebrowser
    pkgs.zoom-us
    pkgs.spotify
    pkgs.keepassxc
    pkgs.kpcli
    pkgs.wget
    # Communication
    pkgs.xcompmgr # for zoom annotation
    pkgs.discord
    # Gaming
    pkgs.minecraft
    # ----------------------------Dev------------------------
    # General
    pkgs.git
    pkgs.neovim
    # Emacs
    pkgs.libvterm
    pkgs.pandoc
    pkgs.ripgrep
    pkgs.irony-server # c-type language autocomplete
    pkgs.texlive.combined.scheme-full # latex
    pkgs.ispell
    pkgs.readline
    # nixlang
    pkgs.rnix-lsp
    # General C-langs
    pkgs.cmake
    pkgs.gnumake
    pkgs.libtool
    pkgs.SDL
    pkgs.ccls
    pkgs.gcc
    pkgs.gdb
    pkgs.bear
    # Python
    python-with-my-packages
    # FPL team generator
    pkgs.cbc
  ];
  # Config files
  home.file.".emacs".source = ./emacs/init.el;
  home.file.".folders".source = ./emacs/folders;
  home.file.".config/qutebrowser/config.py".source = ./gui/qutebrowser/config.py;
  # Make that use python readline apps use vi mode
  programs.readline.extraConfig = ''
    set editing-mode vi
    set keymap vi
    set convert-meta on
    '';
}

  # TODO - make shortcuts to switch to, ex, firefox window rather than reopening it in current
