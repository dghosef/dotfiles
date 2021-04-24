# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
#
#
#
# ###########################################################
# # INSTALL HOME-MANAGER USING INSTRUCTIONS FOR FULL SYSTEM #
# # AS PART OF THE INSTALL PROCESS - CONSIDERED ESSENTIAL   #
# ###########################################################

{ config, pkgs, lib, ... }:
with pkgs;
{
  # -------------------System stuff-------------------
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # ---------------Boot------------------
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # ----------------Networking-------------------
  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.iwd.enable = true;
  networking.wireless.enable = false;
  networking.networkmanager.enable = true;
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  # CHANGE wlan0 TO WHATEVER ELSE IT SHOULD BE(wlo1)
  networking.interfaces.wlo1.useDHCP = true;
  # Enable the OpenSSH daemon. Disable root login
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";



  # -----------------Location/Keyboard-----------------
  # Set your time zone.
  time.timeZone = "America/Los_Angeles";


  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  # Configure keymap in X11
  services.xserver.layout = "us";
  # Para mi tarea de español
  services.xserver.xkbOptions = "compose:ralt";


  # ------------------Hardware Configuration----------------

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];
  # Enable scanning
  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.hplipWithPlugin ];
  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.extraConfig = "unload-module module-switch-on-port-available";
  # hardware.pulseaudio.package = pkgs.pulseaudioFull;
  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;
  # Disable shutdown in power key
  services.logind.extraConfig = ''HandlePowerKey=suspend'';
  services.logind.lidSwitch = "ignore";
  # Enable bluetooth(really spotty right now)
  # hardware.bluetooth.enable = true;
  # Switch to headphones by default
  boot.extraModprobeConfig = ''
    alias snd-card-0 snd-hda-intel
    alias sound-slot-0 snd-hda-intel
    options snd-hda-intel model=dell-m4-1 enable_msi=1
'';


  # ------------------User Configuration----------------
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.dghosef = {
    isNormalUser = true;
    home = "/home/dghosef";
    extraGroups = [ "wheel" # Enable ‘sudo’ for the user.
                    "scanner" "lp"
                    "audio" "sound"
                    "networkmanager" 
                    "libvirtd" # virt-namager
                    "dialout" # usb serial -107e
                  ]; 
  };
  programs.fish.enable = true;
  users.extraUsers.dghosef.shell = pkgs.fish;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # ------------------Individual Packages---------------
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # Allow non-free license packages
  nixpkgs.config.allowUnfree = true;
  environment.pathsToLink = [ "/libexec" ]; # required for i3blocks
  programs.steam.enable = true;
  virtualisation.libvirtd.enable = true;
  programs.dconf.enable = true;
  environment.systemPackages = with pkgs; [
    lxappearance
    acpi
    lxqt.pavucontrol-qt
  ];
  environment.variables.EDITOR = "termite";
  # ---------------------------X-Server-----------------------
  services.xserver = {
    enable = true;
    # ======================HARDWARE-SPECIFIC TO MY ZENBOOK DUO - COMMENT OUT ON OTHER SYSTEMS=======================
    videoDrivers = [ "nvidia" ];
    # ======================HARDWARE-SPECIFIC TO MY ZENBOOK DUO - COMMENT OUT ON OTHER SYSTEMS=======================


    desktopManager = {
      xterm.enable = false;
    };

    displayManager = {
      defaultSession = "none+i3";
      lightdm = {
        enable = true;
        background = ./lock.jpg;
      };
    };

    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
  };
  # -------------------fonts------------------
  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
    dina-font
    fira-code
    font-awesome
    fira-code-symbols
    liberation_ttf
    material-icons
    material-design-icons
    mplus-outline-fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    proggyfonts
    roboto-mono
    unifont
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
