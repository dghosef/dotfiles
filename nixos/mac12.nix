{ config, pkgs, lib, ... }:
{
  networking.networkmanager.enable = true;
  # CHANGE wlan0 TO WHATEVER ELSE IT SHOULD BE(wlo1) on other systems
  networking.interfaces.wlo1.useDHCP = true;
  # Disable shutdown in power key
  services.logind.extraConfig = ''HandlePowerKey=suspend'';
  services.logind.lidSwitch = "ignore";
  # Enable bluetooth(really spotty right now)
  # Switch to headphones by default
  boot.extraModprobeConfig = ''
    alias snd-card-0 snd-hda-intel
    alias sound-slot-0 snd-hda-intel
    options snd-hda-intel model=dell-m4-1 enable_msi=1
'';
  # Use nvidia drivers
  services.xserver.videoDrivers = [ "nvidia" ];
}
