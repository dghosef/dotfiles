{ config, pkgs, lib, ... }:
{
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
  # Use nvidia drivers
  services.xserver.videoDrivers = [ "nvidia" ];
}
