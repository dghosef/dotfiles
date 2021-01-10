{config, lib, pkgs, ...}:
{
  services.screen-locker.enable = true;
  services.screen-locker.inactiveInterval = 5;
  services.screen-locker.lockCmd = "i3lock --color=002b36 --show-failed-attempts";
}
