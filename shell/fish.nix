{config, lib, pkgs, ...}:
{
  programs.fish = {
    enable = true;
    interactiveShellInit = ''fish_vi_key_bindings
                           set CS107E ~/cs107e_home/cs107e.github.io/cs107e
                           set PATH $PATH:$CS107E/bin'';
  };
}
