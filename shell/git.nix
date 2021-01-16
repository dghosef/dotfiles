{config, lib, pkgs, ...}:
{
  programs.git = {
    enable = true;
    userName = "dghosef";
    userEmail = "jdtan638@gmail.com";
    ignores = [ "*~undo-tree~" ];

  };
}
