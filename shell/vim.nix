{pkgs, config, ...}:
{
  programs.vim.enable = true;
  programs.vim.extraConfig = ''
    set autoindent
    set relativenumber number
    syntax on
    filetype indent on
  '';
}
