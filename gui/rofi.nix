{config, pkgs, ...}:
let 
bg = "#073642";
fg = "#fdf6e3";
in
{
  programs.rofi.enable = true;
  programs.rofi.colors = {
    window = {
      background = bg;
      border = bg;
      separator = fg;
    };
    rows = {
      normal = {
        background = bg;
        foreground = fg;
        backgroundAlt = bg;
        highlight = {
          background = "#586e75";
          foreground = fg;
        };
      };
    };
  };
}
