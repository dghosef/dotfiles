{ config, lib, pkgs, ... }:

let 
  mod = "Mod4";
  fg  = "#fdf6e3";
  bg  = "#002b36";
  bg_border = "#073642";
  fg_border = "#859900";
  status_bg  = "#073642";
in {
  xsession.windowManager.i3 = {
    enable = true;
    config = {
      # =============General Settings========
      workspaceAutoBackAndForth = false;
      # =============Aesthetic==========
      # border
      window.border = 4;
      colors.focused          = { border = fg_border; background = fg_border; text = fg_border; indicator = fg_border; childBorder = fg_border; };
      colors.focusedInactive  = { border = bg_border; background = bg_border; text = bg_border; indicator = bg_border; childBorder = bg_border; };
      colors.unfocused        = { border = bg_border; background = bg_border; text = bg_border; indicator = bg_border; childBorder = bg_border; };
      colors.urgent           = { border = fg_border; background = fg_border; text = fg_border; indicator = fg_border; childBorder = fg_border; };

      startup = [
        { command = "feh --bg-fill ${./top.png}"; notification = false; always = true; }
        { command = "dropbox start"; notification = false; always = true; }
        { command = "nm-applet"; notification = false; always = true; }
        { command = "pactl load-module module-switch-on-connect"; notification = false; always = true; }
        { command = "xcompmgr -c -l0 -t0 -r0 -o.00"; notification = false; always = true; }
      ];

      bars = [
        {
	  position = "top";
          fonts = ["Roboto Mono 10" "Awesome 15"];
          statusCommand = "i3blocks -c ${./blocks.conf}";
	  colors.background = status_bg;
	  colors.separator ="#2AA198";
	  colors.focusedWorkspace = {border ="#2aa198"; background = status_bg; text = "#2aa198";};
	  colors.inactiveWorkspace = {border = status_bg; background = status_bg; text = "#2aa198";};
          extraConfig = "separator_symbol |";
        }
      ];

      fonts = ["Roboto Mono, FontAwesome 10"];

      gaps.inner = 5;
      floating.titlebar = false;
      window.titlebar = false;

      # ===================Keybindings============
      modifier = mod;
      keybindings = lib.mkOptionDefault {

        # ------------Lock----------------
        "${mod}+g" = "exec --no-startup-id i3lock --color=#000000 --show-failed-attempts";

        # -------------Navigation-----------
        # Focus
        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";
  
        # Move
        "${mod}+Shift+h" = "move left";
        "${mod}+Shift+j" = "move down";
        "${mod}+Shift+k" = "move up";
        "${mod}+Shift+l" = "move right";
  
        # Basic utilities
        # volume buttons
        "${mod}+equal" = "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +10%";
        "${mod}+minus" = "exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -10%";

        # --------------------Spotify------------------
        "${mod}+semicolon" = "exec --no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause";
        "${mod}+apostrophe" = "exec --no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next";
        "${mod}+Shift+semicolon" = "exec --no-startup-id dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous";
        # ------------------App Launching----------------
  
        # Application launcher
        "${mod}+r" = "exec --no-startup-id rofi -show run";
  
        # Emacs - ws 1
        "${mod}+Return" = "workspace number 1";
        "${mod}+Shift+Return" = "exec --no-startup-id emacs";
  
        # Browsers - ws 2
        "${mod}+b" = "workspace number 2";
        "${mod}+Shift+b" = "exec --no-startup-id qutebrowser";
        "${mod}+Shift+f" = "exec --no-startup-id firefox";
        
        # Discord - ws 3
        "${mod}+d" = "workspace number 3";
        "${mod}+Shift+d" = "exec --no-startup-id Discord";
  
        # Termite - ws 4
        "${mod}+t" = "workspace number 4";
        "${mod}+Shift+t" = "exec --no-startup-id termite";
  
        # Zoom - ws 5
        "${mod}+z" = "workspace number 5";
        "${mod}+Shift+z" = "exec --no-startup-id zoom-us";
  
        # Music - ws 6
        "${mod}+m" = "workspace number 6";
        "${mod}+Shift+m" = "exec --no-startup-id spotify";
  
        # Password Manager - ws 7
        "${mod}+p" = "workspace number 7";
        "${mod}+Shift+p" = "exec --no-startup-id keepassxc";

        # Scratchpad workspace- ws 10
        "${mod}+s" = "workspace number 10";
      };
    };
  };
}

