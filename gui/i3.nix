{ config, lib, pkgs, ... }:

let 
  mod = "Mod4";
in {
  xsession.windowManager.i3 = {
    enable = true;
    config = {
      workspaceAutoBackAndForth = false;
      # border
      gaps.smartBorders = "on";
      window.border = 4;
      startup = [
        { command = "feh --bg-fill ${../nixos/lock.jpg}"; notification = false; always = true; }
        { command = "dropbox start"; notification = false; always = true; }
        { command = "xcompmgr -c -l0 -t0 -r0 -o.00"; notification = false; always = true; }
        { command = "setxkbmap -option caps:escape -option ralt:compose"; notification = false; always = true; }
      ];

      bars = [
        {
          position = "bottom";
          workspaceButtons = false;
          fonts = {
            names = ["Roboto Mono" "FontAwesome"];
            size = 10.0;
          };
          statusCommand = "i3blocks -c ${./blocks.conf}";
          extraConfig = "separator_symbol |";
          trayOutput = "None";
        }
      ];

      fonts = {
        names = ["Roboto Mono" "FontAwesome"];
      };

      floating.titlebar = false;
      window.titlebar = false;

      # ===================Keybindings============
      modifier = mod;
      keybindings = lib.mkOptionDefault {

        # ------------lock and screenshot----------------
        "${mod}+g" = "exec --no-startup-id i3lock --color=#000000 --show-failed-attempts";
        "${mod}+x" = "--release exec --no-startup-id scrot -s '%Y-%m-%d_$wx$h_scrot.png' -e 'mv $f ~/Desktop/'";

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
        "${mod}+Shift+f" = "exec --no-startup-id firefox";
        "${mod}+Shift+b" = "exec --no-startup-id firefox";
        
        # Discord - ws 3
        "${mod}+d" = "workspace number 3";
        "${mod}+Shift+d" = "exec --no-startup-id firefox --new-window discord.com/app";
        
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

        # Scratchpad
        "${mod}+Shift+BackSpace" = "move scratchpad";
        "${mod}+backslash" = "scratchpad show";

        # Rebind mod + w to not to the tabbed thing
        "${mod}+w" = "workspace number 9";
      };
    };
  };
}
