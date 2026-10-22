{ config, pkgs, inputs, ... }:

{
  imports = [
    # Flake input (path:/home/rodrigo/.dotfiles) — pure-eval safe, no absolute imports.
    inputs.my-dotfiles.homeModules.default
  ];

  dotfiles.enable = true;
  # LSP packages only; keep the existing ~/.config/nvim → .dotfiles/nvim symlink.
  dotfiles.neovim.enable = true;

  home.username = "rodrigo";
  home.homeDirectory = "/home/rodrigo";
  home.stateVersion = "26.05";
  programs.bash = {
    enable = true;
  };

  programs.zsh = {
    enable = true;
    oh-my-zsh = {
      enable = true;
      plugins = [
      "git"
      "sudo"
      "z"
      "copypath"
      "copyfile"
      "docker"
      "docker-compose"
      "kubectl"
      "aws"
      ];
      theme = "robbyrussell";
    };

    plugins = [
    {
      name = "zsh-autosuggestions";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-autosuggestions";
        rev = "v0.7.1"; # You can pin a version or commit hash
        sha256 = "sha256-vpTyYq9ZgfgdDsWzjxVAE7FZH4MALMNZIFyEOBLm5Qo=";
      };
    }
    {
      name = "zsh-syntax-highlighting";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-syntax-highlighting";
        rev = "0.8.0";
        sha256 = "sha256-iJdWopZwHpSyYl5/FQXEW7gl/SrKaYDEtTH9cGP7iPo=";
      };
     }
    ];

    profileExtra = ''
        if [ -z "$WAYLAND_DISPLAY"] &&  [ "$XDG_VTNR" = 1 ]; then
           exec start-hyprland
        fi
   '';
  };


  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        disable_loading_bar = true;
        grace = 300; # 5-second grace period to move mouse before locking
        hide_cursor = true;
      };

      background = [
        {
          path = "/path/to/your/wallpaper.png"; # Change to your actual wallpaper path
          blur_passes = 2; # Blurs the background for an aesthetic look
          blur_size = 7;
        }
      ];

      input-field = [
        {
          size = "250, 60";
          outline_thickness = 2;
          dots_size = 0.2; 
          dots_spacing = 0.2;
          fade_on_empty = false;
        }
      ];
    };
  };


  # Install and configure hypridle (Great for saving your 48Wh battery!)
  services.hypridle = {
    enable = true;
    settings = {
      listener = [
        {
          timeout = 150;                                # 2.5 minutes
          on-timeout = "brightnessctl set 10%";         # Dim screen backlight
          on-resume = "brightnessctl set 100%";        # Restore screen backlight
        }
        {
          timeout = 300;                                # 5 minutes
          on-timeout = "loginctl lock-session";         # Trigger hyprlock via systemd
        }
        {
          timeout = 600;                                # 10 minutes
          on-timeout = "hyprctl dispatch dpms off";     # Turn off screen entirely
          on-resume = "hyprctl dispatch dpms on";       # Turn screen back on
        }
      ];
    };
  };
  programs.git = {
    enable = true;
    settings = {
      init.defaultBranch = "main";
      push.autoSetupRemote = true;
      pull.rebase = true;
      user.name = "Rodrigo Paris";
      user.email = "rocostaparis@hotmail.com";
    };
includes = [
      {
        condition = "gitdir:~/Oliver/";
	contents = {
	  user = {
	    name = "Rodrigo Paris";
	    email = "rodrigo.paris@oliver.app";
          };
        };
       }
    ]; 
  };
  home.packages = with pkgs; [
    emacsPackages.pbcopy
    emacsPackages.vterm
    libvterm
    neovim
    emacs
    code-cursor
    brave
    awscli2
    spotify
    docker-compose
    wofi
    bitwarden-desktop
    lxappearance
    papirus-icon-theme
    cameractrls
    gcc
    thunar
    rofi
    foliate
    zathura
    discord
    easyeffects
    chromium
    feh
    mpv
    telegram-desktop
    hyprshot
    flameshot
    dbeaver-bin
    gnome-themes-extra
    volantes-cursors
    # upwork requires a manual .deb download (nixpkgs cannot fetch it).
    # Re-add after prefetching the .deb from https://www.upwork.com/ab/downloads/os/linux/
    # upwork
  ];
}
