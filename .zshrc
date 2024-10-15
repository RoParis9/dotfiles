# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="spaceship"

plugins=(git sudo docker web-search history zsh-autosuggestions asdf zsh-syntax-highlighting )

source $ZSH/oh-my-zsh.sh

alias v="nvim"

alias w="nitrogen --set-zoom-fill --random ~/Media/wallpapers"
# Php composer
export PATH="$PATH:$HOME/.config/composer/vendor/bin"

#Cargo
export PATH="$HOME/.cargo/bin/cargo:$PATH"


export PATH=$HOME/Downloads/idea-IC-242.21829.142/bin:$PATH

# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
[[ ! -r '/home/rodrigo/.opam/opam-init/init.zsh' ]] || source '/home/rodrigo/.opam/opam-init/init.zsh' > /dev/null 2> /dev/null
# END opam configuration
#
export PATH=$HOME/.config/emacs/bin/doom:$PATH
export PATH="/home/rodrigo/.config/herd-lite/bin:$PATH"
export PHP_INI_SCAN_DIR="/home/rodrigo/.config/herd-lite/bin:$PHP_INI_SCAN_DIR"
