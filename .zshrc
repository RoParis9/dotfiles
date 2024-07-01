# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH="$HOME/.oh-my-zsh"


ZSH_THEME="spaceship"

plugins=(git sudo docker web-search history zsh-autosuggestions asdf zsh-syntax-highlighting )

source $ZSH/oh-my-zsh.sh


alias v="nvim"

alias w="nitrogen --set-zoom-fill --random ~/Media/wallpapers"

#Nodejs
export PATH=/home/rodrigo/nodejs/bin:$PATH

#asdf
export PATH=$HOME/.asdf/asdf.sh:$PATH 

# Php composer
export PATH="$PATH:$HOME/.config/composer/vendor/bin"


# Load Angular CLI autocompletion.
source <(ng completion script)

export PATH="$PATH:$HOME/.asdf/installs/nodejs/20.11.0/lib"
