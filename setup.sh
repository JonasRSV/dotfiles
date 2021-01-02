
fg_black="$(tput setaf 0)"
fg_red="$(tput setaf 1)"
fg_green="$(tput setaf 2)"
fg_yellow="$(tput setaf 3)"
fg_blue="$(tput setaf 4)"
fg_magenta="$(tput setaf 5)"
fg_cyan="$(tput setaf 6)"
fg_white="$(tput setaf 7)"
reset="$(tput sgr0)"

echo "${fg_red}$ This setup file is incomplete ${reset}"

if ! command -v git 
then 
  echo "${fg_red}git not found Please install it${fg_reset}"
  echo "Try:"
  echo "       sudo apt-get install git"
  exit 1
fi

echo "${fg_green}$ Setting up for zsh ${reset}"

if ! command -v zsh 
then 
  echo "${fg_red}zsh not found Please install it${fg_reset}"
  echo "Try:"
  echo "       sudo apt-get install zsh"
  exit 1
fi

ZSHRC_PATH=$HOME/.zshrc
if [ ! -f $ZSHRC_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/.zshrc -> $ZSHRC_PATH ${reset}"
  ln -s $HOME/dotfiles/.zshrc $ZSHRC_PATH
else
  echo "${fg_magenta} $ZSHRC_PATH already exists - skipping... ${reset}"
fi

# Pure https://github.com/sindresorhus/pure
ZSH_PROMPT_ASYNC_PATH=/usr/local/share/zsh/site-functions/async
if [ ! -f $ZSH_PROMPT_ASYNC_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/zsh-scripts/pure_async.zsh -> $ZSH_PROMPT_ASYNC_PATH ${reset}"
  sudo ln -s $HOME/dotfiles/zsh-scripts/pure_async.zsh $ZSH_PROMPT_ASYNC_PATH
else
  echo "${fg_magenta} $ZSH_PROMPT_ASYNC_PATH already exists - skipping... ${reset}"
fi

ZSH_PROMPT_PURE_PATH=/usr/local/share/zsh/site-functions/prompt_pure_setup
if [ ! -f $ZSH_PROMPT_PURE_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/zsh-scripts/pure_pure.zsh -> $ZSH_PROMPT_PURE_PATH ${reset}"
  sudo ln -s $HOME/dotfiles/zsh-scripts/pure_pure.zsh $ZSH_PROMPT_PURE_PATH 
else
  echo "${fg_magenta} $ZSH_PROMPT_PURE_PATH already exists - skipping... ${reset}"
fi

if ! command -v fzf 
then 
  echo "${fg_red}fzf not found Please install it${fg_reset}"
  echo "Try:"
  echo "       git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && ~/.fzf/install"
  exit 1
fi


PROFILE_PATH=$HOME/.profile
if [ ! -f $PROFILE_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/.profile -> $PROFILE_PATH ${reset}"
  sudo ln -s $HOME/dotfiles/.profile $PROFILE_PATH 
else
  echo "${fg_magenta} $PROFILE_PATH already exists - skipping... ${reset}"
fi

echo "${fg_green}$ Setting zsh to default ${reset}"
chsh -s $(which zsh)


echo "${fg_green} Setting up for neovim ${reset}"

if ! command -v nvim && ! command -v nvim.appimage
then 
  echo "${fg_red}nvim not found Please install it${fg_reset}"
  echo "Try:"
  echo "       https://github.com/neovim/neovim/releases"
  exit 1
fi

NEOVIMRC_PATH=$HOME/.config/nvim/init.vim 
if [ ! -f $NEOVIMRC_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/init.vim -> $NEOVIMRC_PATH ${reset}"
  ln -s $HOME/dotfiles/init.vim $NEOVIMRC_PATH 
else
  echo "${fg_magenta} $NEOVIMRC_PATH already exists - skipping... ${reset}"
fi

VIMRC_PATH=$HOME/.vimrc
if [ ! -f $VIMRC_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/.vimrc -> $VIMRC_PATH ${reset}"
  ln -s $HOME/dotfiles/.vimrc $VIMRC_PATH 
else
  echo "${fg_magenta} $VIMRC_PATH already exists - skipping... ${reset}"
fi

VIM_PATH=$HOME/.vim
if [ ! -f $VIM_PATH/colors/mycolo.vim ]; then
  echo "${fg_green} Linking $HOME/dotfiles/.vim -> $VIM_PATH ${reset}"
  ln -s $HOME/dotfiles/.vim $VIM_PATH 
else
  echo "${fg_magenta} $VIM_PATH already exists - skipping... ${reset}"
fi


VIM_VUNDLE_PATH=$HOME/.vim/bundle/Vundle.vim
if [ ! -f $VIM_VUNDLE_PATH/README.md ]; then
  echo "${fg_green} Installing Vundle (vim plugin manager) at $VIM_VUNDLE_PATH ${reset}"
  git clone https://github.com/VundleVim/Vundle.vim.git $VIM_VUNDLE_PATH
else
  echo "${fg_magenta} $VIM_VUNDLE_PATH already exists - skipping... ${reset}"
fi 


echo "${fg_green}$ Setting up for tmux ${reset}"

if ! command -v tmux 
then 
  echo "${fg_red}tmux not found Please install it${fg_reset}"
  echo "Try:"
  echo "       sudo apt-get install tmux"
  exit 1
fi


TMUXRC_PATH=$HOME/.tmux.conf
if [ ! -f $TMUXRC_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/.tmux.conf -> $TMUXRC_PATH ${reset}"
  ln -s $HOME/dotfiles/.tmux.conf $TMUXRC_PATH
else
  echo "${fg_magenta} $TMUXRC_PATH already exists - skipping... ${reset}"
fi 

echo "${fg_green}$ Setting up feh (Background image setter) ${reset}"

if ! command -v feh
then 
  echo "${fg_red}feh not found Please install it${fg_reset}"
  echo "try:"
  echo "       sudo apt-get install feh"
  exit 1
fi


echo "${fg_green}$ Setting up for xmobar ${reset}"

if ! command -v xmobar 
then 
  echo "${fg_red}xmobar not found Please install it${fg_reset}"
  echo "I am using 0.37 - Install from source here:"
  echo "       https://github.com/jaor/xmobar"
  echo "${fg_magenta} Remember to install with all extensions ${fg_reset}"
  exit 1
fi

XMOBAR_PATH=$HOME/.xmobarrc
if [ ! -f $XMOBAR_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/.xmobarrc -> $XMOBAR_PATH ${reset}"
  ln -s $HOME/dotfiles/.xmobarrc $XMOBAR_PATH
else
  echo "${fg_magenta} $XMOBAR_PATH already exists - skipping... ${reset}"
fi 

echo "${fg_green}$ Setting up for xmonad ${reset}"

if ! command -v xmonad 
then 
  echo "${fg_red}Xmonad not found Please install it${fg_reset}"
  echo "I am using 0.15 - See installation instructions  here:"
  echo "       https://github.com/xmonad/xmonad"
  echo "${fg_magenta} Make sure the version is >= 0.15 and make sure to install contrib ${fg_reset}"
  exit 1
fi

XMONAD_PATH=$HOME/.xmonad/xmonad.hs
if [ ! -f $XMONAD_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/xmonad.hs -> $XMONAD_PATH ${reset}"
  ln -s $HOME/dotfiles/xmonad.hs $XMONAD_PATH

  echo "${fg_green}$ You will have to recompile xmonad once this script is done.. ${reset}"
else
  echo "${fg_magenta} $XMONAD_PATH already exists - skipping... ${reset}"
fi 

XMONAD_XSESSION_PATH=/usr/share/xsessions/xmonad.desktop
if [ ! -f $XMONAD_XSESSION_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/xmonad.desktop -> $XMONAD_XSESSION_PATH ${reset}"
  sudo ln -s $PWD/xmonad.desktop /usr/share/xsessions/xmonad.desktop
else
  echo "${fg_magenta} $XMONAD_XSESSION_PATH already exists - skipping... ${reset}"
fi 

echo "${fg_green}$ Creating .xsessionrc that will start xmonad ${reset}"

XSESSIONRC_PATH=$HOME/.xsessionrc
if [ ! -f $XSESSIONRC_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/.xsessionrc -> $XSESSIONRC_PATH ${reset}"
  ln -s $HOME/dotfiles/.xsessionrc $XSESSIONRC_PATH

  echo "${fg_green}$ If something breaks when you restart now - just enter a tty and comment out the contents of this create file ${reset}"
else
  echo "${fg_magenta} $XSESSIONRC_PATH already exists - skipping... ${reset}"
  echo "${fg_magenta} Make sure the file launches xmonad.. ${reset}"
fi 

echo "${fg_green}$ Setting up for eww ${reset}"

if ! command -v eww 
then 
  echo "${fg_red}eww not found Please install it${fg_reset}"
  echo "See installation instructions here:"
  echo "       https://github.com/elkowar/eww"
  exit 1
fi

EWW_PATH=$HOME/.config/eww
if [ ! -f $EWW_PATH/eww.xml ]; then
  echo "${fg_green} Linking $HOME/dotfiles/eww -> $EWW_PATH ${reset}"
  ln -s $HOME/dotfiles/eww $EWW_PATH
else
  echo "${fg_magenta} $EWW_PATH already exists - skipping... ${reset}"
fi 


echo "${fg_green}$ Setting up for nerdfonts ${reset}"
NERD_FONTS=$(fc-list | grep Nerd)
if [ -z $NERD_FONTS ] 
then 
  echo "${fg_red}nerds fonts not found Please install it${fg_reset}"
  echo "Try:"
  echo "       git clone --depth 1 https://github.com/ryanoasis/nerd-fonts.git ~/.nerd-fonts && ~/.nerd-fonts/install.sh"
  echo "${fg_red} This will take a while since it is a lot of fonts${fg_reset}"
  exit 1
else
  echo "${fg_magenta} Nerdfonts already exists - skipping... ${reset}"
fi

echo "${fg_green}$ Setting up picom (for nice corners in WM) ${reset}"
if ! command -v picom 
then 
  echo "${fg_red}picom not found Please install it${fg_reset}"
  echo "Build it from:"
  echo "       https://github.com/yshui/picom"
  exit 1
fi


echo "${fg_green}$ Setting up GDM3 ${reset}"
echo "${fg_green}$ Running 'sudo dpkg-reconfigure gdm3' ${reset}"
sudo dpkg-reconfigure gdm3

PICOM_PATH=$HOME/.config/picom.conf

if [ ! -f $PICOM_PATH ]; then
  echo "${fg_green} Linking $HOME/dotfiles/picom.conf -> $PICOM_PATH ${reset}"
  ln -s $HOME/dotfiles/picom.conf $PICOM_PATH
else
  echo "${fg_magenta} $PICOM_PATH already exists - skipping... ${reset}"
fi 
