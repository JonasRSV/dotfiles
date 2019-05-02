#XDG_CONFIG_HOME = "~/.config"
#export LC_ALL=en_US.UTF-8  
#export LANG=en_US.UTF-8

#export LANG="en_US.ISO8859-1"
#export LC_COLLATE="en_US.ISO8859-1"
#export LC_CTYPE="en_US.ISO8859-1"
#export LC_MESSAGES="en_US.ISO8859-1"
#export LC_MONETARY="en_US.ISO8859-1"
#export LC_NUMERIC="en_US.ISO8859-1"
#export LC_TIME="en_US.ISO8859-1"
#export LC_ALL=

export XCODE_VERSION="Xcode 9.2"

export GOPATH="$HOME/go"
PATH=$PATH:~/go/bin/



. /usr/local/etc/profile.d/z.sh
source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"

# JENV
#export JENV_ROOT=/usr/local/opt/jenv
export JAVA_HOME="/Library/Java/JavaVirtualMachines/openjdk-11.0.2.jdk/Contents/Home"

bindkey -v

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward

function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
    RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
    zle reset-prompt
}

function lazy-push {
  message=$(curl https://whatthecommit.com/ | cat | grep '<p>' | cut -c 4-)
  git add . && git commit -m $message && git push
}

zle -N zle-line-init
zle -N zle-keymap-select
export KEYTIMEOUT=1

alias vim="nvim"
alias gitYolo="git add * && git commit -m 'yolomit' && git push -u origin --force"
alias gitCba=lazy-push
alias find=fd
alias memory=ncdu

## entr command to monitor file changes http://www.entrproject.org/

alias top="htop"

cr() {
  gcc $1 -o $1.bin && ./$1.bin
}


rg++() {
    g++ -Dpp -g -Wall -Wconversion -fsanitize=address,undefined -std=c++11 "$@"
}

unalias z
z() {
  if [[ -z "$*" ]]; then
    cd "$(_z -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
  else
    _last_z_args="$@"
    _z "$@"
  fi
}

zz() {
  cd "$(_z -l 2>&1 | sed 's/^[0-9,.]* *//' | fzf -q "$_last_z_args")"
}



vf() {
  vim $(fzf --preview='head -$LINES {}')
}


ezh() {
  vim ~/.zshrc
}

szh() {
  source ~/.zshrc
}

evi() {
  vim ~/.vimrc
}

ksub() {
  C=`basename $(pwd)`
  S="$(pwd)/submission.csv" 
  kaggle competitions submit -c $C -f $S -m $1
}

kscore() {
  C=`basename $(pwd)`
  kaggle competitions submissions -c $C
}

fs() {
	local -r fmt='#{session_id}:|#S|(#{session_attached} attached)'
	{ tmux display-message -p -F "$fmt" && tmux list-sessions -F "$fmt"; } \
		| awk '!seen[$1]++' \
		| column -t -s'|' \
		| fzf -q '$' --reverse --prompt 'switch session: ' -1 \
		| cut -d':' -f1 \
		| xargs tmux switch-client -t
}

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/jonval/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/jonval/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/jonval/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/jonval/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
