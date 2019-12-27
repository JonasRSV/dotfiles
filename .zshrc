export LOCAL_IP=$(ifconfig wlp58s0 | grep -m 1 inet | awk '{print $2}')
export PUBLIC_IP=$(dig @ns1-1.akamaitech.net ANY whoami.akamai.net +short)

echo "$(envsubst < ~/.landing)"

for file in ~/dotfiles/scripts/*; do source $file; done

autoload -U promptinit; promptinit

prompt pure

# pavucontrol (For pulse audio!!) bluetoothctl to connect to headset!! (pacmd for other stuff!!)

source ~/dotfiles/zsh-autosuggestions.zsh
bindkey '^n' autosuggest-accept
ZSH_AUTO_SUGGEST_STRATEGY=(history completion)


source /home/jonas/plugins/z.sh

# For completion and history search to work nice
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


function lazy-push {
  message=$(curl https://whatthecommit.com/ | cat | grep '<p>' | cut -c 4-)
  git add . && git commit -m $message && git push
}

function cr {
  c++ -Wall $1 -o cr_run_file.out && ./cr_run_file.out && rm cr_run_file.out
}

function crp {
  c++ -O2 -Wall $1 -o cr_run_file.out && ./cr_run_file.out && rm cr_run_file.out
}

function hrp {
  ghc -O2 -o haskell-script $1 && ./haskell-script $2 $3 $4 $5 && rm haskell-script

}
function hr {
  ghc -o haskell-script $1 && ./haskell-script $2 $3 $4 $5 && rm haskell-script
}


alias ipy="ipython --profile J"
alias python="python3"
#alias pip="pip3"
alias l='ls -a --color=tty'
alias ls='ls --color=tty'
alias grep='grep --color=auto '
alias go-home="sudo openvpn --config ~/pivpns/jonas.ovpn"
alias vim="nvim"
alias gitYolo="git add * && git commit -m 'yolomit' && git push -u origin --force"
alias gitCba=lazy-push
#alias find=fd
alias memory=ncdu
alias xd=xdg-open

## entr command to monitor file changes http://www.entrproject.org/

alias top="htop"

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


fs() {
	local -r fmt='#{session_id}:|#S|(#{session_attached} attached)'
	{ tmux display-message -p -F "$fmt" && tmux list-sessions -F "$fmt"; } \
		| awk '!seen[$1]++' \
		| column -t -s'|' \
		| fzf -q '$' --reverse --prompt 'switch session: ' -1 \
		| cut -d':' -f1 \
		| xargs tmux switch-client -t
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

if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
    tmux
    #tmux attach -t default || tmux new -s default
fi


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/jonas/google-cloud-sdk/path.zsh.inc' ]; then . '/home/jonas/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/jonas/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/jonas/google-cloud-sdk/completion.zsh.inc'; fi

export EDITOR=vim
export VISUAL=vim


local-port-scan() {
  sudo nmap -vv --reason --open --privileged -sS -F ${LOCAL_IP%.*}.0/24
}
