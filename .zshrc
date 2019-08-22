
echo "$(cat .landing)"


autoload -U promptinit; promptinit
prompt pure

alias python="python3"
alias pip="pip3"

source /home/jonas/plugins/z.sh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


function lazy-push {
  message=$(curl https://whatthecommit.com/ | cat | grep '<p>' | cut -c 4-)
  git add . && git commit -m $message && git push
}


alias vim="nvim"
alias gitYolo="git add * && git commit -m 'yolomit' && git push -u origin --force"
alias gitCba=lazy-push
alias find=fd
alias memory=ncdu

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

