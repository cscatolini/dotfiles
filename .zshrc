export KOPS_STATE_STORE=s3://kubernetes-stag-config
export EDITOR=emacs
# Path to your oh-my-zsh installation.
export ZSH=/Users/cscatolini/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="random"
#ZSH_THEME="kolo"
#ZSH_THEME="norm"
#ZSH_THEME="robbyrussell"
ZSH_THEME="sorin"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git brew npm emoji)

# User configuration

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi


# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

source ~/.oh-my-zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 
. `brew --prefix`/etc/profile.d/z.sh

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
alias zshconfig="emacs -nw ~/.zshrc"
alias ohmyzsh="subl ~/.oh-my-zsh"

# environment variables
export GOPATH=~/Code/go
export RBENV_ROOT=~/.rbenv
eval "$(rbenv init -)"
#export PATH=/usr/local/bin:$PATH
export PATH=$PATH:/Applications/Postgres.app/Contents/Versions/9.5/bin
export PATH=$PATH:$GOPATH/bin
export NVM_DIR="/Users/cscatolini/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
# Calling nvm use automatically in a directory with a .nvmrc file
autoload -U add-zsh-hook
load-nvmrc() {
    if [[ -f .nvmrc && -r .nvmrc ]]; then
	nvm use
    elif [[ $(nvm version) != $(nvm version default)  ]]; then
	echo "Reverting to nvm default version"
	nvm use default
    fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc
export WORKON_HOME=$HOME/.virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
export PIP_REQUIRE_VIRTUALENV=false
source <(awless completion zsh)
## Support for bash
#PROMPT_COMMAND='prompt'

## Mirrored support for zsh. See: https://superuser.com/questions/735660/whats-the-zsh-equivalent-of-bashs-prompt-command/735969#735969 
#precmd() { eval "$PROMPT_COMMAND" }

#function prompt()
#{
#    if [ "$PWD" != "$MYOLDPWD" ]; then
#        MYOLDPWD="$PWD"
#        test -e .venv && workon `cat .venv`
#    fi
#}

# useful alias
alias edit="emacs -nw"
alias gti="git"
alias gpg='gpg1'
alias mkae="make"
alias maek="make"
alias ll="ls -lah"
alias here="atom ."
alias redisserver="cd ~ && redis-server"
alias waws="workon aws2"
alias wipython="workon ipython"
alias wpgcli="workon pgcli"
alias code="cd ~/Code"
alias cat="ccat"
alias cta="ccat"
alias ecrlogin="waws && aws ecr get-login --no-include-email | bash && deactivate"
alias lala="echo 💩"

alias cdkc='docker rm $(docker ps -qa --no-trunc --filter "status=exited")'
alias cdkca='docker rm -f $(docker ps -a -q)'
alias cdki='docker rmi $(docker images --filter "dangling=true" -q --no-trunc)'
alias cdkia='docker rmi -f $(docker images -q)'
alias cdkn='docker run -v /var/run/docker.sock:/var/run/docker.sock -v /var/lib/docker:/var/lib/docker --rm martin/docker-cleanup-volumes'
alias cdall='(cdkca || true) && (cdki || true) && (cdkn || true)'
alias fix-spotify="diskutil erasevolume HFS+ 'RAM Disk' `hdiutil attach -nomount ram://409600`"
alias simulator="open -n /Applications/Xcode.app/Contents/Developer/Applications/Simulator.app"

# kube aliases
alias kube-prod="kubectl --context prod"
alias kube-stag="kubectl-stag --context stag"

# kill using grep
killGrep() {
    kill $(ps aux | grep $1 | awk '{print $2}')
}
alias kill-grep=killGrep

# replace using git grep and sed
gitReplace() {
    git grep -l '$1' | xargs sed -i '' -e 's/$1/$2/g'
}
# changelog since tag
gitChangelog() {
    git log --oneline --no-merges $1..HEAD
}
# commit affected files
gitAffectedFiles() {
    git diff-tree --no-commit-id --name-only -r $1
}
nodeTimestamp() {
    node -e "var a  = new Date(); console.log(a.toISOString().split('.')[0].split('-').join('').split('T').join('').split(':').join(''));"
}
nodeEpoch() {
    node -e "var a = new Date(); console.log(Math.round(a.getTime() / 1000));"
}
pgbadgerGenerateHtml() {
    pgbadger --prefix "%t:%r:%u@%d:[%p]:" --outfile /tmp/pgbadger.html $1 && open /tmp/pgbadger.html
}
alias greplace=gitReplace
alias gbrdelete="git branch | grep -v "master" | xargs git branch -D"
alias gleaderboards="git shortlog -sn"
alias gbranches='git for-each-ref --count=10 --sort=-committerdate refs/heads/ --format="%(refname:short)"'
alias gchangelog=gitChangelog
alias gFiles=gitAffectedFiles
alias timestamp=nodeTimestamp
alias epoch=nodeEpoch
alias pghtml=pgbadgerGenerateHtml
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/cscatolini/.sdkman"
[[ -s "/Users/cscatolini/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/cscatolini/.sdkman/bin/sdkman-init.sh"

eval "$(thefuck --alias)"
export PATH="/usr/local/bin:/usr/local/opt/gettext/bin:$PATH"
export PATH=/usr/local/bin:/usr/local/sbin:~/bin:$PATH
