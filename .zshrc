# Path to your oh-my-zsh installation.
export ZSH=/Users/cscatolini/.oh-my-zsh

# Environment variables
export EDITOR=emacs
export PAGER="less -iMSx4FXRe"
export GOPATH=~/Code/go
export PATH=$PATH:$GOPATH/bin
export PATH="$HOME/.fastlane/bin:$PATH"
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel
export VIRTUALENVWRAPPER_SCRIPT=/usr/local/bin/virtualenvwrapper.sh
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
export NVM_NO_USE=false         # disable load node version from .nvmrc
export NVM_LAZY_LOAD=true

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="sorin"

plugins=(git brew zsh-nvm zsh-syntax-highlighting kubectl completion)
source $ZSH/oh-my-zsh.sh

# External sources
source /usr/local/bin/virtualenvwrapper_lazy.sh
autoload -U compinit && compinit

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# External software initialization
eval "$(rbenv init -)"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# For a full list of active aliases, run `alias`.

# software aliases
alias edit="emacs -nw"
alias cat="ccat"
alias generate-proto="protoc --go_out=plugins=grpc:. *.proto"

# typo aliases
alias car="cat"
alias gti="git"
alias mkae="make"
alias maek="make"
alias cta="cat"
alias sl="ls"

# shortcut alises
alias ll="ls -lah"
alias tfgo="cd ~/Code/go/src/github.com/topfreegames"
alias tfgolab="cd ~/Code/go/src/git.topfreegames.com/topfreegames"
alias code="cd ~/Code"
alias cpwd="pwd | pbcopy"
alias zshconfig="emacs -nw ~/.zshrc"
alias ecrlogin="aws ecr get-login --no-include-email | bash"
alias lala="echo 💩"
alias ok="echo 😁"
alias simulator="open -n /Applications/Xcode.app/Contents/Developer/Applications/Simulator.app"

# docker aliases
alias cdkc='docker rm $(docker ps -qa --no-trunc --filter "status=exited")'
alias cdkca='docker rm -f $(docker ps -a -q)'
alias cdki='docker rmi $(docker images --filter "dangling=true" -q --no-trunc)'
alias cdkia='docker rmi -f $(docker images -q)'
alias cdkn='docker run -v /var/run/docker.sock:/var/run/docker.sock -v /var/lib/docker:/var/lib/docker --rm martin/docker-cleanup-volumes'
alias cdnp='docker network prune -f'
alias cdvp='yes | docker volume prune'
alias cdall='(cdkca || true) && (cdki || true) && (cdkn || true) && (cdnp || true) && (cdvp || true)'

# kube aliases
alias kube-prod="KUBECONFIG=~/.kube/kube-prod-us-cilium.config kubectl"
alias kube-prod-old="kubectl --context kube-prod.tfgco.com"
alias kube-stag="KUBECONFIG=~/.kube/kube-stag-us-cilium.config kubectl"
alias kube-mystack="kubectl --context kube-mystack.tfgco.com"
alias kube-eu="KUBECONFIG=~/.kube/kube-prod-eu-cilium.config kubectl"
alias kube-ap="KUBECONFIG=~/.kube/kube-prod-ap-cilium.config kubectl"
alias kube-sa="KUBECONFIG=~/.kube/config-prod-sa.yaml kubectl"
alias kube-nodes-info='kubectl get nodes --no-headers | grep node | awk '\''{print $1}'\'' | xargs -I {} sh -c '\''echo {} ; kubectl describe node {} | grep Allocated -A 5 | grep -ve Event -ve Allocated -ve percent -ve -- ; echo '\'''
# Get CPU request total (we x40 because because each m4.xlarge has 4 vcpus (4000m) )
alias kube-nodes-cpualloc='kube-nodes-info | grep % | awk '\''{print $1}'\'' | awk '\''{ sum += $1 } END { if (NR > 0) { print sum/(NR*40), "%\n" } }'\'''
# Get mem request total (we x160 because because each m4.xlarge has 16G ram )
alias kube-nodes-memalloc='kube-nodes-info | grep % | awk '\''{print $5}'\'' | awk '\''{ sum += $1 } END { if (NR > 0) { print sum/(NR*160), "%\n" } }'\'''
alias stern-prod-old="stern --context kube-prod.tfgco.com"
alias stern-prod="KUBECONFIG=~/.kube/kube-prod-us-cilium.config stern"
alias stern-stag="KUBECONFIG=~/.kube/kube-stag-us-cilium.config stern"
alias stern-mystack="stern --context kube-mystack.tfgco.com"
alias stern-eu="KUBECONFIG=~/.kube/kube-prod-eu-cilium.config stern"
alias stern-ap="KUBECONFIG=~/.kube/kube-prod-ap-cilium.config stern"
alias stern-sa="KUBECONFIG=~/.kube/config-prod-sa.yaml stern"
alias watch-prod-old="watch kubectl --context kube-prod.tfgco.com"
alias watch-prod="KUBECONFIG=~/.kube/kube-prod-us-cilium.config watch kubectl"
alias watch-stag="KUBECONFIG=~/.kube/kube-stag-us-cilium.config watch kubectl"
alias watch-mystack="watch kubectl --context kube-mystack.tfgco.com"
alias watch-eu="KUBECONFIG=~/.kube/kube-prod-eu-cilium.config watch kubectl"
alias watch-ap="KUBECONFIG=~/.kube/kube-prod-ap-cilium.config watch kubectl"
alias watch-sa="KUBECONFIG=~/.kube/config-prod-sa.yaml watch kubectl"

# git aliases
alias gbrdelete="git branch | grep -v "master" | xargs git branch -D"
alias gleaderboards="git shortlog -sn"
alias gbranches='git for-each-ref --count=10 --sort=-committerdate refs/heads/ --format="%(refname:short)"'

# kill using grep
killGrep() {
    kill $(ps aux | grep $1 | awk '{print $2}')
}

# force kill using grep
killGrepForce() {
    kill -9 $(ps aux | grep $1 | awk '{print $2}')
}

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
    pgbadger --prefix "%t:%r:%u@%d:[%p]:" --outfile /tmp/pgbadger.html -f stderr $1 && open /tmp/pgbadger.html
}
dockerKillByName() {
    docker ps -aqf "name=$1" | xargs -L1 docker kill
}
random() {
    re='^[0-9]+$'
    if ! [[ $1 =~ $re ]] ; then
        openssl rand -hex 16
    else
        openssl rand -hex $1
    fi
}

# helper functions aliases
alias dkill=dockerKillByName
alias epoch=nodeEpoch
alias gFiles=gitAffectedFiles
alias gchangelog=gitChangelog
alias greplace=gitReplace
alias kill-grep=killGrep
alias kill-grep-force=killGrepForce
alias pghtml=pgbadgerGenerateHtml
alias ssh-prod=sshProd
alias ssh-stag=sshStag
alias timestamp=nodeTimestamp

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/cscatolini/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/cscatolini/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/cscatolini/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/cscatolini/google-cloud-sdk/completion.zsh.inc'; fi
