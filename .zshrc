# ============================================================
# .zshrc — Starship + Antidote
# Baseado em github.com/cscatolini/dotfiles
# ============================================================

# ------------------------------------------------------------
# 1. ANTIDOTE — gerenciador de plugins
# ------------------------------------------------------------
# Instalar: brew install antidote
source $(brew --prefix)/opt/antidote/share/antidote/antidote.zsh
antidote load ${ZDOTDIR:-~}/.zsh_plugins.txt

# ------------------------------------------------------------
# 2. ENVIRONMENT & PATH
# ------------------------------------------------------------
export EDITOR="emacs -nw"
export PAGER="less -iMSx4FXRe"

export PATH="$HOME/.local/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh" --no-use
[ -s "$NVM_DIR/bash_completion" ] && source "$NVM_DIR/bash_completion"
nvm use default --silent 2>/dev/null
[ -n "$NVM_BIN" ] && export PATH="$NVM_BIN:$PATH"

# ------------------------------------------------------------
# 3. COMPLETIONS
# ------------------------------------------------------------
autoload -U compinit && compinit

# Editar linha de comando no $EDITOR com Ctrl+X Ctrl+E
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# ------------------------------------------------------------
# 4. INICIALIZAÇÃO DE FERRAMENTAS
# (comente as que não usar)
# ------------------------------------------------------------

# Google Cloud SDK
[ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]       && source "$HOME/google-cloud-sdk/path.zsh.inc"
[ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ] && source "$HOME/google-cloud-sdk/completion.zsh.inc"

# ------------------------------------------------------------
# 5. ALIASES — software
# ------------------------------------------------------------
alias open-arc='open -a Arc'
export BROWSER='open -a Arc'

alias emacs="emacs -nw"
alias edit="$EDITOR"
alias ll="ls -lah"
alias cpwd="pwd | pbcopy"
alias zshconfig="$EDITOR ~/.zshrc"
alias code="cd ~/Code"

# bat (syntax highlight no cat) — instalar: brew install bat
command -v bat &>/dev/null && alias cat="bat"

# ------------------------------------------------------------
# 6. ALIASES — typos comuns
# ------------------------------------------------------------
alias car="cat"
alias gti="git"
alias mkae="make"
alias maek="make"
alias cta="cat"
alias sl="ls"

# ------------------------------------------------------------
# 10. FUNÇÕES
# ------------------------------------------------------------
# Matar processo por nome (grep)
killGrep()      { kill    $(ps aux | grep "$1" | awk '{print $2}'); }
killGrepForce() { kill -9 $(ps aux | grep "$1" | awk '{print $2}'); }
alias kill-grep=killGrep
alias kill-grep-force=killGrepForce

# Docker — matar container por nome
dockerKillByName() { docker ps -aqf "name=$1" | xargs -L1 docker kill; }
alias dkill=dockerKillByName

# Git — substituição em massa via grep
gitReplace() { git grep -l "$1" | xargs sed -i '' -e "s/$1/$2/g"; }
alias greplace=gitReplace

# Git — changelog desde uma tag
gitChangelog() { git log --oneline --no-merges "$1"..HEAD; }
alias gchangelog=gitChangelog

# Git — arquivos afetados num commit
gitAffectedFiles() { git diff-tree --no-commit-id --name-only -r "$1"; }
alias gFiles=gitAffectedFiles

# Helpers de timestamp
alias timestamp="date +%Y%m%d%H%M%S"
alias epoch="date +%s"

# Gerar hex aleatório
random() {
  if [[ $1 =~ ^[0-9]+$ ]]; then
    openssl rand -hex "$1"
  else
    openssl rand -hex 16
  fi
}

# ------------------------------------------------------------
# 11. STARSHIP — prompt (deve ficar por último)
# ------------------------------------------------------------
# Instalar: brew install starship
# Customizar: ~/.config/starship.toml
eval "$(starship init zsh)"
