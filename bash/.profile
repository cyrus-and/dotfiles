# load bashrc for interactive login shells
if [ -n "$BASH_VERSION" -a -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

# password store on dropbox
export PASSWORD_STORE_DIR="$HOME/Dropbox/pass/"

# always install gems locally with gem or bundler
export GEM_HOME="$HOME/.gem"
export PATH="$GEM_HOME/bin:$PATH"

# always install npm packages locally
export NPM_CONFIG_PREFIX="$HOME/.npm"
export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"

# always install pip packages locally
export PIP_USER=yes
export PATH="$HOME/.local/bin:$HOME/Library/Python/3.7/bin/:$PATH"

# add rust to path
export PATH="$HOME/.cargo/bin:$PATH"
