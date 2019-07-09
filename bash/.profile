# load bashrc for interactive login shells
if [ -n "$BASH_VERSION" -a -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

# always install gems locally with gem or bundler
export GEM_HOME="$HOME/.gem"
export PATH="$PATH:$GEM_HOME/bin"

# always install npm packages locally
export NPM_CONFIG_PREFIX="$HOME/.npm"
export PATH="$PATH:$NPM_CONFIG_PREFIX/bin"

# always install pip packages locally
export PIP_USER=yes
export PATH="$HOME/.local/bin:$PATH"
