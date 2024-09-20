# load bashrc for interactive login shells
if [ -n "$BASH_VERSION" -a -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

# password store on dropbox
export PASSWORD_STORE_DIR="$HOME/Dropbox/pass/"

# use the homebrew version of ruby
export PATH="/usr/local/opt/ruby/bin:$PATH"

# always install gems locally with gem or bundler
export GEM_HOME="$HOME/.gem"
export PATH="$GEM_HOME/bin:$PATH"

# always install npm packages locally
export NPM_CONFIG_PREFIX="$HOME/.npm"
export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"

# always install pip packages locally
export PIP_USER=yes
export PATH="$(python2 -c 'import site; print(site.USER_BASE + "/bin")'):$PATH"
export PATH="$(python3 -c 'import site; print(site.USER_BASE + "/bin")'):$PATH"

# add rust to path
export PATH="$HOME/.cargo/bin:$PATH"

# add go to path
export PATH="$HOME/go/bin:$PATH"
