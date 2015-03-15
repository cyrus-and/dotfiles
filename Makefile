.PHONY: stow unstow

stow:
	stow -t ~ -R */

unstow:
	stow -t ~ -D */
