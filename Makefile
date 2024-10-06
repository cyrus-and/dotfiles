MAKEFLAGS += --always-make

stow:
	stow --no-folding --target ~/ --restow */

unstow:
	stow --no-folding --target ~/ --delete */
