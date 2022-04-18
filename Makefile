script_path = $$HOME/.dotfiles/script

install:
	bash $(script_path)/install.sh

fish:
	fish $(script_path)/fish.fish

brew:
	bash $(script_path)/brew.sh
