if [ -f "$HOME/.profile" ]; then
	() {
		# Source the POSIX profile with local sh semantics, especially word splitting.
		emulate -L sh
		. "$HOME/.profile"
	}
fi
