#
# Note: Apply arguments when using 'make run' by setting the ARGS var
# Example: make run ARGS="spider.png --sample normal"

# Compiler
SBCL=sbcl

# Source & Output
SRC=img2txt.lisp
OUT=img2txt

# Detect OS and install SBCL if missing
detect-os := $(shell uname -s)

# Default arguments (overridable)
ARGS?=""

# Build the executable
$(OUT): $(SRC)
	@echo "Compiling $(SRC) to $(OUT)..."
	@$(SBCL) --non-interactive --load $(SRC) --eval "(img2txt:build-exec :filepath \"./$(OUT)\")"
	@echo "Build complete!"

# Run with arguments
run: $(OUT)
	@echo "Running $(OUT) with args: $(ARGS)"
	@time echo $(ARGS) | xargs ./$(OUT)

# Run as a script
script:
	@$(SBCL) --script $(SRC) $(ARGS)

# Clean build files
clean:
	@echo "Cleaning up..."
	@rm -f $(OUT)
	@echo "Clean complete."


install-sbcl:
	@if ! command -v sbcl >/dev/null 2>&1; then \
		echo "SBCL not found. Installing..."; \
		if [ "$(detect-os)" = "Linux" ]; then \
			if command -v apt >/dev/null 2>&1; then \
				sudo apt update && sudo apt install -y sbcl; \
			elif command -v dnf >/dev/null 2>&1; then \
				sudo dnf install -y sbcl; \
			elif command -v pacman >/dev/null 2>&1; then \
				sudo pacman -S --noconfirm sbcl; \
			elif command -v zypper >/dev/null 2>&1; then \
				sudo zypper install -y sbcl; \
			else \
				echo "Unsupported Linux package manager. Install SBCL manually."; \
				exit 1; \
			fi; \
		elif [ "$(detect-os)" = "Darwin" ]; then \
			if command -v brew >/dev/null 2>&1; then \
				brew install sbcl; \
			else \
				echo "Homebrew not found. Install it from https://brew.sh"; \
				exit 1; \
			fi; \
		elif [ "$(detect-os)" = "FreeBSD" ]; then \
			sudo pkg install -y sbcl; \
		else \
			echo "Unsupported OS. Please install SBCL manually."; \
			exit 1; \
		fi; \
	fi

.PHONY: all run script clean install-sbcl
