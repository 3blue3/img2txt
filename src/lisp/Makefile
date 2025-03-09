#
# Note: Apply arguments when using 'make run' by setting the ARGS var
# Example: make run ARGS="spider.png --sample normal"

# Compiler
SBCL=sbcl

# Source & Output
SRC=img2txt.lisp
OUT=img2txt

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

.PHONY: all run script clean
