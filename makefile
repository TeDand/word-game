EXEC       = word-game-exe

.PHONY: build run clean

# Define the default target
default: build

# Build and run the Haskell project
all:
	stack build
	stack exec $(EXEC)

# Build the Haskell project
build:
	stack build

# Run the Haskell project
run:
	stack exec $(EXEC)

# Clean generated files
clean:
	stack clean