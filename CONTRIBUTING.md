# NA

## Development

This package uses [Task](https://taskfile.dev/) to manage development
tasks. Task is a modern alternative to Make that’s easier to use and
more powerful.

### Installing Task

First, install Task following the instructions at
[taskfile.dev](https://taskfile.dev/installation/).

### Available Tasks

Once Task is installed, you can run the following commands from the root
of the package:

``` bash
# Display all available tasks
task

# Install R package dependencies
task install-deps

# Install Python dependencies
task install-py-deps

# Generate documentation
task document

# Build the package
task build

# Install the package
task install

# Check the package
task check

# Run tests
task test

# Build vignettes
task vignettes

# Clean build artifacts
task clean

# Run all main tasks (document, build, check, test)
task all

# Run CI tasks (install-deps, install-py-deps, document, check, test)
task ci
```
