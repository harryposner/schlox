SOURCE_DIR = src
SOURCE_FILES = scanner.scm      \
	       token.scm        \
	       ast.scm          \
	       parser.scm       \
	       pretty-print.scm \
	       eval.scm         \
	       callable.scm     \
	       to-string.scm    \
	       resolver.scm     \
	       schlox.scm

EXECUTABLE = schlox

RELEASE_DIR = build
RELEASE_EXECUTABLE = $(addprefix $(RELEASE_DIR)/, $(EXECUTABLE))
OBJECT_FILES = $(addprefix $(RELEASE_DIR)/, $(SOURCE_FILES:.scm=.o))


DEBUG_DIR = debug
DEBUG_FLAGS = -debug-level 3 -verbose
DEBUG_EXECUTABLE = $(addprefix $(DEBUG_DIR)/, $(EXECUTABLE))
DEBUG_OBJECT_FILES = $(addprefix $(DEBUG_DIR)/, $(SOURCE_FILES:.scm=.o))

.PHONY: default all clean remake setup_release release setup_debug debug

default: release

all: release debug

clean:
	rm -rf $(RELEASE_DIR) $(DEBUG_DIR)

remake: clean all


# Release build rules

release: setup_release $(RELEASE_EXECUTABLE)

setup_release:
	mkdir -p $(RELEASE_DIR)

$(RELEASE_EXECUTABLE): $(OBJECT_FILES)
	csc -o $(RELEASE_EXECUTABLE) $(OBJECT_FILES)

$(RELEASE_DIR)/%.o: $(SOURCE_DIR)/%.scm
	csc -c -o $@ $<


# Debug build rules

debug: setup_debug $(DEBUG_EXECUTABLE)

setup_debug:
	mkdir -p $(DEBUG_DIR)

$(DEBUG_EXECUTABLE): $(DEBUG_OBJECT_FILES)
	csc $(DEBUG_FLAGS) -o $(DEBUG_EXECUTABLE) $(DEBUG_OBJECT_FILES)

$(DEBUG_DIR)/%.o: $(SOURCE_DIR)/%.scm
	csc $(DEBUG_FLAGS) -c -o $@ $<
