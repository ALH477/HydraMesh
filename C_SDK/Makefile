# =============================================================================
# DCF Simple Makefile (for environments without CMake)
# =============================================================================

CC ?= gcc
AR ?= ar
CFLAGS = -std=c11 -Wall -Wextra -Wpedantic -fPIC -O2
CFLAGS_DEBUG = -std=c11 -Wall -Wextra -Wpedantic -fPIC -g -O0 -DDEBUG
LDFLAGS = -lpthread -ldl

# Directories
SRC_DIR = src
INC_DIR = include
BUILD_DIR = build
TEST_DIR = tests/unit
EXAMPLE_DIR = examples

# Sources
SOURCES = $(wildcard $(SRC_DIR)/*.c)
OBJECTS = $(SOURCES:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)

# Targets
STATIC_LIB = $(BUILD_DIR)/libdcf.a
SHARED_LIB = $(BUILD_DIR)/libdcf.so
TEST_BIN = $(BUILD_DIR)/dcf_tests
EXAMPLE_BIN = $(BUILD_DIR)/example_basic

.PHONY: all clean test debug release shared static example

# Default target
all: static shared test example

# Release build
release: CFLAGS += -DNDEBUG
release: all

# Debug build
debug: CFLAGS = $(CFLAGS_DEBUG)
debug: all

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Compile objects
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -I$(INC_DIR) -c $< -o $@

# Static library
static: $(STATIC_LIB)

$(STATIC_LIB): $(OBJECTS)
	$(AR) rcs $@ $^
	@echo "Built: $@"

# Shared library
shared: $(SHARED_LIB)

$(SHARED_LIB): $(OBJECTS)
	$(CC) -shared -o $@ $^ $(LDFLAGS)
	@echo "Built: $@"

# Test binary
test: $(TEST_BIN)
	@echo "Running tests..."
	@$(TEST_BIN)

$(TEST_BIN): $(TEST_DIR)/test_suite.c $(STATIC_LIB)
	$(CC) $(CFLAGS) -I$(INC_DIR) $< -L$(BUILD_DIR) -ldcf $(LDFLAGS) -o $@
	@echo "Built: $@"

# Example binary
example: $(EXAMPLE_BIN)

$(EXAMPLE_BIN): $(EXAMPLE_DIR)/basic_example.c $(STATIC_LIB)
	$(CC) $(CFLAGS) -I$(INC_DIR) $< -L$(BUILD_DIR) -ldcf $(LDFLAGS) -o $@
	@echo "Built: $@"

# Clean
clean:
	rm -rf $(BUILD_DIR)

# Install
install: $(STATIC_LIB) $(SHARED_LIB)
	install -d $(DESTDIR)/usr/local/lib
	install -d $(DESTDIR)/usr/local/include/dcf
	install -m 644 $(STATIC_LIB) $(DESTDIR)/usr/local/lib/
	install -m 755 $(SHARED_LIB) $(DESTDIR)/usr/local/lib/
	install -m 644 $(INC_DIR)/*.h $(DESTDIR)/usr/local/include/dcf/
	ldconfig || true

# Help
help:
	@echo "DCF Build Targets:"
	@echo "  all      - Build everything (default)"
	@echo "  release  - Build optimized release"
	@echo "  debug    - Build with debug symbols"
	@echo "  static   - Build static library"
	@echo "  shared   - Build shared library"
	@echo "  test     - Build and run tests"
	@echo "  example  - Build example"
	@echo "  clean    - Remove build artifacts"
	@echo "  install  - Install libraries and headers"
