# ------------------------------------------------------------
#  Fortran + Python GUI Build System for SAN12
# ------------------------------------------------------------

# Fortran compiler and flags
FC      = gfortran
FFLAGS  = -w -fno-automatic

# Executable name
TARGET  = san2015

# Source file
SRC     = SAN2015-01.for

# Python script
PY_SCRIPT = san12_gui.py

# ROOT macro file
ROOT_MACRO = plot_SAN12.C

# Install destination
PREFIX  = /usr/local/bin

# ------------------------------------------------------------
# Python Detection
# ------------------------------------------------------------

# Find user's Python interpreter
PYTHON := $(shell command -v python3.11 || command -v python3.13)

# Detect user's Python major.minor version (e.g. 3.11)
PYVER := $(shell $(PYTHON) -c "import sys; print(f'{sys.version_info[0]}.{sys.version_info[1]}')")

# ------------------------------------------------------------
# Build Fortran Program
# ------------------------------------------------------------

all: $(TARGET)

$(TARGET): $(SRC)
	$(FC) $(FFLAGS) -o $(TARGET) $(SRC)

# ------------------------------------------------------------
# Fix the shebang in fit_gui.py
# ------------------------------------------------------------

#linux
# fix_shebang:
# 	@echo "Updating shebang in $(PY_SCRIPT) to use python$(PYVER)"
# 	@sed -i "1s|^#!.*|#!/usr/bin/env python$(PYVER)|" $(PY_SCRIPT)

#macOS
fix_shebang:
	@echo "Updating shebang in $(PY_SCRIPT) to use python$(PYVER)"
	@sed -i '' "1s|^#!.*|#!/usr/bin/env python$(PYVER)|" $(PY_SCRIPT)

# ------------------------------------------------------------
# Install system-wide command: san12
# ------------------------------------------------------------

install: $(TARGET) fix_shebang
	@echo "Installing Fortran executable to $(PREFIX)"
	install -m 755 $(TARGET) $(PREFIX)/

	@echo "Creating wrapper script 'san12'..."

	# Wrapper script that launches the GUI with correct paths
	echo "#!/usr/bin/env bash" > san12
	echo "export SAN12_FORTRAN_EXE=\"$(PREFIX)/$(TARGET)\"" >> san12
#	echo "export SAN12_ROOT_SCRIPT=\"$(abspath $(ROOT_MACRO))\"" >> san12
	echo "$(PYTHON) $(abspath $(PY_SCRIPT)) \"\$$@\"" >> san12

	@echo "Installing san12 wrapper into $(PREFIX)"
	install -m 755 san12 $(PREFIX)/

	@rm san12
	@echo ""
	@echo "------------------------------------------------------"
	@echo "Installed successfully:"
	@echo "  • $(PREFIX)/$(TARGET)"
	@echo "  • $(PREFIX)/san12   (global GUI launcher)"
	@echo ""
	@echo "You can now run the GUI anywhere with:"
	@echo "      san12"
	@echo "------------------------------------------------------"

# ------------------------------------------------------------
# Cleanup
# ------------------------------------------------------------

clean:
	rm -f $(TARGET) *.o *.mod san12
