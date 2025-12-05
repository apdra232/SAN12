# **SAN12 — Fortran + Python GUI System**

SAN12 provides a graphical front-end (`san12_gui.py`) for the Fortran spectrum-analysis engine `san2015`.  
A global command **`san12`** is installed so the GUI can be launched from any terminal.

---

## **Requirements**

### **System**
- **gfortran**
- **Python ≥ 3.6**  
  ⭐ Recommended: **Python 3.11** (most stable)
- (Optional) **CERN ROOT / PyROOT** for ROOT → DAF conversion

### **Python Packages**

numpy
matplotlib
tkinter (included with Python on macOS/Linux)
uproot or PyROOT (optional for ROOT support)


Install core dependencies:

```bash
python3.11 -m pip install numpy matplotlib
```
Project Files

SAN2015-01.for     # Fortran SAN12 engine
san12_gui.py         # Python GUI + CLI
Makefile           # Build + install system

Building

Compile the Fortran executable:

```bash
make
make install
```

Make install may require sudo

Produces:

./san2015

Installation

Install globally:

sudo make install

This will:

    Detect your Python version

    Update the shebang in san12_gui.py

    Install san2015 into /usr/local/bin

    Install a launcher script san12 into /usr/local/bin

Run SAN12 from anywhere:
```bash
san12
```

Launches GUI

See san12.help for details of input file

Command-line mode

python san12_gui.py input.inp

Other examples:

python san12_gui.py --convert spectrum.root     # Convert ROOT → DAF
python san12_gui.py --plot run_output           # Plot an existing run
python san12_gui.py input.inp --no-save         # Run without output folder
python san12_gui.py input.inp --folder my_run   # Custom folder name

Cleaning

make clean

Uninstall

sudo rm /usr/local/bin/san2015
sudo rm /usr/local/bin/san12

Notes

    Python 3.11 gives best long-term stability.

    ROOT is optional unless converting ROOT → DAF.

    The wrapper script sets all required environment variables — no hard-coded paths.

    Works on macOS and Linux.

Enjoy using SAN12!

If you'd like a more styled README (badges, screenshots, sections), I can generate that too.

