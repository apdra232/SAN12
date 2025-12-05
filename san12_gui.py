import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess
import os
import sys
import threading
import re
import time
from shutil import copy2, rmtree
import matplotlib.pyplot as plt
import numpy as np

# ---------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------
FORTRAN_EXE = os.environ.get("SAN12_FORTRAN_EXE", "san2015")
ROOT_SCRIPT = os.environ.get("SAN12_ROOT_SCRIPT", "plot_SAN12.C")

def convert_root_to_daf(root_path, daf_path=None, output_box=None, rebin_factor=1):
    """
    Convert a 1-D ROOT histogram to a DAF-formatted text file.
    Optionally rebin by an integer factor (2, 4, ...).
    If rebin_factor > 1, also saves a rebinned ROOT file with an x-axis
    where each bin corresponds to one channel (0..N_bins).
    """
    try:
        import ROOT
    except ImportError:
        msg = "❌ PyROOT not available; install ROOT with Python bindings.\n"
        (output_box.insert(tk.END, msg) if output_box else print(msg))
        return

    if not os.path.exists(root_path):
        msg = f"❌ ROOT file not found: {root_path}\n"
        (output_box.insert(tk.END, msg) if output_box else print(msg))
        return

    f = ROOT.TFile.Open(root_path)
    if not f or f.IsZombie():
        msg = f"❌ Cannot open ROOT file: {root_path}\n"
        (output_box.insert(tk.END, msg) if output_box else print(msg))
        return

    # Get histogram
    h = f.Get("spectrum1")
    if not h:
        keys = f.GetListOfKeys()
        if keys.GetSize() == 0:
            msg = "❌ No histograms found in file.\n"
            (output_box.insert(tk.END, msg) if output_box else print(msg))
            f.Close()
            return
        h = keys.At(0).ReadObj()

    nbins = h.GetNbinsX()
    if output_box:
        output_box.insert(tk.END, f"📊 Found histogram '{h.GetName()}' with {nbins} bins.\n")
    else:
        print(f"📊 Found histogram '{h.GetName()}' with {nbins} bins.")

    h_out = h
    rebinned_root_path = None

    # --- Rebin with corrected X-axis scaling ---
    if rebin_factor > 1:
        nbins_new = nbins // rebin_factor
        # Combine bins using ROOT Rebin
        h_tmp = h.Rebin(rebin_factor, f"{h.GetName()}_rebin{rebin_factor}")
        # Now create a new histogram with x-axis 0..nbins_new
        h_out = ROOT.TH1F(f"{h.GetName()}_scaled{rebin_factor}",
                          h.GetTitle(),
                          nbins_new, 0, nbins_new)
        for i in range(1, nbins_new + 1):
            h_out.SetBinContent(i, h_tmp.GetBinContent(i))

        rebinned_root_path = os.path.splitext(root_path)[0] + f"_x{rebin_factor}.root"
        msg = f"🔁 Rebinning by factor {rebin_factor}: {nbins} → {nbins_new} bins (x-axis set to 0..{nbins_new}).\n"
        (output_box.insert(tk.END, msg) if output_box else print(msg))

        fout = ROOT.TFile(rebinned_root_path, "RECREATE")
        h_out.Write()
        fout.Close()
        msg = f"💾 Saved rebinned ROOT file: {rebinned_root_path}\n"
        (output_box.insert(tk.END, msg) if output_box else print(msg))
        nbins = nbins_new

    # --- Determine DAF output name ---
    if daf_path is None:
        base = os.path.splitext(root_path)[0]
        daf_path = f"{base}.daf" if rebin_factor == 1 else f"{base}_x{rebin_factor}.daf"

    # --- Write DAF file ---
    with open(daf_path, "w") as out:
        out.write(" " * 40 + "\n" + " " * 40 + "\n")
        out.write(" -1\n")
        out.write(f" 1-D HISTO NUMBER 1 {h_out.GetTitle()[:28]}\n")
        out.write(f" SIZE= {nbins}\n")

        line = []
        for i in range(1, nbins + 1):
            val = int(round(h_out.GetBinContent(i)))
            line.append(f"{val:10d}")
            if len(line) == 10:
                out.write("".join(line) + "\n")
                line = []
        if line:
            out.write("".join(line) + "\n")

    f.Close()
    msg = f"✅ Wrote DAF file: {daf_path}\n"
    (output_box.insert(tk.END, msg) if output_box else print(msg))


# ---------------------------------------------------------------------
# Helper: prepare output directory and copy input/spectrum
# ---------------------------------------------------------------------
def prepare_output_dir(input_file, output_box, save_outputs, folder_name):
    """Create output directory near the input, copy input & data files, ask before overwrite."""
    base_dir = os.path.dirname(os.path.abspath(input_file))

    # If not saving, just run in input directory
    if not save_outputs:
        return base_dir

    if not folder_name:
        folder_name = "run_output"

    out_dir = os.path.join(base_dir, folder_name)

    # Ask before overwriting an existing folder
    if os.path.exists(out_dir):
        proceed = messagebox.askyesno(
            "Overwrite existing folder?",
            f"The folder:\n\n{out_dir}\n\nalready exists.\n\nDo you want to overwrite it?"
        )
        if not proceed:
            output_box.insert(tk.END, "⚠️  Run cancelled by user.\n")
            return None
        try:
            rmtree(out_dir)
        except Exception as e:
            output_box.insert(tk.END, f"❌ Could not remove folder: {e}\n")
            return None

    os.makedirs(out_dir, exist_ok=True)
    output_box.insert(tk.END, f"📁 Created output directory: {out_dir}\n")

    # Copy input file into folder under its name AND as san12.inp
    try:
        input_basename = os.path.basename(input_file)
        copy2(input_file, os.path.join(out_dir, input_basename))
        copy2(input_file, os.path.join(out_dir, "san12.inp"))
        output_box.insert(tk.END, f"📄 Copied input file: {input_basename} and san12.inp\n")
    except Exception as e:
        output_box.insert(tk.END, f"⚠️ Could not copy input file: {e}\n")


    # Copy any referenced .daf or .root file
    try:
        with open(input_file) as f:
            for line in f:
                stripped = line.strip()
                if stripped.lower().endswith((".daf", ".root")) and os.path.exists(stripped):
                    copy2(stripped, out_dir)
                    output_box.insert(tk.END, f"📄 Copied data file: {stripped}\n")
    except Exception as e:
        output_box.insert(tk.END, f"⚠️ Could not copy data files: {e}\n")

    output_box.see(tk.END)
    return out_dir

def thread_safe_call(root, func, *args, **kwargs):
    """Schedule any GUI or plotting call on the main thread."""
    root.after(0, lambda: func(*args, **kwargs))

# ---------------------------------------------------------------------
# Fortran run
# ---------------------------------------------------------------------
def run_fortran_live(input_file, output_box, save_outputs=True, folder_name="run_output", out_dir=None):
    """Run SAN12 Fortran with .inp file inside a given folder."""
    # If caller didn't supply out_dir, then create it (for standalone button use)
    if out_dir is None:
        out_dir = prepare_output_dir(input_file, output_box, save_outputs, folder_name)
        if out_dir is None:
            return

    if not os.path.exists(FORTRAN_EXE):
        output_box.insert(tk.END, f"❌ Fortran executable not found: {FORTRAN_EXE}\n")
        return
    if not os.path.exists(input_file):
        output_box.insert(tk.END, f"❌ Input file not found: {input_file}\n")
        return

    output_box.insert(tk.END, f"▶ Preparing to run SAN12 with {input_file}\n")

    try:
        process = subprocess.run([FORTRAN_EXE, os.path.basename(input_file)],
                         capture_output=True, text=True, cwd=out_dir)
        output_box.insert(tk.END, process.stdout + "\n")
        if process.stderr:
            output_box.insert(tk.END, "⚠️  " + process.stderr + "\n")
    except Exception as e:
        output_box.insert(tk.END, f"❌ Error running Fortran: {e}\n")
        return

    output_box.insert(tk.END, "✅ Fortran execution complete.\n\n")
    output_box.see(tk.END)


# ---------------------------------------------------------------------
# ROOT plotting
# ---------------------------------------------------------------------
def run_root(output_box, out_dir):
    if not os.path.exists(ROOT_SCRIPT):
        output_box.insert(tk.END, f"❌ ROOT macro not found: {ROOT_SCRIPT}\n")
        return
    output_box.insert(tk.END, "▶ Generating ROOT plot...\n")
    sys.stdout.flush()

    # --- Copy ROOT macro into output folder so ROOT can find it ---
    try:
        macro_src = os.path.abspath(ROOT_SCRIPT)
        if not os.path.exists(macro_src):
            output_box.insert(tk.END, f"⚠️ ROOT macro not found: {macro_src}\n")
    except Exception as e:
        output_box.insert(tk.END, f"⚠️ Could not copy ROOT macro: {e}\n")

    # --- Run ROOT macro inside output folder ---
    try:
        subprocess.run(["root", "-l", "-b", "-q", "-n", ROOT_SCRIPT], cwd=out_dir, check=True)
        output_box.insert(tk.END, f"✅ ROOT plot created in {out_dir}\n")
    except Exception as e:
        output_box.insert(tk.END, f"❌ Error running ROOT: {e}\n")


# ---------------------------------------------------------------------
# Matplotlib plotting
# ---------------------------------------------------------------------
from mpl_toolkits.axes_grid1 import make_axes_locatable

def plot_with_matplotlib(output_box, out_dir, show_residuals=True):
    """
    Draw SAN12 histogram, fits, background, and optional residual panel.
    Clears all existing axes but keeps same window open.
    """
    try:
        if hasattr(plot_with_matplotlib, "fig") and plt.fignum_exists(plot_with_matplotlib.fig.number):
            for child in list(plot_with_matplotlib.fig.axes):
                plot_with_matplotlib.fig.delaxes(child)
        else:
            plot_with_matplotlib.fig, plot_with_matplotlib.ax = plt.subplots(figsize=(10, 6))

        fig = plot_with_matplotlib.fig
        ax = fig.add_subplot(111)
        plot_with_matplotlib.ax = ax



        # --- Add a small residual axis only if requested ---
        if show_residuals:
            divider = make_axes_locatable(ax)
            ax_resid = divider.append_axes("bottom", size="20%", pad=0.25, sharex=ax)
            ax_resid.clear()
        else:
            ax_resid = None

        # --- Load data ---
        hist = np.loadtxt(os.path.join(out_dir, "histogram.dat"))
        total = np.loadtxt(os.path.join(out_dir, "total_fit.dat"))
        bg = np.loadtxt(os.path.join(out_dir, "background.dat"))

        # --- Main histogram and fits ---
        ax.scatter(hist[:, 0], hist[:, 1], s=4, color='black', label='Data')
        ax.plot(total[:, 0], total[:, 1], 'r-', label='Total Fit', linewidth=3)
        ax.plot(bg[:, 0], bg[:, 1], 'k--', label='Background', linewidth=2)

        # --- Individual peaks ---
        for j in range(1, 100):
            fname = os.path.join(out_dir, f"peak_{j}.dat")
            if not os.path.exists(fname):
                break
            data = np.loadtxt(fname)
            ax.plot(data[:, 0], data[:, 1], '--', linewidth=2)

        # --- Legend & labels ---
        ax.set_ylabel("Counts")
        ax.set_title("SAN12 Fit Results")
        ax.legend(loc="upper left")

        # --- Residuals section (if enabled) ---
        if show_residuals and ax_resid is not None:
            y_data = hist[:, 1]
            y_fit = np.interp(hist[:, 0], total[:, 0], total[:, 1])
            residuals = y_data - y_fit

            ax_resid.bar(hist[:, 0], residuals, width=1.0,
                         color='steelblue', edgecolor='none')
            ax_resid.axhline(0, color='k', linestyle='--', linewidth=1)
            ax_resid.set_ylabel("Residual", fontsize=9)
            ax_resid.set_xlabel("Channel", fontsize=11)
            ax_resid.tick_params(axis='x', labelsize=9)
            ax_resid.tick_params(axis='y', labelsize=9)
            plt.setp(ax.get_xticklabels(), visible=False)
        else:
            ax.set_xlabel("Channel")

        # ------------------------------------------------------------------
        # Peak summary section (identical to your original)
        # ------------------------------------------------------------------
        peak_data = []
        fs = os.path.join(out_dir, "fit_summary.txt")
        if os.path.exists(fs):
            with open(fs) as f:
                for line in f:
                    m = re.search(
                        r"Peak\s+(\d+):\s+Area\s*=\s*([\d.Ee+-]+)\s*\+/-\s*([\d.Ee+-]+)",
                        line)
                    if m:
                        peak_data.append({
                            "index": int(m.group(1)),
                            "area": float(m.group(2)),
                            "err": float(m.group(3))
                        })

        # --- χ² info from san12.out ---
        if os.path.exists(os.path.join(out_dir, "san12.out")):
            with open(os.path.join(out_dir, "san12.out")) as f:
                for line in f:
                    m = re.search(r"XSQPK\(\s*(\d+)\)\s*=\s*([\d.Ee+-]+)", line)
                    if m:
                        pid = int(m.group(1))
                        xsq = float(m.group(2))
                        for p in peak_data:
                            if p["index"] == pid:
                                p["xsq"] = xsq

        for artist in fig.texts[:]:
            artist.remove()

        # --- Text summary box on figure ---
        summary_lines = ["Peak Results:"]
        for p in sorted(peak_data, key=lambda x: x["index"]):
            xsq_txt = f", χ²={p['xsq']:.3f}" if "xsq" in p else ""
            summary_lines.append(
                f"P{p['index']}: {p['area']:.1f} ± {p['err']:.1f}{xsq_txt}"
            )

        for txt in fig.texts:
            txt.remove()
        fig.text(0.15, 0.55, "\n".join(summary_lines),
                 fontsize=9, family='monospace',
                 bbox=dict(facecolor='white', alpha=0.9, edgecolor='black'))

        # --- Layout and save ---
        fig.tight_layout()
        suffix = "_with_residuals" if show_residuals else ""
        png_path = os.path.join(out_dir, f"san12_fit{suffix}.png")
        pdf_path = os.path.join(out_dir, f"san12_fit{suffix}.pdf")
        fig.savefig(png_path, dpi=300)
        fig.savefig(pdf_path)
        output_box.insert(
            tk.END, f"💾 Updated Matplotlib plots: {png_path}, {pdf_path}\n")

        fig.canvas.draw_idle()
        plt.show(block=False)

    except Exception as e:
        output_box.insert(tk.END, f"❌ Error plotting: {e}\n")
        output_box.see(tk.END)


# ---------------------------------------------------------------------
# Combined threaded run
# ---------------------------------------------------------------------
def run_all(input_file, output_box, save_outputs=True, folder_name="run_output", root=None):
    def task():
        out_dir = prepare_output_dir(input_file, output_box, save_outputs, folder_name)
        if out_dir is None:
            return
        run_fortran_live(input_file, output_box, save_outputs, folder_name, out_dir=out_dir)
        run_root(output_box, out_dir)
        time.sleep(1)
        # pass the checkbox state to the plotting function
        thread_safe_call(root, plot_with_matplotlib, output_box, out_dir, show_resid_var.get())
    threading.Thread(target=task).start()

# ---------------------------------------------------------------------
# GUI
# ---------------------------------------------------------------------
def main():
    root = tk.Tk()
    root.title("SAN12")
    root.geometry("720x560")

    tk.Label(root, text="SAN12 Fitting", font=("Arial", 14, "bold")).pack(pady=10)

    # Input file
    frame = tk.Frame(root)
    frame.pack(pady=5)
    tk.Label(frame, text="Input .INP file:").pack(side=tk.LEFT, padx=5)
    inp_var = tk.StringVar()
    tk.Entry(frame, textvariable=inp_var, width=40).pack(side=tk.LEFT, padx=5)
    tk.Button(frame, text="Browse",
            command=lambda: inp_var.set(
                filedialog.askopenfilename(
                    filetypes=[("Input files","*.inp"),("All files","*.*")],
                    initialdir=os.getcwd()   # 👈 start in CWD
                )
            )).pack(side=tk.LEFT, padx=5)

    # def edit_inp_file():
    #     """Open the selected .INP file in gedit."""
    #     inp_path = inp_var.get().strip()
    #     if not inp_path:
    #         messagebox.showwarning("No file selected", "Please select an input (.inp) file first.")
    #         return
    #     if not os.path.exists(inp_path):
    #         messagebox.showerror("File not found", f"The file does not exist:\n{inp_path}")
    #         return
    #     try:
    #         subprocess.Popen(["code", inp_path])
    #     except FileNotFoundError:
    #         messagebox.showerror("Editor not found", "Could not launch gedit. Make sure it’s installed.")
    #     except Exception as e:
    #         messagebox.showerror("Error", f"Failed to open file:\n{e}")

    # tk.Button(frame, text="Edit", command=edit_inp_file).pack(side=tk.LEFT, padx=5)

    # Output folder options
    out_frame = tk.Frame(root)
    out_frame.pack(pady=5)
    tk.Label(out_frame, text="Output folder name:").pack(side=tk.LEFT, padx=5)
    out_var = tk.StringVar(value="run_output")
    tk.Entry(out_frame, textvariable=out_var, width=30).pack(side=tk.LEFT, padx=5)
    save_var = tk.BooleanVar(value=False)
    tk.Checkbutton(root, text="Save outputs to folder", variable=save_var).pack(pady=2)
    global show_resid_var
    show_resid_var = tk.BooleanVar(value=True)
    tk.Checkbutton(root, text="Show residuals in plot", variable=show_resid_var).pack(pady=2)

    # Console output
    output_box = tk.Text(root, wrap="word", height=15, bg="#1e1e1e", fg="#e6e6e6",
                         insertbackground="white", font=("Courier New", 10))
    output_box.pack(fill=tk.BOTH, expand=True, padx=10, pady=5)

    # Buttons
    btn_frame = tk.Frame(root)
    btn_frame.pack(pady=10)

    conv_frame = tk.Frame(root)
    conv_frame.pack(pady=5, fill="x")
    tk.Label(conv_frame, text="Convert ROOT → DAF:", font=("Arial", 11, "bold")).pack(side=tk.LEFT, padx=5)

    # --- Add rebin factor option ---
    tk.Label(conv_frame, text="Bin factor:").pack(side=tk.LEFT, padx=5)
    rebin_var = tk.StringVar(value="1")
    rebin_entry = tk.Entry(conv_frame, textvariable=rebin_var, width=4)
    rebin_entry.pack(side=tk.LEFT, padx=3)

    root_file_var = tk.StringVar()
    tk.Entry(conv_frame, textvariable=root_file_var, width=30).pack(side=tk.LEFT, padx=5)

    def choose_root():
        fname = filedialog.askopenfilename(
            filetypes=[("ROOT files","*.root"),("All files","*.*")],
            initialdir=os.getcwd()
        )
        if fname:
            os.chdir(os.path.dirname(fname))  # 👈 change CWD to chosen file’s folder
            root_file_var.set(fname)

    tk.Button(conv_frame, text="Browse", command=choose_root).pack(side=tk.LEFT, padx=3)

    def do_conversion():
        root_path = root_file_var.get().strip()
        if not root_path:
            messagebox.showerror("No file", "Please select a ROOT file first.")
            return
        try:
            factor = int(rebin_var.get())
            if factor < 1:
                raise ValueError
        except ValueError:
            messagebox.showerror("Invalid factor", "Bin factor must be a positive integer (1, 2, 4, ...).")
            return

        # Don’t add "_x1" for default case
        base = os.path.splitext(root_path)[0]
        daf_path = f"{base}.daf" if factor == 1 else f"{base}_x{factor}.daf"

        convert_root_to_daf(root_path, daf_path, output_box, rebin_factor=factor)   

    tk.Button(conv_frame, text="Convert", command=do_conversion).pack(side=tk.LEFT, padx=5)
    tk.Button(btn_frame, text="Run SAN12 Only",
              command=lambda: threading.Thread(
                  target=run_fortran_live,
                  args=(inp_var.get(), output_box, save_var.get(), out_var.get())
              ).start(),
              width=18).pack(side=tk.LEFT, padx=5)

    tk.Button(btn_frame, text="Run SAN12 + Show Fits",
            command=lambda: run_all(inp_var.get(), output_box, save_var.get(), out_var.get(), root),
            width=20).pack(side=tk.LEFT, padx=5)


    tk.Button(root, text="Quit", command=root.destroy,
              bg="#ffcccc", fg="black").pack(pady=5)

    root.mainloop()

import argparse

def main_cli():
    parser = argparse.ArgumentParser(
        description="SAN12 fitting and plotting CLI tool"
    )
    parser.add_argument("input_file", nargs="?", help="Path to the .INP input file")
    parser.add_argument("--no-save", action="store_true", help="Run in-place (don't create output folder)")
    parser.add_argument("--folder", default="run_output", help="Output folder name (default: run_output)")
    parser.add_argument("--convert", metavar="ROOTFILE", help="Convert ROOT file to DAF and exit")
    parser.add_argument("--plot", metavar="OUTDIR", help="Generate matplotlib plot from existing run folder")
    args = parser.parse_args()

    # Simulate the GUI's output box
    class DummyBox:
        def insert(self, *a, **kw): print(a[1], end="")
        def see(self, *a, **kw): pass

    output_box = DummyBox()

    if args.convert:
        root_file = os.path.abspath(args.convert)
        daf_file = os.path.splitext(root_file)[0] + ".daf"
        convert_root_to_daf(root_file, daf_file, output_box)
        return

    if args.plot:
        out_dir = os.path.abspath(args.plot)
        plot_with_matplotlib(output_box, out_dir)
        return

    if not args.input_file:
        print("❌ No input file specified. Use:")
        print("   python fit_gui.py <input.inp>")
        return

    input_file = os.path.abspath(args.input_file)
    save_outputs = not args.no_save
    folder_name = args.folder

    run_fortran_live(input_file, output_box, save_outputs, folder_name)
    run_root(output_box, os.path.join(os.path.dirname(input_file), folder_name))
    plot_with_matplotlib(output_box, os.path.join(os.path.dirname(input_file), folder_name))


if __name__ == "__main__":
    if len(sys.argv) > 1:
        main_cli()
    else:
        main()

#python fit_gui.py myinput.inp
#python fit_gui.py myinput.inp --no-save
#python fit_gui.py --convert spectrum.root
#python fit_gui.py --plot ./run_output
#python fit_gui.py myinput.inp --folder fit_run_23nov
