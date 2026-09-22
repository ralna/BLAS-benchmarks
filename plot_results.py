#!/usr/bin/env python3
"""Visualise BLAS benchmark results as heatmaps.

Each ``results_<ROUTINE>.csv`` file produced by the benchmark tool is a
table with the columns::

    implementation,m,n,k,gflops

This script renders one heatmap per results file parsed.

Usage
-----
    python plot_results.py                     # all results_*.csv here + one level down
    python plot_results.py results_2026-.../   # every results_*.csv in a directory
    python plot_results.py results_DGEMM.csv   # one or more specific files
"""

import argparse
import csv
import glob
import os
import sys

import matplotlib

import matplotlib.pyplot as plt
import numpy as np

DIMENSIONS = ("m", "n", "k")


def find_csv_files(paths):
    """Resolve the CLI arguments into a list of CSV files.

    With no arguments, look for ``results_*.csv`` in the current directory and one
    level below it (so a timestamped ``results_<datetime>/`` directory is found).
    Directory arguments are globbed for ``results_*.csv``; file arguments are used
    as-is.
    """
    if not paths:
        found = glob.glob("results_*.csv") + glob.glob(os.path.join("*", "results_*.csv"))
        return found

    files = []
    for path in paths:
        if os.path.isdir(path):
            files.extend(glob.glob(os.path.join(path, "results_*.csv")))
        else:
            files.append(path)
    return files


def routine_name(csv_path):
    """Derive the routine name from a ``results_<ROUTINE>.csv`` filename."""
    base = os.path.basename(csv_path)
    name = base[len("results_"):]
    name = name[:-len(".csv")]
    return name or base


def load(csv_path):
    """Parse a results CSV.

    Returns ``(implementations, size_labels, matrix)`` where ``implementations`` is
    the ordered list of row labels, ``size_labels`` the ordered list of column
    labels (one per unique problem size), and ``matrix`` a 2D numpy array of
    GFLOP/s shaped ``(len(implementations), len(size_labels))`` with ``nan`` for
    any missing combination.
    """
    with open(csv_path, newline="") as handle:
        rows = list(csv.DictReader(handle))

    if not rows:
        raise ValueError(f"{csv_path}: no data rows")

    active_dims = [d for d in DIMENSIONS if any((r.get(d) or "").strip() for r in rows)]

    def size_key(row):
        return tuple((row.get(d) or "").strip() for d in active_dims)

    def size_label(key):
        return "\n".join(f"{d}={v}" for d, v in zip(active_dims, key))

    # Preserve first-seen order for both implementations and problem sizes.
    implementations = list(dict.fromkeys(r["implementation"].strip() for r in rows))
    size_keys = list(dict.fromkeys(size_key(r) for r in rows))

    impl_index = {name: i for i, name in enumerate(implementations)}
    size_index = {key: j for j, key in enumerate(size_keys)}

    matrix = np.full((len(implementations), len(size_keys)), np.nan)
    for r in rows:
        value = float(r["gflops"])
        # Non-finite results (Inf at tiny sizes where the timer rounds to zero)
        # and the GPU failure code (-1) are not real measurements: treat them
        # as missing so they neither colour a cell nor skew the scale.
        if not np.isfinite(value) or value < 0:
            value = np.nan
        matrix[impl_index[r["implementation"].strip()], size_index[size_key(r)]] = value

    size_labels = [size_label(key) for key in size_keys]
    return implementations, size_labels, matrix


def plot_heatmap(csv_path):
    """Render a single results CSV to a PNG heatmap beside the source file."""
    implementations, size_labels, matrix = load(csv_path)
    routine = routine_name(csv_path)
    n_rows, n_cols = matrix.shape

    # A perceptually-uniform, colorblind-safe sequential map for a magnitude
    # (GFLOP/s), on a linear scale spanning the data range. Missing cells (no
    # valid measurement) are drawn in a neutral grey.
    cmap = plt.get_cmap("plasma").copy()
    cmap.set_bad("#dddddd")

    if not np.any(np.isfinite(matrix)):
        raise ValueError("no finite GFLOP/s values to plot")
    vmin = np.nanmin(matrix)
    vmax = np.nanmax(matrix)
    if vmin == vmax:  # single distinct value: give Normalize a non-zero range
        vmax = vmin + 1.0
    norm = plt.Normalize(vmin=vmin, vmax=vmax)

    # Size the figure so cells stay legible as the number of problem sizes grows.
    fig_w = max(8.0, 0.55 * n_cols + 3.0)
    fig_h = max(3.0, 0.7 * n_rows + 2.5)
    fig, ax = plt.subplots(figsize=(fig_w, fig_h))

    image = ax.imshow(matrix, aspect="auto", cmap=cmap, norm=norm)

    ax.set_yticks(range(n_rows))
    ax.set_yticklabels(implementations)
    ax.set_xticks(range(n_cols))
    ax.set_xticklabels(size_labels, fontsize=8)

    ax.set_xlabel("Problem size")
    ax.set_title(f"{routine} performance (GFLOP/s)")

    # Recessive gridlines between cells for easier row/column tracing.
    ax.set_xticks(np.arange(-0.5, n_cols, 1), minor=True)
    ax.set_yticks(np.arange(-0.5, n_rows, 1), minor=True)
    ax.grid(which="minor", color="white", linewidth=1)
    ax.tick_params(which="minor", length=0)

    # Choose black/white text by the cell's luminance so the
    # value stays readable
    for i in range(n_rows):
        for j in range(n_cols):
            value = matrix[i, j]
            if np.isnan(value):
                ax.text(j, i, "n/a", ha="center", va="center",
                        color="#888888", fontsize=6)
                continue
            r, g, b, _ = cmap(norm(value))
            luminance = 0.299 * r + 0.587 * g + 0.114 * b
            text_color = "black" if luminance > 0.55 else "white"
            ax.text(j, i, f"{value:.1f}", ha="center", va="center",
                    color=text_color, fontsize=7)

    colorbar = fig.colorbar(image, ax=ax)
    colorbar.set_label("GFLOP/s")

    fig.tight_layout()

    out_path = os.path.splitext(csv_path)[0] + ".png"
    fig.savefig(out_path, dpi=150)
    plt.close(fig)
    return out_path


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("paths", nargs="*",
                        help="results CSV files or directories (default: auto-discover)")
    args = parser.parse_args(argv)

    files = find_csv_files(args.paths)
    if not files:
        print("No results_*.csv files found.", file=sys.stderr)
        return 1

    for csv_path in files:
        try:
            out_path = plot_heatmap(csv_path)
        except (OSError, ValueError, KeyError) as exc:
            print(f"Skipping {csv_path}: {exc}", file=sys.stderr)
            continue
        print(f"Wrote {out_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
