#!/usr/bin/env python3
"""Run Go and OxCaml benchmarks side-by-side and compare.

Usage:
    python3 bench/compare_go.py                   # default: 1s quota each
    python3 bench/compare_go.py --quota 3s        # tighter error bars
    python3 bench/compare_go.py --out compare.txt # custom report path
"""

from __future__ import annotations

import argparse
import datetime
import json
import re
import statistics
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
GO_REPO = Path("/home/xiaomin/dev/geo")

GREEN = "\033[32m"
RED = "\033[31m"
YELLOW = "\033[33m"
BOLD = "\033[1m"
DIM = "\033[2m"
RESET = "\033[0m"

ANSI_RE = re.compile(r"\033\[[0-9;]*m")


def strip_ansi(s: str) -> str:
    return ANSI_RE.sub("", s)


# Each entry maps an OxCaml bench bucket key to:
#   - a flat-bench Bench.Test.create-style name (no index), OR
#   - an indexed-bench prefix used by Bench.Test.create_indexed.
# Indexed benches expose nested {n: ns/op} maps; flat benches expose
# {0: ns/op} so the same data structure works for both.
_INDEXED_BENCHES = {
    "Cap": "RegionCovererCoveringCap",
    "Cell": "RegionCovererCoveringCell",
    "CellUnion": "RegionCovererCoveringCellUnion",
}

_FLAT_BENCHES = [
    "Sign",
    "RobustSignSimple",
    "PointArea",
    "PointFromLatLng",
    "LatLngGetDistance",
]

# Notes shown next to each bucket in the report.
_BENCH_NOTES = {
    "Cap": "(random cap area, ~0.1*avg_cell_area .. 4pi)",
    "Cell": "(single cell region, level in [max-n, max])",
    "CellUnion": "(2^n cells at level 22 + Normalize)",
    "Sign": "(predicates.Sign on three well-separated points)",
    "RobustSignSimple": "(predicates.RobustSign on three well-separated points)",
    "PointArea": "(triangle area on the canonical axis points)",
    "PointFromLatLng": "(latlng->point on a fixed E7 lat/lng)",
    "LatLngGetDistance": "(latlng distance with a varying second argument)",
}


def run_oxcaml(quota: str) -> dict[str, dict[int, float]]:
    out: dict[str, dict[int, float]] = {}
    for exe_name in ("bench_region_coverer", "bench_misc"):
        exe = REPO_ROOT / f"_build/default/bench/{exe_name}.exe"
        if not exe.exists():
            print(f"building OxCaml {exe_name}...", file=sys.stderr)
            subprocess.run(
                ["dune", "build", f"bench/{exe_name}.exe"],
                cwd=REPO_ROOT,
                check=True,
            )
        print(f"running OxCaml {exe_name} (quota={quota})...", file=sys.stderr)
        result = subprocess.run(
            [str(exe), "-ascii", "-quota", quota],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        _parse_oxcaml(result.stdout, out)
    return out


def _parse_oxcaml(stdout: str, out: dict[str, dict[int, float]]) -> None:
    indexed_pat = re.compile(r"(\w+):(\d+)\s+.*?([\d_,]+\.\d+)ns")
    flat_pat = re.compile(r"^\s*(\w+)\s+([\d_,]+\.\d+)ns")
    indexed_to_key = {v: k for k, v in _INDEXED_BENCHES.items()}
    for line in stdout.splitlines():
        m = indexed_pat.search(line)
        if m:
            bench_name = m.group(1)
            n = int(m.group(2))
            ns = _parse_ns(m.group(3))
            key = indexed_to_key.get(bench_name)
            if key is not None:
                out.setdefault(key, {})[n] = ns
            continue
        m = flat_pat.search(line)
        if m:
            bench_name = m.group(1)
            if bench_name in _FLAT_BENCHES:
                ns = _parse_ns(m.group(2))
                out.setdefault(bench_name, {})[0] = ns


def _parse_ns(s: str) -> float:
    return float(s.replace("_", "").replace(",", ""))


def run_go(quota_seconds: float) -> dict[str, dict[int, float]]:
    out: dict[str, dict[int, float]] = {}
    indexed_names = "|".join(
        f"^Benchmark{name}$" for name in _INDEXED_BENCHES.values()
    )
    flat_names = "|".join(f"^Benchmark{name}$" for name in _FLAT_BENCHES)
    pattern = "|".join([indexed_names, flat_names])
    print(f"running Go bench (benchtime={quota_seconds}s)...", file=sys.stderr)
    cmd = [
        "go",
        "test",
        "-bench",
        pattern,
        f"-benchtime={quota_seconds}s",
        "-run",
        "^$",
        "./s2",
    ]
    result = subprocess.run(
        cmd, cwd=GO_REPO, capture_output=True, text=True, check=True
    )
    _parse_go(result.stdout, out)
    return out


def _parse_go(stdout: str, out: dict[str, dict[int, float]]) -> None:
    # Indexed Cap / Cell: BenchmarkRegionCovererCoveringCap/Cap2-8 ... ns/op
    p_cap_cell = re.compile(
        r"BenchmarkRegionCovererCovering(Cap|Cell)/\1(\d+)-\d+\s+\d+\s+([\d.]+)\s+ns/op"
    )
    # CellUnion: BenchmarkRegionCovererCoveringCellUnion/CellUnion-2-cells-8 ... ns/op
    p_cu = re.compile(
        r"BenchmarkRegionCovererCoveringCellUnion/CellUnion-(\d+)-cells-\d+\s+\d+\s+([\d.]+)\s+ns/op"
    )
    # Flat: BenchmarkSign-8                123456789                 9.5 ns/op
    p_flat = re.compile(
        rf"^Benchmark(?P<name>{'|'.join(_FLAT_BENCHES)})-\d+\s+\d+\s+([\d.]+)\s+ns/op"
    )
    for line in stdout.splitlines():
        m = p_cap_cell.match(line)
        if m:
            bench, n, ns = m.group(1), int(m.group(2)), float(m.group(3))
            out.setdefault(bench, {})[n] = ns
            continue
        m = p_cu.match(line)
        if m:
            size = int(m.group(1))
            n = size.bit_length() - 1  # size = 2^n
            ns = float(m.group(2))
            out.setdefault("CellUnion", {})[n] = ns
            continue
        m = p_flat.match(line)
        if m:
            out.setdefault(m.group("name"), {})[0] = float(m.group(2))


def color_ratio(ratio: float, use_color: bool) -> str:
    label = f"{ratio:.2f}x"
    if not use_color:
        return label
    if ratio < 0.9:
        return f"{GREEN}{label}{RESET}"
    if ratio > 1.1:
        return f"{RED}{label}{RESET}"
    return f"{YELLOW}{label}{RESET}"


def format_table(rows: list[tuple[str, ...]], use_color: bool) -> list[str]:
    widths = [max(len(strip_ansi(r[i])) for r in rows) for i in range(len(rows[0]))]
    lines = []
    for idx, row in enumerate(rows):
        parts = []
        for i, cell in enumerate(row):
            pad = widths[i] - len(strip_ansi(cell))
            parts.append(cell + " " * pad if i == 0 else " " * pad + cell)
        lines.append("  ".join(parts))
        if idx == 0:
            lines.append("  ".join("-" * widths[i] for i in range(len(row))))
    return lines


def summarize(
    go: dict[str, dict[int, float]],
    ox: dict[str, dict[int, float]],
    use_color: bool,
) -> tuple[list[str], dict]:
    lines: list[str] = []
    summary: dict = {"per_bench": {}, "overall": {}}

    bench_order = list(_INDEXED_BENCHES.keys()) + list(_FLAT_BENCHES)
    ratios_ox_vs_go: list[float] = []

    for bench in bench_order:
        go_d = go.get(bench, {})
        ox_d = ox.get(bench, {})
        if not go_d or not ox_d:
            continue
        common_keys = set(go_d) & set(ox_d)
        if not common_keys:
            continue
        common = sorted(common_keys)
        is_indexed = bench in _INDEXED_BENCHES
        lines.append("")
        lines.append(
            f"{BOLD if use_color else ''}{bench}{RESET if use_color else ''} "
            f"{DIM if use_color else ''}{_BENCH_NOTES.get(bench, '')}{RESET if use_color else ''}"
        )
        if is_indexed:
            rows: list[tuple[str, ...]] = [
                ("n", "Go ns/op", "OxCaml ns/op", "OxCaml/Go")
            ]
        else:
            rows = [("Go ns/op", "OxCaml ns/op", "OxCaml/Go")]
        per_bench_ox_go: list[float] = []
        for n in common:
            r = ox_d[n] / go_d[n]
            per_bench_ox_go.append(r)
            ratios_ox_vs_go.append(r)
            cells = (
                f"{go_d[n]:,.1f}",
                f"{ox_d[n]:,.1f}",
                color_ratio(r, use_color),
            )
            rows.append((str(n),) + cells if is_indexed else cells)
        lines.extend(format_table(rows, use_color))
        if len(per_bench_ox_go) > 1:
            med = statistics.median(per_bench_ox_go)
            gm = statistics.geometric_mean(per_bench_ox_go)
            lines.append(
                f"  {DIM if use_color else ''}OxCaml/Go median:{RESET if use_color else ''} "
                f"{color_ratio(med, use_color)} "
                f"{DIM if use_color else ''}geomean:{RESET if use_color else ''} "
                f"{color_ratio(gm, use_color)}"
            )
        summary["per_bench"][bench] = {
            "go_ns_per_op": go_d,
            "ox_ns_per_op": ox_d,
            "ratios_ox_vs_go": dict(zip(common, per_bench_ox_go)),
        }

    if ratios_ox_vs_go:
        lines.append("")
        lines.append(
            f"{BOLD if use_color else ''}overall geomean:{RESET if use_color else ''}"
        )
        gm = statistics.geometric_mean(ratios_ox_vs_go)
        lines.append(
            f"  OxCaml/Go = {color_ratio(gm, use_color)} "
            f"over {len(ratios_ox_vs_go)} sub-benchmarks"
        )
        summary["overall"]["ox_vs_go_geomean"] = gm
    return lines, summary


def main() -> None:
    p = argparse.ArgumentParser()
    p.add_argument("--quota", default="1s", help="time per sub-benchmark (default 1s)")
    p.add_argument(
        "--out",
        default="bench/compare.txt",
        help="where to save plain-text report (default: bench/compare.txt)",
    )
    p.add_argument(
        "--json",
        default="bench/compare.json",
        help="where to save JSON data (default: bench/compare.json)",
    )
    p.add_argument("--no-color", action="store_true")
    args = p.parse_args()

    quota_s = args.quota
    quota_seconds = float(quota_s.rstrip("s"))

    go = run_go(quota_seconds)
    ox = run_oxcaml(quota_s)

    use_color = sys.stdout.isatty() and not args.no_color
    lines, summary = summarize(go, ox, use_color)
    for line in lines:
        print(line)

    plain_lines, _ = summarize(go, ox, use_color=False)
    out_path = REPO_ROOT / args.out
    out_path.parent.mkdir(parents=True, exist_ok=True)
    header = [
        f"# s2 Go-vs-OxCaml comparison",
        f"# generated: {datetime.datetime.now().isoformat(timespec='seconds')}",
        f"# quota per sub-bench: {quota_s}",
        "",
    ]
    out_path.write_text("\n".join(header + plain_lines) + "\n")
    print(f"\nsaved plain-text report to {out_path}", file=sys.stderr)

    json_path = REPO_ROOT / args.json
    json_path.write_text(
        json.dumps(
            {
                "quota": quota_s,
                "timestamp": datetime.datetime.now().isoformat(timespec="seconds"),
                **summary,
            },
            indent=2,
            default=str,
        )
    )
    print(f"saved JSON data to {json_path}", file=sys.stderr)


if __name__ == "__main__":
    main()
