import subprocess
import time
import os
import sys
import csv
from datetime import datetime

try:
    import matplotlib.pyplot as plt
except ImportError:
    print(
        "matplotlib is not installed. Please install it to generate plots: pip install matplotlib"
    )
    plt = None

# --- Configuration ---
SCHEME_SCRIPT_FILENAME = "tailrecursive.scm"
CSV_FILENAME = "scheme_performance.csv"  # CSV filename for results

# Paths to interpreters/programs (User should verify/update these if not in PATH)
# For 'custom_scheme', it's assumed to be relative to where this script is run.
# If your custom program doesn't use '--script', change its 'args_template' below.
INTERPRETER_PATHS = {
    "racket": "/opt/homebrew/bin/racket",
    "chicken": "/opt/homebrew/opt/chicken/bin/csi",
    "chez": "/opt/homebrew/bin/chez",
    "custom_scheme": os.path.join("build", "build", "scheme_program"),
}

# Defines how each interpreter/program is called
# 'label' is for plots and CSV, 'executable_key' points to INTERPRETER_PATHS,
# 'args_template' is a list where SCHEME_SCRIPT_FILENAME will be substituted if "{script}" is present.
# Adjust 'args_template' if an interpreter takes the script name differently (e.g., no --script flag).
COMMAND_CONFIGS = [
    {
        "label": "Racket",
        "executable_key": "racket",
        "args_template": ["--script", "{script}"],
    },
    {
        "label": "CHICKEN",  # Assumes 'chicken' command itself implies script execution or use 'csi -s'
        "executable_key": "chicken",
        "args_template": [
            "-s",
            "{script}",
        ],  # Common for CHICKEN tools like csi or chicken-csi
    },
    {
        "label": "Chez Scheme",
        "executable_key": "chez",
        "args_template": ["--script", "{script}"],
    },
    {
        "label": "Custom Build",
        "executable_key": "custom_scheme",
        "args_template": [
            "--script",
            "{script}",
        ],  # Assumption: your program also uses --script
        # If not, change to e.g. ["{script}"]
    },
]

CSV_HEADERS = [
    "Timestamp",
    "Interpreter/Program",
    "Arguments",
    "Execution Time (s)",
    "Return Code",
    "Label",  # Added for clarity in CSV if needed
]
# --- End Configuration ---


def _write_to_csv(
    timestamp, executable_path, arguments_str, exec_time, ret_code, label
):
    """
    Helper function to write a single data row to the CSV file.
    It handles header creation if the file is new or empty.
    """
    data_row = [
        timestamp,
        executable_path,
        arguments_str,
        f"{exec_time:.4f}",
        str(ret_code),
        label,
    ]

    file_exists = os.path.exists(CSV_FILENAME)
    is_empty = False
    if file_exists:
        try:
            if os.path.getsize(CSV_FILENAME) == 0:
                is_empty = True
        except OSError:
            is_empty = True

    try:
        with open(CSV_FILENAME, "a", newline="", encoding="utf-8") as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_MINIMAL)
            if not file_exists or is_empty:
                writer.writerow(CSV_HEADERS)
            writer.writerow(data_row)
        print(f"Results appended to '{CSV_FILENAME}' for {label}")
    except IOError as e:
        print(f"Error writing to CSV file '{CSV_FILENAME}' for {label}: {e}")


def run_and_time_command(label, executable_path, base_args, scheme_script_path):
    """
    Executes a given command, times its execution, and logs results.
    Returns (execution_time, return_code) or (None, error_code) on failure.
    """
    current_timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S.%f")[:-3]

    # Substitute script path into arguments
    processed_args = []
    for arg in base_args:
        if arg == "{script}":
            processed_args.append(scheme_script_path)
        else:
            processed_args.append(arg)
    arguments_str = " ".join(processed_args)

    execution_time_val = 0.0
    return_code_val = -1  # Default error for this script
    start_time = None

    if not os.path.exists(executable_path):
        error_message = (
            f"Error: Executable not found at '{executable_path}' for {label}."
        )
        print(error_message)
        _write_to_csv(
            current_timestamp,
            executable_path,
            arguments_str,
            0.0,
            -10,
            label,  # Custom error code
        )
        return None, -10

    # Check for execute permissions for the executable (interpreter/program)
    if not os.access(executable_path, os.X_OK):
        try:
            # Attempt to add execute permission (useful for local builds)
            # For system interpreters, this might fail or be unnecessary if already set
            current_mode = os.stat(executable_path).st_mode
            os.chmod(executable_path, current_mode | 0o111)  # Add u+x, g+x, o+x
            print(f"Note: Added execute permission to '{executable_path}' for {label}.")
        except OSError as e:
            warning_message = f"Warning: Could not set execute permission for '{executable_path}' for {label}: {e}. Execution may fail."
            print(warning_message)
            # Log this attempt, but proceed to try running anyway
            _write_to_csv(
                current_timestamp,
                executable_path,
                arguments_str,
                0.0,
                -11,
                label,  # Custom error code
            )
            # Depending on system, execution might still be possible or might fail below

    command_to_run = [executable_path] + processed_args

    print(f"\n🚀 Executing ({label}): {' '.join(command_to_run)}")
    print(
        f"--- Output of '{os.path.basename(executable_path)}' for {label} will appear below ---"
    )
    start_time = time.perf_counter()

    try:
        result = subprocess.run(
            command_to_run,
            capture_output=False,
            check=False,  # stdout/stderr go to parent
        )
        end_time = time.perf_counter()
        execution_time_val = end_time - start_time
        return_code_val = result.returncode

        print(
            f"--- Execution Report for {label} ({os.path.basename(executable_path)}) ---"
        )
        print(f"Execution completed in: {execution_time_val:.4f} seconds")
        print(f"Return Code: {return_code_val}")

    except FileNotFoundError:
        if start_time:
            execution_time_val = time.perf_counter() - start_time
        error_message = f"Error: Could not execute for {label}. The command '{executable_path}' or interpreter was not found."
        print(error_message)
        return_code_val = -20  # Custom error for execution FileNotFoundError
    except PermissionError:
        if start_time:
            execution_time_val = time.perf_counter() - start_time
        error_message = f"Error: Permission denied when trying to execute '{executable_path}' for {label}."
        print(error_message)
        return_code_val = -30  # Custom error for execution PermissionError
    except Exception as e:
        if start_time:
            execution_time_val = time.perf_counter() - start_time
        error_message = (
            f"An unexpected error occurred during execution for {label}: {e}"
        )
        print(error_message)
        return_code_val = -40  # Custom error for other exceptions
    finally:
        print(
            f"--- End of output for {label} ({os.path.basename(executable_path)}) ---"
        )

    _write_to_csv(
        current_timestamp,
        executable_path,
        arguments_str,
        execution_time_val,
        return_code_val,
        label,
    )
    return execution_time_val if return_code_val == 0 else None, return_code_val


def plot_results(results_data, scheme_script_name):
    """
    Generates and shows a bar plot of execution times.
    results_data is a list of tuples: (label, time)
    """
    if not plt:
        print("Matplotlib not available. Skipping plot generation.")
        return
    if not results_data:
        print("No successful results to plot.")
        return

    labels = [res[0] for res in results_data]
    times = [res[1] for res in results_data]

    # Ensure labels and times are not empty before proceeding
    if not labels or not times:
        print("No valid data for plotting.")
        return

    plt.figure(figsize=(12, 7))
    colors = plt.cm.viridis(
        [
            i / float(len(labels) - 1 if len(labels) > 1 else 1)
            for i in range(len(labels))
        ]
    )  # Get distinct colors

    bars = plt.bar(labels, times, color=colors)

    plt.ylabel("Execution Time (s) ⏱️")
    plt.xlabel("Scheme Interpreter/Program 💻")
    plt.title(f'Execution Time Comparison for "{scheme_script_name}" 📊')
    plt.xticks(rotation=45, ha="right")  # Rotate labels for better fit
    plt.grid(axis="y", linestyle="--", alpha=0.7)
    plt.tight_layout()  # Adjust layout

    # Add text labels on top of each bar
    max_time = max(times) if times else 0.01
    for bar in bars:
        yval = bar.get_height()
        plt.text(
            bar.get_x() + bar.get_width() / 2.0,
            yval + 0.01 * max_time,  # Position text slightly above bar
            f"{yval:.4f}s",
            ha="center",
            va="bottom",
            fontsize=9,
        )

    plot_filename = f"runtime_comparison_{os.path.splitext(scheme_script_name)[0]}.png"
    try:
        plt.savefig(plot_filename)
        print(f"\n📈 Plot saved as '{plot_filename}'")
    except Exception as e:
        print(f"Error saving plot: {e}")
    plt.show()


def main():
    """
    Main function to orchestrate running commands and plotting results.
    """
    if not os.path.exists(SCHEME_SCRIPT_FILENAME):
        print(
            f"Error: Scheme script '{SCHEME_SCRIPT_FILENAME}' not found. "
            "Please ensure it's in the current directory or update the path."
        )
        return
    if not os.access(SCHEME_SCRIPT_FILENAME, os.R_OK):
        print(f"Error: Scheme script '{SCHEME_SCRIPT_FILENAME}' is not readable.")
        return

    results_for_plot = []

    print(f"Benchmarking Scheme script: '{SCHEME_SCRIPT_FILENAME}'")
    print(f"Logging results to: '{CSV_FILENAME}'")

    for config in COMMAND_CONFIGS:
        label = config["label"]
        executable_key = config["executable_key"]
        args_template = config["args_template"]

        executable_path = INTERPRETER_PATHS.get(executable_key)
        if not executable_path:
            print(
                f"Configuration error: Executable path for key '{executable_key}' not found in INTERPRETER_PATHS."
            )
            continue

        # Resolve to absolute path for custom_scheme if it's relative, for robustness
        if executable_key == "custom_scheme" and not os.path.isabs(executable_path):
            executable_path = os.path.abspath(executable_path)

        exec_time, ret_code = run_and_time_command(
            label, executable_path, args_template, SCHEME_SCRIPT_FILENAME
        )

        if exec_time is not None and ret_code == 0:  # Only plot successful runs
            results_for_plot.append((label, exec_time))
        elif (
            exec_time is None
        ):  # Indicates an error before or during execution managed by run_and_time_command
            print(
                f"Skipping plot entry for {label} due to execution error (return code: {ret_code})."
            )
        elif ret_code != 0:  # Subprocess ran but returned an error
            print(
                f"Skipping plot entry for {label} due to non-zero return code: {ret_code}."
            )

    if results_for_plot:
        plot_results(results_for_plot, SCHEME_SCRIPT_FILENAME)
    else:
        print(
            "\nNo successful executions to plot. Please check logs and configurations."
        )


if __name__ == "__main__":
    main()
