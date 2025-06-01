import subprocess
import time
import argparse
import os
import sys
import csv
from datetime import datetime

CSV_FILENAME = "performance.csv"
CSV_HEADERS = [
    "Timestamp",
    "File Path",
    "Arguments",
    "Execution Time (s)",
    "Return Code",
]


def _write_to_csv(timestamp, file_path, arguments_str, exec_time, ret_code):
    """
    Helper function to write a single data row to the CSV file.
    It handles header creation if the file is new or empty.

    Args:
        timestamp (str): Timestamp of the log entry.
        file_path (str): Path of the executed file.
        arguments_str (str): String representation of arguments passed to the file.
        exec_time (float): Execution time in seconds.
        ret_code (int or str): Return code of the process.
    """
    # Prepare the data row for CSV
    data_row = [
        timestamp,
        file_path,
        arguments_str,
        f"{exec_time:.4f}",  # Format execution time
        str(ret_code),  # Ensure return code is a string
    ]

    file_exists = os.path.exists(CSV_FILENAME)
    is_empty = False
    if file_exists:
        try:
            # Check if the file exists but is empty
            if os.path.getsize(CSV_FILENAME) == 0:
                is_empty = True
        except OSError:
            # If getsize fails (e.g., permissions, special file), assume it might need headers
            is_empty = True

    try:
        # Open the CSV file in append mode ('a')
        # newline='' prevents extra blank rows in the CSV
        # encoding='utf-8' is good practice
        with open(CSV_FILENAME, "a", newline="", encoding="utf-8") as csvfile:
            writer = csv.writer(csvfile, quoting=csv.QUOTE_MINIMAL)
            # Write headers if the file is new or was empty
            if not file_exists or is_empty:
                writer.writerow(CSV_HEADERS)
            writer.writerow(data_row)
        print(f"Results appended to '{CSV_FILENAME}'")
    except IOError as e:
        print(f"Error writing to CSV file '{CSV_FILENAME}': {e}")


def run_and_time_file(file_path, args_for_file):
    """
    Executes a given file, times its execution, and logs results to console and CSV.
    Stdout and Stderr of the executed file are NOT captured by this script.

    Args:
        file_path (str): The path to the file to execute.
        args_for_file (list): A list of arguments to pass to the executed file.
    """
    # Get current timestamp for logging
    current_timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S.%f")[
        :-3
    ]  # Includes milliseconds
    arguments_str = " ".join(args_for_file) if args_for_file else ""

    # Initialize default values for logging variables
    execution_time_val = 0.0
    return_code_val = -1  # Default to a general error code for the script itself
    start_time = None  # To ensure it's defined in all paths

    if not os.path.exists(file_path):
        error_message = f"Error: File not found at '{file_path}'"
        print(error_message)
        stderr_data = error_message  # Log this script's error
        return_code_val = -10  # Custom error code for file not found by this script
        _write_to_csv(
            current_timestamp,
            file_path,
            arguments_str,
            execution_time_val,
            return_code_val,
        )
        return

    # Check for execute permissions and try to set them if necessary
    if not os.access(file_path, os.X_OK):
        try:
            # Add execute permission for user, group, others
            os.chmod(file_path, os.stat(file_path).st_mode | 0o111)
            print(f"Note: Added execute permission to '{file_path}'")
        except OSError as e:
            warning_message = f"Warning: Could not set execute permission for '{file_path}': {e}. Execution may fail."
            print(warning_message)
            return_code_val = -11  # Custom error code for chmod failure
            _write_to_csv(
                current_timestamp,
                file_path,
                arguments_str,
                execution_time_val,
                return_code_val,
            )
            return  # Stop if we can't ensure it's executable

    # --- Prepare command for execution ---
    command = []
    # If it's a Python script, explicitly use the current Python interpreter
    if file_path.endswith(".py"):
        command.append(sys.executable)  # Use the same interpreter running this script
    command.append(file_path)
    if args_for_file:
        command.extend(args_for_file)

    print(f"Executing: {' '.join(command)}")
    print(f"--- Output of '{file_path}' will appear below ---")
    start_time = time.perf_counter()  # Record start time just before execution

    # --- Execute the file ---
    try:
        # Execute the file. capture_output=False means stdout/stderr go to parent.
        # check=False means it won't raise CalledProcessError for non-zero exit codes.
        result = subprocess.run(
            command,
            capture_output=False,  # Key change: Do not capture stdout/stderr
            check=False,
        )
        end_time = time.perf_counter()
        execution_time_val = end_time - start_time
        return_code_val = result.returncode

        # stdout_data and stderr_data remain empty as they are not captured from subprocess

        # Print execution report to console (excluding subprocess stdout/stderr)
        print(f"\n--- Execution Report for '{file_path}' ---")
        print(f"Execution completed in: {execution_time_val:.4f} seconds")
        print(f"Return Code: {return_code_val}")

    except FileNotFoundError:
        # This error occurs if the command (e.g., python interpreter) or the script file itself isn't found at execution time.
        if start_time:  # If timing started
            end_time = time.perf_counter()
            execution_time_val = end_time - start_time
        error_message = f"Error: Could not execute. The command or interpreter for '{file_path}' was not found."
        print(error_message)
        stderr_data = error_message  # Log this script's error
        return_code_val = -2  # Custom error code for execution FileNotFoundError
    except PermissionError:
        # This error occurs if there's a permission issue during the actual execution attempt.
        if start_time:
            end_time = time.perf_counter()
            execution_time_val = end_time - start_time
        error_message = (
            f"Error: Permission denied when trying to execute '{file_path}'."
        )
        print(error_message)
        stderr_data = error_message  # Log this script's error
        return_code_val = -3  # Custom error code for execution PermissionError
    except Exception as e:
        # Catch any other unexpected errors during subprocess.run or time recording
        if start_time:
            end_time = time.perf_counter()
            execution_time_val = end_time - start_time
        else:  # If start_time was not even set
            execution_time_val = 0
        error_message = f"An unexpected error occurred during execution: {e}"
        print(error_message)
        stderr_data = error_message  # Log this script's error
        return_code_val = -4  # Custom error code for other exceptions
    finally:
        print(f"--- End of output for '{file_path}' ---")

    # --- Log results to CSV ---
    # stdout_data and stderr_data for the CSV will be empty unless this script generated an error message for stderr_data
    _write_to_csv(
        current_timestamp,
        file_path,
        arguments_str,
        execution_time_val,
        return_code_val,
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Run a file, time its execution, and log to console and performance.csv. Does NOT capture stdout/stderr of the executed program."
    )
    parser.add_argument("file_path", help="The path to the file to execute.")
    parser.add_argument(
        "args_for_file",
        nargs=argparse.REMAINDER,  # Collects all remaining arguments into a list
        help="Arguments to pass to the file being executed.",
    )

    args = parser.parse_args()
    run_and_time_file(args.file_path, args.args_for_file)
