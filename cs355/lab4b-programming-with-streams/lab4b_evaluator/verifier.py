#!/usr/bin/python

from pathlib import Path
import shutil
import filecmp
import subprocess
import sys
import tempfile
from typing import List

QUESTION_DIRECTORY_PREFIX = 'Q'
TESTCASE_FILE_PREFIX = 'tc'
USER_TESTCASE_OUTPUT_FILE_SUFFIX = 'user_out'
TESTCASE_OUTPUT_FILE_SUFFIX = 'out'
DEFAULT_TIMEOUT_DURATION = 10

# Processes a single question folder.
def process_qn(folder: Path) -> None:
	solution_file_path: Path = folder / f"{folder.stem}.scm"
	testcase_files: List[Path] = list(folder.glob(f"{TESTCASE_FILE_PREFIX}*.scm"))

	if not solution_file_path.exists():
		print(f"Warning: {solution_file_path} does not exist. Please add it in.")
		print("Skipping this question.")
		return

	solution_file_contents = open(solution_file_path, 'r').read()

	if len(testcase_files) == 0:
		print(f"Warning: No testcases exist for {folder}. Please ask the instructor.")
		return

	with tempfile.TemporaryDirectory(prefix='testcase_gen_') as tmpdir:
		base_path = Path(tmpdir)
		for file_path in testcase_files:
			print(f"Processing {file_path}...")
			merged_filename = base_path / f'test_{file_path.stem}.scm'
			user_output_filename = base_path / f"{file_path.stem}_{USER_TESTCASE_OUTPUT_FILE_SUFFIX}.txt"
			expected_output_filename = folder / f"{file_path.stem}_{TESTCASE_OUTPUT_FILE_SUFFIX}.txt"

			with open(merged_filename, 'w') as merged_file, \
				open(file_path, 'r') as file:
				testcase_contents = file.read()
				if len(testcase_contents) == 0:
					print(f"Warning: {file_path} is an empty file. Please ask the instructor.")
				merged_file_contents = \
					solution_file_contents + \
					"\n\n;;;TESTCASE;;;\n\n" + \
					testcase_contents

				merged_file.write(merged_file_contents)

			try:
				compile_process = subprocess.run(
					["racket", merged_filename],
					stdout=subprocess.PIPE,
					stderr=subprocess.PIPE,
					timeout=DEFAULT_TIMEOUT_DURATION)

				if compile_process.returncode != 0:
					# If there is a compilation error
					print(f"Error: compilation error in {file_path}:")
					# Print the compilation error
					print(compile_process.stderr.decode())
					print("\n\nFile Contents:")
					print(merged_file_contents)
					sys.exit(1)
				else:
					with open(user_output_filename, 'w') as output_file:
						subprocess.run(
							["racket", merged_filename],
							stdout=output_file,
							stderr=subprocess.STDOUT,
							timeout=DEFAULT_TIMEOUT_DURATION)

					# Compare the generated output with the expected output
					if filecmp.cmp(user_output_filename, expected_output_filename, shallow=False):
						print(f"Test Case {file_path}: Passed \u2713")
					else:
						print(f"Test Case {file_path}: Failed \u03A7")
						print(f"Check for extra lines, spaces or characters")
						print(f"Differences in output for Test Case {file_path} (actual output on the left, expected output on the right):")
						# Use 'diff' to show the differences
						print("===================")
						subprocess.run(["sdiff", user_output_filename, expected_output_filename])
						print("===================")

			except subprocess.TimeoutExpired:
				print(f"Error: {file_path} timed out after {DEFAULT_TIMEOUT_DURATION} seconds.")

# Processes the question folder root directory.
def process_folders() -> None:
	question_folder_path = Path(sys.argv[1])
	folders: List[Path] = list(question_folder_path.glob("Q*"))
	folders = [ f for f in folders if f.is_dir() ]
	folders = sorted(folders)

	if len(folders) == 0:
		print("Error: No Question folders found. Exiting.")
		sys.exit(1)

	print("Directories to process: ")
	for f in folders:
		print(f"  {f}")

	for f in folders:
		print(f"Processing {f}...")
		process_qn(f)

def main():
	if len(sys.argv) < 2:
		print(f"""
Usage: {sys.argv[0]} <submission folder>

The directory structure must be as follows:

    <root>
    ├── {QUESTION_DIRECTORY_PREFIX}1 -- Directory for Question 1.
    │   ├── {QUESTION_DIRECTORY_PREFIX}1.scm -- Solution to the Qn.
    │   │   Will be used to generate outputs. MUST have same name as directory.
    │   ├── {TESTCASE_FILE_PREFIX}1.scm -- Testcase 1. Will be appended to the submitted file (Q1.scm) and
    │   │   run.
    │   ├── {TESTCASE_FILE_PREFIX}2.scm -- Testcase 2. Same as above.
    │   ├── {TESTCASE_FILE_PREFIX}3.scm -- Testcase 3. Same as above.
    │   ...
    │
    ├── {QUESTION_DIRECTORY_PREFIX}2
    │   └── ...
    ├── {QUESTION_DIRECTORY_PREFIX}3
    │   └── ...
    ...

If the solution is in the CURRENT directory, please run it as follows:

    python {sys.argv[0]} ./

""")
		sys.exit(1)

	if not shutil.which("racket"):
		print("Error: could not find the racket executable. Please install racket.")
		sys.exit(1)

	process_folders()

if __name__ == '__main__':
	main()