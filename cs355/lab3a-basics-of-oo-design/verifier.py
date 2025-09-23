#!/usr/bin/python

from pathlib import Path
import re
import json
import shutil
import filecmp
import subprocess
import sys
import tempfile
from typing import List

QUESTION_DIRECTORY_PREFIX = 'Q'
SCHEME_FILE_EXT = '.scm'
SOLUTION_FILE_NAME_STEM = 'sol'
TESTCASE_FILE_PREFIX = 'tc'
USER_TESTCASE_OUTPUT_FILE_SUFFIX = 'user_out'
TESTCASE_OUTPUT_FILE_SUFFIX = 'out'
DEFAULT_TIMEOUT_DURATION = 20
METADATA_FILE_NAME = 'metadata.json'

# Processes a single question folder.
def process_qn_scheme(folder: Path) -> None:
	if not shutil.which("racket"):
		print("Error: could not find the racket executable. Please install racket.")
		sys.exit(1)

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

def check_naming_schemes_java(text: str):
	class_name_regex: re.Pattern = re.compile(
		r"(public\s+)?"
		r"(abstract\s+)?"
		r"class\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+"
		r"(extends\s+([a-zA-Z_][a-zA-Z0-9_]*))?\s*\{",
		re.MULTILINE)

	CLASS_NAME_OFFSET = 3
	EXTENDS_NAME_OFFSET = 5

	print("You have the following classes defined in this file:")
	for match in class_name_regex.finditer(text):
		class_name = match[CLASS_NAME_OFFSET]
		print(f"  {class_name}: ")
		print(f"    Declaration: '{match[0]}'")
		ok: bool = True
		if not class_name[0].isupper():
			print("    @@ Incorrect naming convention?")
			ok = False
		if '\n' in match[0] or '\r\n' in match[0]:
			print("    @@ All parts of class declaration and bracket not in same line?")
			ok = False
		if ok:
			print("    \u2713 Automated checks passed.")
	print('')

# Processes a single question folder.
def process_qn_java(
	folder: Path,
	main_java_file: str,
	main_java_class: str,
	has_testcases: bool) -> None:

	if not shutil.which("java"):
		print("Error: could not find the java executable. Please install java.")
		sys.exit(1)

	if not shutil.which("javac"):
		print("Error: could not find the javac compiler. Please install.")
		sys.exit(1)

	solution_path: Path = folder / SOLUTION_FILE_NAME_STEM
	solution_file_path: Path = solution_path / main_java_file

	if not solution_file_path.exists():
		print(f"Error: {solution_path} does not exist. Please add it in.")
		print("Skipping this question.")
		return

	if has_testcases:
		print("Error: testcases are not supported yet.")
		sys.exit(1)

	with tempfile.TemporaryDirectory(prefix='testcase_gen_') as tmpdir:
		base_path = Path(tmpdir)
		build_path = base_path

		try:
			compile_process = subprocess.run(
				['javac', '-d', build_path, main_java_file],
				cwd=solution_path,
				stdout=subprocess.PIPE,
				stderr=subprocess.PIPE,
				timeout=DEFAULT_TIMEOUT_DURATION)

			if compile_process.returncode != 0:
				# If there is a compilation error
				print(f"Error: compilation error in {solution_path} ({main_java_file}):")
				# Print the compilation error
				print(compile_process.stderr.decode())
				sys.exit(1)

			print("\u2713 Compilation Succeeded")
			run_process = subprocess.run(
				['java', '-cp', base_path, main_java_class],
				stdout=subprocess.PIPE,
				stderr=subprocess.STDOUT,
				timeout=DEFAULT_TIMEOUT_DURATION)

			if run_process.returncode != 0:
				# If there is a compilation error
				print(f"Error: runtime error in {solution_path} ({main_java_file}):")
				# Print the compilation error
				print(run_process.stderr.decode())
				sys.exit(1)

			print(f"Output:\n{run_process.stdout.decode()}")

		except subprocess.TimeoutExpired:
			print(f"Error: {solution_path} ({main_java_file}) timed out after {DEFAULT_TIMEOUT_DURATION} seconds.")

	for file_path in solution_path.glob("*.java"):
		print(f"@@ {file_path.name}:")
		with open(file_path, 'r') as file:
			text = file.read()
			check_naming_schemes_java(text)

# Processes the question folder root directory.
def process_folders() -> None:
	question_folder_path = Path(sys.argv[1])

	lab_desc = json.load(open(METADATA_FILE_NAME, 'r'))

	if not question_folder_path.exists():
		print(f"Error: Output folder '{question_folder_path}' does not exist.")
		sys.exit(1)

	print(f"Lab Name: {lab_desc['lab_name']}")

	questions = lab_desc['questions']

	if len(questions) == 0:
		print("Error: No Questions found. Exiting.")
		sys.exit(1)

	print("Questions to process: ")
	for q in questions:
		print(f"  {q['name']} (type: {q['type']})")

	print()

	for q in questions:
		qn_inpath = question_folder_path / q['name']
		print(f"Processing {q['name']}...")
		if q['type'] == 'scheme':
			process_qn_scheme(qn_inpath)
		elif q['type'] == 'java':
			process_qn_java(qn_inpath, q['main_file'], q['main_class'], q['has_testcases'])
		else:
			print(f"Error: unknown question type: '{q['type']}'")

def main():
	if len(sys.argv) < 2:
		print(f"""
Usage: {sys.argv[0]} <question folder>

The directory structure must be as follows:

    <root>
    ├── {QUESTION_DIRECTORY_PREFIX}1/ -- Directory for Question 1.
    │   ├── {SOLUTION_FILE_NAME_STEM}/ -- Solution to the Qn. Will be used to
    │   │   generate outputs. Place your java files here.
    │   ...
    │
    │
    ├── {QUESTION_DIRECTORY_PREFIX}2/
    │   ├── {SOLUTION_FILE_NAME_STEM}/
    │   ...
    │
    ├── {QUESTION_DIRECTORY_PREFIX}3/
    │   └── ...
    ...

If the solution is in the CURRENT directory, please run it as follows:

    python {sys.argv[0]} ./
""")
		sys.exit(1)

	process_folders()


if __name__ == '__main__':
	main()