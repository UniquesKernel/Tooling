import subprocess
import sys
import os

def get_reverse_dependencies(package, level=0, visited=None, use_yum=True):
    if visited is None:
        visited = set()

    # Prevent revisiting the same package
    if package in visited:
        return
    visited.add(package)

    # Determine the command based on the package manager
    command = ['repoquery', '--installed', '--whatrequires', package] if use_yum else ['apt-rdepends', '--reverse', '--state-follow=Installed', '--state-show=Installed', package]

    # Call the package manager to find reverse dependencies
    try:
        output = subprocess.check_output(command, text=True)
    except subprocess.CalledProcessError as e:
        print(f"Error querying package {package}: {e}")
        return

    # Parse output
    dependencies = output.strip().split('\n')
    for dep in dependencies:
        dep = dep.split()[-1] if not use_yum else dep  # Extract package name for apt output
        if dep and dep not in visited:  # Ignore empty lines and already visited packages
            print('  ' * level + dep)
            get_reverse_dependencies(dep, level + 1, visited, use_yum)

# Check for available package manager and set flag
use_yum = os.path.exists('/usr/bin/yum')
use_apt = os.path.exists('/usr/bin/apt')

if not (use_yum or use_apt):
    print("Neither yum nor apt is available on this system.")
    sys.exit(1)

# Take package name from command-line argument
if len(sys.argv) != 2:
    print("Usage: python3 trace.py [package_name]")
    sys.exit(1)

package_name = sys.argv[1]
get_reverse_dependencies(package_name, use_yum=use_yum)

