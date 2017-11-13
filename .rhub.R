# checks on rhub the build for different plateforms
require(rhub)

validate_email()
platforms()

check(platform = "ubuntu-gcc-release")
check(platform = "linux-x86_64-rocker-gcc-san")
check(platform = "fedora-gcc-devel")
check(platform = "windows-x86_64-release")
