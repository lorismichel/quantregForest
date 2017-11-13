# checks on rhub
require(rhub)

validate_email()
platforms()

check(platform = "ubuntu-gcc-release")
check(platform = "macos-elcapitan-release")
check(platform = "linux-x86_64-rocker-gcc-san")
check(platform = "fedora-gcc-devel")
check(platform = "windows-x86_64-release")
