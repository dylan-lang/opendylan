#!/usr/bin/env ruby
# Run in a source directory. Make sure debian/changelog is updated if needed.
# When building for x86 (32 bit), make sure MPS sources are available in the mps-kit subdirectory.
# When building for x86_64, make sure you increase the stack size (eg, ulimit -s 20000)

# Required dependencies:
# sudo apt-get install automake gcc libgc-dev rubygems ruby-dev
# sudo gem install fpm

require 'fileutils'
include FileUtils

# Create staging area
STAGING_DIR=`mktemp -d`.chomp
INSTALL_DIR="/usr/lib/opendylan"

additional_fpm_flags=""
configure_flags="--prefix=#{INSTALL_DIR}"

mkdir_p "#{STAGING_DIR}/usr/bin"

#system("make clean 2> /dev/null") # Clean up in case we start from a dirty directory
system("./autogen.sh") # Will return errors, but safe to ignore

# Add MPS configure flag for 32 bit x86
if(`uname -m` =~ /i686/)
  configure_flags << " --with-gc=mps --with-gc-path=#{Dir.pwd}/mps-kit"
  jamfile="sources/jamfiles/x86-linux-build.jam"
else
  configure_flags << " --with-gc=boehm"
  jamfile="sources/jamfiles/x86_64-linux-build.jam"
end

# Add libgc dependency for 64 bit x86
if(`uname -m` =~ /x86_64/)
  additional_fpm_flags << ' -d "libgc-dev (>= 0)"'
end

# Configure and build first 2 stages
puts "Configuring with #{configure_flags}"
system("./configure #{configure_flags}") || exit(1)
system("make 2-stage-bootstrap") || exit(1)

# Overwrite the rpath, so the packaged binaries use the absolute
# install dir instead (fixes problems when /proc is missing, eg. in a chroot).
original_jamfile_contents = File.read(jamfile)
File.open(jamfile, 'w') { |f| f.write(original_jamfile_contents.gsub('\\\\$ORIGIN/..', INSTALL_DIR)) }
at_exit do
  # Restore the original jamfile when we quit
  File.open(jamfile, 'w') { |f| f.write(original_jamfile_contents) }
end

# Add libgc dependency for 64 bit x86
if(`uname -m` =~ /x86_64/)
  additional_fpm_flags << ' -d "libgc-dev (>= 0)"'
end

# Build the last stage with the modified rpath jamfile
system("make 3-stage-bootstrap") || exit(1)

# Install into staging area
system("make install DESTDIR=#{STAGING_DIR}") || exit(1)

# Strip all the libraries and binaries
needs_stripping = Dir["#{STAGING_DIR}/usr/lib/opendylan/lib/*.so"] + Dir["#{STAGING_DIR}/usr/lib/opendylan/bin/*"]
needs_stripping.each do |f|
  unless(system("strip \"#{f}\""))
    puts "Failed to strip #{f}, quiting."
    exit(1)
  end
end

srcdir = Dir.pwd
# Create symlinks in /usr/bin for everything in /usr/lib/opendylan/bin
Dir.chdir "#{STAGING_DIR}/usr/bin"
Dir["../lib/opendylan/bin/*"].each { |f| FileUtils.ln_s(f, File.basename(f)) }
Dir.chdir srcdir

VERSION=`./Bootstrap.3/bin/dylan-compiler -shortversion`.chomp

# Generate the actual deb package
FPM_CMD=<<EOF
fpm -s dir -t deb -n opendylan --deb-changelog packages/unix/debian/changelog -v #{VERSION} -C #{STAGING_DIR} -p opendylan-VERSION_ARCH.deb \
    -d "gcc (>= 0)" -d "libc6-dev (>= 0)" #{additional_fpm_flags} -m "Wim Vander Schelden <wim@fixnum.org>" \
    --license MIT --url "http://opendylan.org/" --vendor "Dylan Hackers" --description "A Dylan compiler
    Dylan is a multi-paradigm programming language. It is a
    object-oriented language and is well suited for a
    functional style of programming.
    Though Dylan is a dynamic language, it was carefully
    designed to allow compilation to efficient machine code." \
    usr/lib/opendylan usr/bin
EOF

puts FPM_CMD
system(FPM_CMD) || exit(1)

rm_rf STAGING_DIR

# Check the generated package for problems
#system("lintian *.deb")
