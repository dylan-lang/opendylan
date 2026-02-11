#!/bin/sh
set -e

# You may need to install the following packages for this script to work: autoconf
# automake cmake ninja-build libtool.  On at least one system (aarch64 Debian) libtinfo5
# was also needed.

# https://releases.llvm.org/
LLVM_RELEASE=21.1.8
LLVM_REL=$(echo $LLVM_RELEASE | sed s/-rc/rc/)

LLVM_CLANG=$(echo $LLVM_RELEASE | sed 's/\([0-9]*\).*/\1/')

# https://github.com/bdwgc/bdwgc
# Note: For 8.2.8 and earlier the URL was https://github.com/ivmai/bdwgc. There does
# appear to be a forwarding pointer but not all the earlier releases (ex: 8.2.8) are
# there.
BDWGC_RELEASE=8.2.10

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.
top_srcdir=$(cd $srcdir/../..; pwd)

(cd $top_srcdir; ./autogen.sh)

OPENDYLAN_RELEASE=$($top_srcdir/configure --version | sed -n 's/^Open Dylan configure //p')

MACHINE=$(uname -m)
SYSTEM=$(uname -s)

echo Building Open Dylan $OPENDYLAN_RELEASE for $MACHINE $SYSTEM
DISTDIR=$(pwd)/release/opendylan-${OPENDYLAN_RELEASE}
rm -rf ${DISTDIR}
mkdir -p ${DISTDIR}

MAKE=make
TAR=tar
LLVM_TARGETS=Native
NEED_LIBUNWIND=:
NEED_INSTALL_NAME=false
SYSROOT=
CFLAGS_ARCH=
USE_LLD="-fuse-ld=lld"
USE_LLD_OPTS="-DLLVM_ENABLE_LLD:BOOL=ON"
BUILD_SRC=false
case ${MACHINE}-${SYSTEM} in
    amd64-FreeBSD)
        BUILD_SRC=:
        MAKE=gmake
        TRIPLE=amd64-unknown-freebsd11
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
    i386-FreeBSD)
        BUILD_SRC=:
        MAKE=gmake
        TRIPLE=i386-unknown-freebsd11
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
    x86_64-Linux|i686-Linux)
        BUILD_SRC=:
        TRIPLE=x86_64-unknown-linux-gnu
        TAR="tar --wildcards"
        USE_LLD_OPTS=
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
    aarch64-Linux)
        BUILD_SRC=:
        TRIPLE=aarch64-linux-gnu
        TAR="tar --wildcards"
        USE_LLD_OPTS=
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
    x86_64-Darwin)
        TRIPLE=x86_64-apple-darwin
        NEED_LIBUNWIND=false
        NEED_INSTALL_NAME=:
        SYSROOT=" -isysroot $(xcrun --show-sdk-path)"
        USE_LLD=
        USE_LLD_OPTS=
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
    arm64-Darwin)
        TRIPLE=x86_64-apple-darwin
        LLVM_TARGETS="AArch64;ARM;X86"
        CFLAGS_ARCH="-arch x86_64"
        NEED_LIBUNWIND=false
        NEED_INSTALL_NAME=:
        SYSROOT=" -isysroot $(xcrun --show-sdk-path)"
        USE_LLD=
        USE_LLD_OPTS=
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
esac

LLVM_DIST=llvm-project-${LLVM_REL}
BDWGC_DIST=gc-${BDWGC_RELEASE}

if [ ! -d ${LLVM_DIST} ]; then
    echo Cloning llvm-project tag ${LLVM_REL}
    git clone --depth 1 --branch llvmorg-${LLVM_REL} https://github.com/llvm/llvm-project.git ${LLVM_DIST}
fi

if [ ! -d ${BDWGC_DIST} ]; then
    echo Cloning BDW GC tag ${BDWGC_RELEASE}
    git clone --depth 1 --branch v${BDWGC_RELEASE} https://github.com/bdwgc/bdwgc.git ${BDWGC_DIST}
fi

RT_OPTS=
RT_TARGETS=
if $NEED_LIBUNWIND; then
    RT_OPTS="-DLLVM_ENABLE_RUNTIMES:STRING=libunwind -DLIBUNWIND_INSTALL_HEADERS:BOOL=ON -DLIBUNWIND_INSTALL_INCLUDE_DIR=${DISTDIR}/include -DLIBUNWIND_INSTALL_LIBRARY_DIR=${DISTDIR}/lib"
    RT_TARGETS=install-runtimes
fi
(cd ${LLVM_DIST};
 cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=OFF \
       -DCMAKE_INSTALL_PREFIX=${DISTDIR}/llvm \
       -DLLVM_TARGETS_TO_BUILD:STRING="${LLVM_TARGETS}" \
       -DLLVM_ENABLE_PROJECTS="llvm;clang;lld" \
       ${RT_OPTS} ${USE_LLD_OPTS} \
       -DLLVM_DISABLE_ASSEMBLY_FILES=ON \
       -DLLVM_ENABLE_TERMINFO=OFF \
       -DLLVM_ENABLE_LIBEDIT=OFF \
       -DLLVM_ENABLE_LIBXML2=OFF \
       -DLLVM_ENABLE_ZLIB=ON \
       -DLLVM_ENABLE_ZSTD=OFF \
       -DLLVM_INSTALL_TOOLCHAIN_ONLY=ON \
       -DLLVM_BUILD_TOOLS=OFF \
       -DLLVM_BUILD_UTILS=OFF \
       -DLLVM_TOOL_LLVM_ISEL_FUZZER_BUILD=OFF \
       -DLLVM_TOOL_LLVM_OPT_FUZZER_BUILD=OFF \
       -DLLVM_TOOL_LLVM_ITANIUM_DEMANGLE_FUZZER_BUILD=OFF \
       -DLLVM_TOOL_LLVM_MICROSOFT_DEMANGLE_FUZZER_BUILD=OFF \
       -DLLVM_TOOL_LLVM_SPECIAL_CASE_LIST_FUZZER_BUILD=OFF \
       -DLLVM_TOOL_LLVM_YAML_NUMERIC_PARSER_FUZZER_BUILD=OFF \
       -DLLVM_ENABLE_PER_TARGET_RUNTIME_DIR:BOOL=OFF \
       -DLLVM_INCLUDE_TESTS=OFF \
       -DCLANG_ENABLE_LIBXML2:BOOL=OFF \
       -DCLANG_ENABLE_STATIC_ANALYZER=OFF \
       -DCLANG_INCLUDE_DOCS=OFF \
       -DCLANG_INCLUDE_TESTS=OFF \
       -DCLANG_TOOL_CLANG_CHECK_BUILD=OFF \
       -DCLANG_TOOL_CLANG_DIFF_BUILD=OFF \
       -DCLANG_TOOL_CLANG_EXTDEF_MAPPING_BUILD=OFF \
       -DCLANG_TOOL_CLANG_FORMAT_BUILD=OFF \
       -DCLANG_TOOL_CLANG_FORMAT_VS_BUILD=OFF \
       -DCLANG_TOOL_CLANG_FUZZER_BUILD=OFF \
       -DCLANG_TOOL_CLANG_IMPORT_TEST_BUILD=OFF \
       -DCLANG_TOOL_CLANG_OFFLOAD_BUNDLER_BUILD=OFF \
       -DCLANG_TOOL_CLANG_OFFLOAD_WRAPPER_BUILD=OFF \
       -DCLANG_TOOL_CLANG_REFACTOR_BUILD=OFF \
       -DCLANG_TOOL_CLANG_RENAME_BUILD=OFF \
       -DCLANG_TOOL_CLANG_SCAN_DEPS_BUILD=OFF \
       -DCLANG_TOOL_CLANG_SHLIB_BUILD=OFF \
       -DCLANG_TOOL_C_INDEX_TEST_BUILD=OFF \
       -DCLANG_TOOL_DIAGTOOL_BUILD=OFF \
       -DCLANG_TOOL_LIBCLANG_BUILD=OFF \
       -DCLANG_TOOL_SCAN_BUILD_BUILD=OFF \
       -DCLANG_TOOL_SCAN_VIEW_BUILD=OFF \
       -DLLVM_PARALLEL_COMPILE_JOBS=${DYLAN_JOBS} \
       llvm;
 ninja install-clang install-lld install-clang-resource-headers ${RT_TARGETS})
CC="${DISTDIR}/llvm/bin/clang${SYSROOT}"
CXX="${DISTDIR}/llvm/bin/clang++${SYSROOT}"

RTLIBS_INSTALL=

if $NEED_LIBUNWIND; then
    for i in ${DISTDIR}/lib/libunwind*; do
        RTLIBS_INSTALL="$RTLIBS_INSTALL $i"
    done
fi

echo Building BDWGC in ${BDWGC_DIST}
(cd ${BDWGC_DIST};
 ./autogen.sh;
 ./configure CC="$CC" CFLAGS="-O2 ${CFLAGS_ARCH}" -q --prefix=$DISTDIR \
             --disable-docs --disable-static \
             --enable-threads=posix \
             --enable-parallel-mark \
             --with-libatomic-ops=none;
 ${MAKE} all install >build.log)

for i in ${DISTDIR}/lib/libgc*; do
    RTLIBS_INSTALL="$RTLIBS_INSTALL $i"
    if $NEED_INSTALL_NAME; then
        install_name_tool -id @rpath/$(basename $i) $i || true
    fi
done

echo Building Open Dylan
$top_srcdir/configure CC="$CC" CXX="$CXX" \
                      CPPFLAGS="-I${DISTDIR}/include" \
                      CFLAGS="-O2 ${CFLAGS_ARCH}" \
                      LDFLAGS="-L${DISTDIR}/lib $USE_LLD" \
                      --build ${TRIPLE} \
                      --with-gc=${DISTDIR} \
                      --with-harp-collector=boehm \
                      "$@"

echo "RTLIBS_INSTALL += ${RTLIBS_INSTALL} ;" >>sources/jamfiles/config.jam

${MAKE} dist DYLAN_JOBS=${DYLAN_JOBS}
