#!/bin/sh
set -e

LLVM_RELEASE=10.0.0
LLVM_CLANG=$(echo $LLVM_RELEASE | sed 's/\([0-9]*\).*/\1/')

LIBUNWIND_RELEASE=1.4.0

BDWGC_RELEASE=8.0.4

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

# clang+llvm-10.0.0-aarch64-linux-gnu.tar.xz
# clang+llvm-10.0.0-aarch64-linux-gnu.tar.xz.sig
# clang+llvm-10.0.0-amd64-pc-solaris2.11.tar.xz
# clang+llvm-10.0.0-amd64-pc-solaris2.11.tar.xz.sig
# clang+llvm-10.0.0-amd64-unknown-freebsd11.tar.xz
# clang+llvm-10.0.0-amd64-unknown-freebsd11.tar.xz.sig
# clang+llvm-10.0.0-armv7a-linux-gnueabihf.tar.xz
# clang+llvm-10.0.0-armv7a-linux-gnueabihf.tar.xz.sig
# clang+llvm-10.0.0-i386-unknown-freebsd11.tar.xz
# clang+llvm-10.0.0-i386-unknown-freebsd11.tar.xz.sig
# clang+llvm-10.0.0-x86_64-apple-darwin.tar.xz
# clang+llvm-10.0.0-x86_64-apple-darwin.tar.xz.sig
# clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
# clang+llvm-10.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz.sig
# clang+llvm-10.0.0-x86_64-linux-sles11.3.tar.xz
# clang+llvm-10.0.0-x86_64-linux-sles11.3.tar.xz.sig

MAKE=make
TAR=tar
NEED_LIBUNWIND=:
LIBUNWIND_EXCEPTIONS=
BUILD_SRC=false
case ${MACHINE}-${SYSTEM} in
    amd64-FreeBSD)
        MAKE=gmake
        TRIPLE=amd64-unknown-freebsd11
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
    i386-FreeBSD)
        MAKE=gmake
        TRIPLE=i386-unknown-freebsd11
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
    x86_64-Linux|i686-Linux)
        BUILD_SRC=:
        TAR="tar --wildcards"
        LIBUNWIND_EXCEPTIONS='--enable-cxx-exceptions'
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
    aarch64-Linux)
        TRIPLE=aarch64-linux-gnu
        TAR="tar --wildcards"
        LIBUNWIND_EXCEPTIONS='--enable-cxx-exceptions'
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
    x86_64-Darwin)
        TRIPLE=x86_64-darwin-apple
        NEED_LIBUNWIND=false
        DYLAN_JOBS=$(getconf _NPROCESSORS_ONLN)
        ;;
esac

LIBUNWIND_DIST=libunwind-${LIBUNWIND_RELEASE}
LIBUNWIND_TAR=${LIBUNWIND_DIST}.tar.gz

BDWGC_DIST=gc-${BDWGC_RELEASE}
BDWGC_TAR=${BDWGC_DIST}.tar.gz

if $BUILD_SRC; then
    LLVM_DIST=llvm-project-llvmorg-${LLVM_RELEASE}
    LLVM_TAR=llvmorg-${LLVM_RELEASE}.tar.gz
    if [ ! -f "$LLVM_TAR" ]; then
        echo Error: LLVM source distribution file missing 1>&2
        echo Please download https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_RELEASE}/${LLVM_TAR} and place the file in the current directory 1>&2
        exit 1
    fi
else
    LLVM_DIST=clang+llvm-${LLVM_RELEASE}-${TRIPLE}
    LLVM_TAR=${LLVM_DIST}.tar.xz
    if [ ! -f "$LLVM_TAR" ]; then
        echo Error: LLVM binary distribution file missing 1>&2
        echo Please download https://github.com/llvm/llvm-project/releases/download/llvmorg-${LLVM_RELEASE}/${LLVM_TAR} and place the file in the current directory 1>&2
        exit 1
    fi
fi
if $NEED_LIBUNWIND && [ ! -f "$LIBUNWIND_TAR" ]; then
    echo Error: libunwind source distribution file missing 1>&2
    echo Please download https://github.com/libunwind/libunwind/releases/download/v${LIBUNWIND_RELEASE}/${LIBUNWIND_TAR} and place the file in the current directory 1>&2
    exit 1
fi
if [ ! -f "$BDWGC_TAR" ]; then
    echo Error: BDW GC distribution file missing 1>&2
    echo Please download https://github.com/ivmai/bdwgc/releases/download/v${BDWGC_RELEASE}/${BDWGC_TAR} and place the file in the current directory 1>&2
    exit 1
fi

if $BUILD_SRC; then
    echo Unpacking LLVM project sources into ${LLVM_DIST}
    rm -rf "${LLVM_DIST}"
    ${TAR} -xzf ${LLVM_TAR} \
           ${LLVM_DIST}/llvm \
           ${LLVM_DIST}/clang \
           ${LLVM_DIST}/lld
    (cd ${LLVM_DIST};
     cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_ASSERTIONS=OFF \
           -DLLVM_ENABLE_PROJECTS="llvm;clang;lld" \
           -DLLVM_ENABLE_TERMINFO=OFF \
           -DLLVM_INSTALL_TOOLCHAIN_ONLY=ON \
           -DLLVM_BUILD_TOOLS=OFF \
           -DLLVM_BUILD_UTILS=OFF \
           -DLLVM_TOOL_LLVM_ISEL_FUZZER_BUILD=OFF \
           -DLLVM_TOOL_LLVM_OPT_FUZZER_BUILD=OFF \
           -DLLVM_TOOL_LLVM_ITANIUM_DEMANGLE_FUZZER_BUILD=OFF \
           -DLLVM_TOOL_LLVM_MICROSOFT_DEMANGLE_FUZZER_BUILD=OFF \
           -DLLVM_TOOL_LLVM_SPECIAL_CASE_LIST_FUZZER_BUILD=OFF \
           -DLLVM_TOOL_LLVM_YAML_NUMERIC_PARSER_FUZZER_BUILD=OFF \
           -DLLVM_INCLUDE_TESTS=OFF \
           -DCLANG_ENABLE_STATIC_ANALYZER=OFF \
           -DCLANG_ENABLE_ARCMT=OFF \
           -DCLANG_INCLUDE_DOCS=OFF \
           -DCLANG_INCLUDE_TESTS=OFF \
           -DCLANG_TOOL_ARCMT_TEST_BUILD=OFF \
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
           -DCLANG_TOOL_C_ARCMT_TEST_BUILD=OFF \
           -DCLANG_TOOL_C_INDEX_TEST_BUILD=OFF \
           -DCLANG_TOOL_DIAGTOOL_BUILD=OFF \
           -DCLANG_TOOL_LIBCLANG_BUILD=OFF \
           -DCLANG_TOOL_SCAN_BUILD_BUILD=OFF \
           -DCLANG_TOOL_SCAN_VIEW_BUILD=OFF \
           -DLLVM_PARALLEL_COMPILE_JOBS=${DYLAN_JOBS} \
           llvm;
     ninja)
    mkdir -p ${DISTDIR}/bin ${DISTDIR}/lib
    cp -RP \
           ${LLVM_DIST}/bin/clang-[1-9]* \
           ${LLVM_DIST}/bin/clang \
           ${LLVM_DIST}/bin/clang++ \
           ${LLVM_DIST}/bin/lld \
           ${LLVM_DIST}/bin/ld.lld \
           ${DISTDIR}/bin/
    cp -RP ${LLVM_DIST}/lib/clang ${DISTDIR}/lib/
else
    echo Unpacking LLVM into ${LLVM_DIST}
    rm -rf "${LLVM_DIST}"
    ${TAR} -xJf ${LLVM_TAR} \
           "${LLVM_DIST}/bin/clang-[1-9]*" \
           ${LLVM_DIST}/bin/clang \
           ${LLVM_DIST}/bin/clang++ \
           ${LLVM_DIST}/bin/lld \
           ${LLVM_DIST}/bin/ld.lld \
           ${LLVM_DIST}/lib/clang
    cp -RP ${LLVM_DIST}/* ${DISTDIR}/
fi
CC=${DISTDIR}/bin/clang
CXX=${DISTDIR}/bin/clang++

RTLIBS_INSTALL=

if $NEED_LIBUNWIND; then
    echo Unpacking libunwind into ${LIBUNWIND_DIST}
    rm -rf ${LIBUNWIND_DIST}
    ${TAR} -xzf ${LIBUNWIND_TAR}

    (cd ${LIBUNWIND_DIST};
     ./configure CC=$CC -q --prefix=$DISTDIR \
                 --with-pic $LIBUNWIND_EXCEPTIONS \
                 --disable-static --disable-documentation --disable-tests \
                 --disable-coredump --disable-ptrace --disable-setjmp; \
     ${MAKE} all install >build.log)

    for i in ${DISTDIR}/lib/libunwind*; do
        RTLIBS_INSTALL="$RTLIBS_INSTALL $i"
    done
fi

echo Unpacking BDWGC into ${BDWGC_DIST}
rm -rf "${BDWGC_DIST}"
${TAR} -xzf ${BDWGC_TAR}

echo Building BDWGC in ${BDWGC_DIST}
(cd ${BDWGC_DIST};
 ./configure CC=$CC -q --prefix=$DISTDIR \
             --disable-docs --disable-static \
             --enable-threads=posix \
             --enable-parallel-mark \
             --with-libatomic-ops=none;
 ${MAKE} all install >build.log)

for i in ${DISTDIR}/lib/libgc*; do
    RTLIBS_INSTALL="$RTLIBS_INSTALL $i"
done

echo Building Open Dylan
$top_srcdir/configure CC="$CC" CXX="$CXX" \
                      CPPFLAGS="-I${DISTDIR}/include" \
                      LDFLAGS="-L${DISTDIR}/lib -fuse-ld=lld" \
                      --with-gc=${DISTDIR} \
                      --with-harp-collector=boehm \
                      "$@"

echo "RTLIBS_INSTALL += ${RTLIBS_INSTALL} ;" >>sources/jamfiles/config.jam

${MAKE} 3-stage-bootstrap DYLAN_JOBS=${DYLAN_JOBS}

sed -i~ "s;${DISTDIR};\$(SYSTEM_ROOT);g" sources/jamfiles/config.jam
rm sources/jamfiles/config.jam~

${MAKE} dist
