#
# Script: Dylan test suites makefile
# Author: Andy Armstrong
#
# Use this makefile to link any of the PC test suites or
# examples. First you need to have set up your environment variables
# by running the script kan-env which lives in
# dylan\admin\scripts\kan-env.bat. You need to pass the following
# arguments:
#
#   kan-env -p <build-area> -s <system-area> -d <release> -b <branch>
#
# which have the following meanings to this makefile:
#
#   <test-area>		- the directory for building tests/examples
#   <release-area>	- the release to be testing
#   <compiler-area>	- area to run pentium-dw/build from
#   <branch>		- the Hope branch
#
# The compiler-area defaults to the release-area, and the branch
# defaults to the trunk. This is almost certainly what you want unless
# you have yet to build a compiler in the area you are testing (you're
# only building and testing DUIM, say). It is probably the best idea
# not to use the release area as the test area because that will
# corrupt the release, but it is perfectly legal if that is what is
# wanted. You can use the argument '-sp' to specify the same area for
# system and personal.
#
# Note that the Hope branch is only used if there are sources missing
# from your build area. It is a bug if Hope is used at any other time.
#
# Here are some of the more useful targets:
#
#    nmake dylan-test-suite-app     - build the dylan test app
#    nmake duim-gui-test-suite      - build the DUIM gui test suite
#    nmake ole-examples             - build the OLE examples
#
#    nmake personal-test-suites     - build the Personal tests
#    nmake professional-test-suites - build the Professional tests
#    nmake enterprise-test-suites   - build the Enterprise tests
#
#    nmake personal-examples        - build the Personal examples
#    nmake professional-examples    - build the Professional examples
#    nmake enterprise-examples      - build the Enterprise examples
#
# Mail any problems you have to dylan-admin.
#

#
# Application setup.
#
CHECKOUT = hope checkout -stale-unit-files delete -missing-dir force -writable-files warn -modified-files warn -branch $(DYLAN_HOPE_BRANCH)
CHECKOUT_HIERARCHY = $(CHECKOUT) -rec -compound
COMPILER = $(DYLAN_RELEASE_ROOT)\install\x86-win32\bin\pentium-dw
COMPILE = $(COMPILER) update-libraries
COMPILE_AND_SAVE = $(COMPILE) -save
ENSURE_DIRECTORY = mkdir
UNINSTALL = rm -f
CLEAN =
ADMIN_SOURCE = $(WEBSTER_PERSONAL_ROOT)\qa\admin
LOGS = $(ADMIN_SOURCE)\logs
ADMIN_BUILDS_SOURCE = $(ADMIN_SOURCE)\builds
BUILD_APP = $(ADMIN_BUILDS_SOURCE)\build-application
BUILD_DLL = $(ADMIN_BUILDS_SOURCE)\build-library

#
# Source targets
#

REGISTRY_SOURCE = $(WEBSTER_PERSONAL_ROOT)\registry
GENERIC_REGISTRY = $(REGISTRY_SOURCE)\generic
X86_WIN32_REGISTRY = $(REGISTRY_SOURCE)\x86-win32
APP_SOURCE = $(WEBSTER_PERSONAL_ROOT)\app
LIB_SOURCE = $(WEBSTER_PERSONAL_ROOT)\lib

C_FFI_SOURCE = $(LIB_SOURCE)\c-ffi
C_FFI_TESTS_SOURCE = $(C_FFI_SOURCE)\test
CMU_SOURCE = $(WEBSTER_PERSONAL_ROOT)\partners\cmu
CMU_TEST_SUITE_SOURCE = $(CMU_SOURCE)\cmu-test-suite
DEUCE_SOURCE = $(WEBSTER_PERSONAL_ROOT)\deuce
DEUCE_TESTS_SOURCE = $(DEUCE_SOURCE)\tests
STANDALONE_DEUCE_SOURCE = $(DEUCE_SOURCE)\standalone
DUIM_SOURCE = $(WEBSTER_PERSONAL_ROOT)\duim
DUIM_TESTS_SOURCE = $(DUIM_SOURCE)\tests
DUIM_EXAMPLES_SOURCE = $(DUIM_SOURCE)\examples
FORMAT_SOURCE = $(LIB_SOURCE)\format
FORMAT_TESTS_SOURCE = $(FORMAT_SOURCE)\tests
HARLEQUIN_EXTENSIONS_SOURCE = $(LIB_SOURCE)\functional-extensions
FUNCTIONAL_EXTENSIONS_TESTS_SOURCE = $(FUNCTIONAL_EXTENSIONS_SOURCE)\tests
LOCATORS_SOURCE = $(LIB_SOURCE)\locators
LOCATORS_TESTS_SOURCE = $(LOCATORS_SOURCE)\tests
PRINT_SOURCE = $(LIB_SOURCE)\print
PRINT_TESTS_SOURCE = $(PRINT_SOURCE)\tests
QA_SOURCE = $(WEBSTER_PERSONAL_ROOT)\qa
STREAMS_SOURCE = $(LIB_SOURCE)\streams
STREAMS_TESTS_SOURCE = $(STREAMS_SOURCE)\tests
THREADS_SOURCE = $(LIB_SOURCE)\threads
THREADS_TESTS_SOURCE = $(THREADS_SOURCE)\tests
OLE_SOURCE = $(WEBSTER_PERSONAL_ROOT)\ole
OLE_AUTOMATION_SOURCE = $(OLE_SOURCE)\ole-automation
OLE_AUTOMATION_TESTS_SOURCE = $(OLE_AUTOMATION_SOURCE)\tests
DATABASES_SOURCE = $(WEBSTER_PERSONAL_ROOT)\databases
SQL_ODBC_TEST_SOURCE = $(DATABASES_SOURCE)\sql-odbc-test

app-source: $(APP_SOURCE)
cmu-source: $(CMU_SOURCE)
cmu-test-suite-source: $(CMU_TEST_SUITE_SOURCE)
c-ffi-tests-source: $(C_FFI_TESTS_SOURCE)
deuce-tests-source: $(DEUCE_TESTS_SOURCE)
standalone-deuce-source: $(STANDALONE_DEUCE_SOURCE)
duim-tests-source: $(DUIM_TESTS_SOURCE)
duim-examples-source: $(DUIM_EXAMPLES_SOURCE)
format-tests-source: $(FORMAT_TESTS_SOURCE)
functional-extensions-tests-source: $(FUNCTIONAL_EXTENSIONS_TESTS_SOURCE)
locators-tests-source: $(LOCATORS_TESTS_SOURCE)
print-tests-source: $(PRINT_TESTS_SOURCE)
qa-source: $(QA_SOURCE)
streams-tests-source: $(STREAMS_TESTS_SOURCE)
threads-tests-source: $(THREADS_TESTS_SOURCE)
ole-automation-tests-source: $(OLE_AUTOMATION_TESTS_SOURCE)
databases-source: $(DATABASES_SOURCE)
sql-odbc-test-source: $(SQL_ODBC_TEST_SOURCE)

#
# Products
#
TESTWORKS = $(WEBSTER_PERSONAL_BIN)\testworks.dll
TESTWORKS_PLUS = $(WEBSTER_PERSONAL_BIN)\testworks-plus.dll
TESTWORKS_SPECS = $(WEBSTER_PERSONAL_BIN)\testworks-specs.dll
GUI_TESTWORKS = $(WEBSTER_PERSONAL_BIN)\gui-testworks.dll

DYLAN_TEST_SUITE = $(WEBSTER_PERSONAL_LIB)\dylan-test-suite.lib
DEUCE_TEST_SUITE = $(WEBSTER_PERSONAL_LIB)\deuce-test-suite.lib
DUIM_TEST_SUITE = $(WEBSTER_PERSONAL_LIB)\duim-test-suite.lib

CMU_TEST_SUITE = $(WEBSTER_PERSONAL_BIN)\cmu-test-suite.exe
DYLAN_TEST_SUITE_APP = $(WEBSTER_PERSONAL_BIN)\dylan-test-suite-app.exe
THREADS_TEST_SUITE_APP = $(WEBSTER_PERSONAL_BIN)\threads-test-suite-app.exe
C_FFI_TEST_SUITE_APP = $(WEBSTER_PERSONAL_BIN)\c-ffi-test-suite-app.exe
LIBRARIES_TEST_SUITE_APP = $(WEBSTER_PERSONAL_BIN)\libraries-test-suite-app.exe
PENTIUM_TEST_SUITE_APP = $(WEBSTER_PERSONAL_BIN)\pentium-test-suite-app.exe
DUIM_TEST_SUITE_APP = $(WEBSTER_PERSONAL_BIN)\duim-test-suite-app.exe
DUIM_GUI_TEST_SUITE = $(WEBSTER_PERSONAL_BIN)\duim-gui-test-suite.exe
WIN32_DUIM_GUI_TEST_SUITE = $(WEBSTER_PERSONAL_BIN)\win32-duim-gui-test-suite.exe
STANDALONE_DEUCE = $(WEBSTER_PERSONAL_BIN)\standalone-deuce.exe
OLE_TEST = $(WEBSTER_PERSONAL_BIN)\ole-test.exe
OLE_COCLASS_TEST = $(WEBSTER_PERSONAL_BIN)\ole-coclass-test.exe
SQL_ODBC_TEST = $(WEBSTER_PERSONAL_BIN)\sql-odbc-test.exe

FACTORIAL_BIG = $(WEBSTER_PERSONAL_BIN)\factorial-big.exe
FACTORIAL_SMALL = $(WEBSTER_PERSONAL_BIN)\factorial-small.exe
HELLO_WORLD = $(WEBSTER_PERSONAL_BIN)\hello-world.exe

DUIM_RESOURCE_EXAMPLE = $(WEBSTER_PERSONAL_BIN)\duim-resource-example.exe
LIFE = $(WEBSTER_PERSONAL_BIN)\life.exe
OTHELLO = $(WEBSTER_PERSONAL_BIN)\othello.exe
PENTE = $(WEBSTER_PERSONAL_BIN)\pente.exe
SCRIBBLE = $(WEBSTER_PERSONAL_BIN)\scribble.exe
TETRIS = $(WEBSTER_PERSONAL_BIN)\tetris.exe
TIC_TAC_TOE = $(WEBSTER_PERSONAL_BIN)\tic-tac-toe.exe

OLE_SCRIBBLE = $(WEBSTER_PERSONAL_BIN)\ole-scribble.exe
OCX_SCRIBBLE = $(WEBSTER_PERSONAL_BIN)\ocx-scribble.exe
SAMPLE_AUTOMATION_CONTROLLER = $(WEBSTER_PERSONAL_BIN)\sample-automation-controller.exe
SAMPLE_AUTOMATION_SERVER = $(WEBSTER_PERSONAL_BIN)\sample-automation-server.exe
SAMPLE_OLE_CONTAINER = $(WEBSTER_PERSONAL_BIN)\sample-ole-container.exe
SAMPLE_OLE_SERVER = $(WEBSTER_PERSONAL_BIN)\sample-ole-server.exe
WINDOWS_FFI_EXAMPLE = $(WEBSTER_PERSONAL_BIN)\windows-ffi-example.exe
WINDOWS_GADGETS_EXAMPLE = $(WEBSTER_PERSONAL_BIN)\windows-gadgets-example.exe
WINDOWS_RESOURCE_EXAMPLE = $(WEBSTER_PERSONAL_BIN)\windows-resource-example.exe
WIN32_OLE_SERVER = $(WEBSTER_PERSONAL_BIN)\win32-ole-server.exe
SQL_EXAMPLE = $(WEBSTER_PERSONAL_BIN)\sql-example.exe

#
# Targets
#

dylan-test-suite: $(DYLAN_TEST_SUITE)
deuce-test-suite: $(DEUCE_TEST_SUITE)
duim-test-suite: $(DUIM_TEST_SUITE)
testworks: $(TESTWORKS)
testworks-plus: $(TESTWORKS_PLUS)
testworks-specs: $(TESTWORKS_SPECS)
gui-testworks: $(GUI_TESTWORKS)

cmu-test-suite: $(CMU_TEST_SUITE)
dylan-test-suite-app: $(DYLAN_TEST_SUITE_APP)
threads-test-suite-app: $(THREADS_TEST_SUITE_APP)
c-ffi-test-suite-app: $(C_FFI_TEST_SUITE_APP)
libraries-test-suite-app: $(LIBRARIES_TEST_SUITE_APP)
pentium-test-suite-app: $(PENTIUM_TEST_SUITE_APP)
duim-test-suite-app: $(DUIM_TEST_SUITE_APP)
duim-gui-test-suite: $(DUIM_GUI_TEST_SUITE)
win32-duim-gui-test-suite: $(WIN32_DUIM_GUI_TEST_SUITE)
standalone-deuce: $(STANDALONE_DEUCE)
ole-test: $(OLE_TEST)
ole-coclass-test: $(OLE_COCLASS_TEST)
sql-odbc-test: $(SQL_ODBC_TEST)

factorial-big: $(FACTORIAL_BIG)
factorial-small: $(FACTORIAL_SMALL)
hello-world: $(HELLO_WORLD)

duim-resource-example: $(WINDOWS_RESOURCE_EXAMPLE)
life: $(LIFE)
othello: $(OTHELLO)
pente: $(PENTE)
scribble: $(SCRIBBLE)
tetris: $(TETRIS)
tic-tac-toe: $(TIC_TAC_TOE)

ole-scribble: $(OLE_SCRIBBLE)
ocx-scribble: $(OCX_SCRIBBLE)
sample-automation-controller: $(SAMPLE_AUTOMATION_CONTROLLER)
sample-automation-server: $(SAMPLE_AUTOMATION_SERVER)
sample-ole-container: $(SAMPLE_OLE_CONTAINER)
sample-ole-server: $(SAMPLE_OLE_SERVER)
win32-ole-server: $(WIN32_OLE_SERVER)
windows-ffi-example: $(WINDOWS_FFI_EXAMPLE)
windows-gadgets-example: $(WINDOWS_GADGETS_EXAMPLE)
windows-resource-example: $(WINDOWS_RESOURCE_EXAMPLE)
sql-example: $(SQL_EXAMPLE)

testworks-libraries: testworks testworks-plus testworks-specs gui-testworks
duim-test-suites: duim-test-suite-app duim-gui-test-suite win32-duim-gui-test-suite
test-suites: libraries-test-suite-app duim-test-suites
pentium-test-suites: pentium-test-suite-app
ole-test-suites: ole-test ole-coclass-test
database-test-suites: sql-odbc-test

console-examples: hello-world factorial-small factorial-big
win32-examples: windows-ffi-example windows-gadgets-example windows-resource-example
ole-examples: win32-ole-server sample-automation-controller sample-automation-server sample-ole-container
duim-examples: life othello pente scribble tetris tic-tac-toe duim-resource-example
duim-ole-examples: ole-scrible ocx-scribble

personal-test-suites: test-suites
professional-test-suites: personal-test-suites ole-test-suites database-test-suites
enterprise-test-suites: professional-test-suites

personal-examples: console-examples duim-examples win32-examples
professional-examples: personal-examples ole-examples duim-ole-examples
enterprise-examples: professional-examples

personal: personal-test-suites personal-examples
professional: professional-test-suites professional-examples
enterprise: enterprise-test-suites enterprise-examples

#
# Clean
#
# Is there a good way to clean out just a piece of the system?
#

clean-all:
  rmdir /S /Q $(WEBSTER_PERSONAL_BUILD)
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_BUILD)
  rmdir /S /Q $(WEBSTER_PERSONAL_BIN)
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_BIN)
  rmdir /S /Q $(WEBSTER_PERSONAL_LIB)
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_LIB)
  rmdir /S /Q $(WEBSTER_PERSONAL_ROOT)\databases\x86-win32
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_ROOT)\databases\x86-win32
  rmdir /S /Q $(LOGS)
  $(ENSURE_DIRECTORY) $(LOGS)

#
# Make source hierarchy
#

$(WEBSTER_PERSONAL_ROOT):
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_ROOT)

$(LIB_SOURCE):
  $(ENSURE_DIRECTORY) $(LIB_SOURCE)

$(WEBSTER_PERSONAL_BUILD): $(WEBSTER_PERSONAL_ROOT)
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_BUILD)

$(WEBSTER_PERSONAL_BIN): $(WEBSTER_PERSONAL_ROOT)
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_BIN)

$(WEBSTER_PERSONAL_LIB): $(WEBSTER_PERSONAL_ROOT)
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_LIB)

#
# Check out sources
#

$(ADMIN_SOURCE):
  $(ENSURE_DIRECTORY) $(ADMIN_SOURCE)
  $(ENSURE_DIRECTORY) $(LOGS)
  $(ENSURE_DIRECTORY) $(LIB_SOURCE)
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_BUILD)
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_BIN)
  $(ENSURE_DIRECTORY) $(WEBSTER_PERSONAL_LIB)
  $(CHECKOUT_HIERARCHY) D-qa -directory $(QA_SOURCE) > $(LOGS)\checkout-qa.log

registries: $(GENERIC_REGISTRY) $(X86_WIN32_REGISTRY)
source-skeleton: registries

$(REGISTRY_SOURCE): $(ADMIN_SOURCE)
  $(ENSURE_DIRECTORY) $(REGISTRY_SOURCE)

$(GENERIC_REGISTRY): $(REGISTRY_SOURCE)
  $(ENSURE_DIRECTORY) $(GENERIC_REGISTRY)
  $(CHECKOUT) -directory $(GENERIC_REGISTRY) \
    -compound D-registry-generic \
    -unit testworks \
    -and -unit testworks-specs \
    -and -unit testworks-plus \
    -and -unit gui-testworks \
    -and -unit testworks-test-suite \
    -and -unit dylan-test-suite \
    -and -unit functional-extensions-test-suite \
    -and -unit streams-test-suite \
    -and -unit print-test-suite \
    > $(LOGS)\checkout-generic-registry-1.log
  $(CHECKOUT) -directory $(GENERIC_REGISTRY) \
    -compound D-registry-generic \
    -unit format-test-suite \
    -and -unit locators-test-suite \
    -and -unit deuce-test-suite \
    -and -unit duim-test-suite \
    -and -unit threads-test-suite \
    -and -unit c-ffi-test \
    -and -unit libraries-test-suite \
    -and -unit generic-test-suite \
    > $(LOGS)\checkout-generic-registry-2.log
  $(CHECKOUT) -directory $(GENERIC_REGISTRY) \
    -compound D-registry-generic \
    -unit dylan-test-suite-app \
    -and -unit cmu-test-suite \
    -and -unit threads-test-suite-app \
    -and -unit c-ffi-test-suite-app \
    -and -unit functional-extensions-test-suite-app \
    -and -unit libraries-test-suite-app \
    -and -unit generic-test-suite-app \
    -and -unit duim-test-suite-app \
    > $(LOGS)\checkout-generic-registry-apps.log
  $(CHECKOUT) -directory $(GENERIC_REGISTRY) \
    -compound D-registry-generic \
    -unit hello-world \
    -and -unit factorial-big \
    -and -unit factorial-small \
    > $(LOGS)\checkout-generic-registry-examples.log

$(X86_WIN32_REGISTRY): $(REGISTRY_SOURCE)
  $(ENSURE_DIRECTORY) $(X86_WIN32_REGISTRY)
  $(CHECKOUT) -directory $(X86_WIN32_REGISTRY) \
    -compound D-registry-x86-win32 \
    -unit pentium-test-suite \
    -and -unit pentium-test-suite-app \
    -and -unit duim-gui-test-suite \
    -and -unit win32-duim-gui-test-suite \
    -and -unit ole-test \
    -and -unit ole-coclass-test \
    -and -unit sql-odbc-test \
    > $(LOGS)\checkout-x86-win32-registry-tests.log
  $(CHECKOUT) -directory $(X86_WIN32_REGISTRY) \
    -compound D-registry-x86-win32 \
    -unit windows-ffi-example \
    -and -unit windows-gadgets-example \
    -and -unit windows-resource-example \
    > $(LOGS)\checkout-x86-win32-registry-win32-examples.log
  $(CHECKOUT) -directory $(X86_WIN32_REGISTRY) \
    -compound D-registry-x86-win32 \
    -unit duim-examples \
    -and -unit duim-resource-example \
    -and -unit life \
    -and -unit othello \
    -and -unit pente \
    -and -unit scribble \
    -and -unit tetris \
    -and -unit tic-tac-toe \
    -and -unit standalone-deuce \
    > $(LOGS)\checkout-x86-win32-registry-duim-examples.log
  $(CHECKOUT) -directory $(X86_WIN32_REGISTRY) \
    -compound D-registry-x86-win32 \
    -unit ole-scribble \
    -and -unit ocx-scribble \
    -and -unit sample-automation-controller \
    -and -unit sample-automation-server \
    -and -unit sample-ole-container \
    -and -unit sample-ole-server \
    -and -unit win32-ole-server \
    > $(LOGS)\checkout-x86-win32-registry-ole-examples.log
  $(CHECKOUT) -directory $(X86_WIN32_REGISTRY) \
    -compound D-registry-x86-win32 \
    -unit sql-example \
    > $(LOGS)\checkout-x86-win32-registry-database-examples.log

registry-source: $(GENERIC_REGISTRY) $(X86_WIN32_REGISTRY)

$(APP_SOURCE): source-skeleton
  $(CHECKOUT_HIERARCHY) D-app -directory $(APP_SOURCE) > $(LOGS)\checkout-app.log

$(C_FFI_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(C_FFI_SOURCE)

$(C_FFI_TESTS_SOURCE): $(C_FFI_SOURCE)
  $(CHECKOUT_HIERARCHY) D-lib-c-ffi-test -directory $(C_FFI_TESTS_SOURCE) > $(LOGS)\checkout-c-ffi-test.log

$(CMU_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(CMU_SOURCE)

$(CMU_TEST_SUITE_SOURCE): $(CMU_SOURCE)
  $(CHECKOUT_HIERARCHY) D-partners-cmu-cmu-test-suite -directory $(CMU_TEST_SUITE_SOURCE) > $(LOGS)\checkout-cmu-test-suite.log

$(DEUCE_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(DEUCE_SOURCE)

$(DEUCE_TESTS_SOURCE): $(DEUCE_SOURCE)
  $(CHECKOUT_HIERARCHY) D-deuce-tests -directory $(DEUCE_TESTS_SOURCE) > $(LOGS)\checkout-deuce-tests.log

$(STANDALONE_DEUCE_SOURCE): $(DEUCE_SOURCE)
  $(CHECKOUT_HIERARCHY) D-deuce-standalone -directory $(STANDALONE_DEUCE_SOURCE) > $(LOGS)\checkout-standalone-deuce.log

$(DUIM_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(DUIM_SOURCE)

$(DUIM_TESTS_SOURCE): $(DUIM_SOURCE)
  $(CHECKOUT_HIERARCHY) D-duim-tests -directory $(DUIM_TESTS_SOURCE) > $(LOGS)\checkout-duim-tests.log

$(DUIM_EXAMPLES_SOURCE): $(DUIM_SOURCE)
  $(CHECKOUT_HIERARCHY) D-duim-examples -directory $(DUIM_EXAMPLES_SOURCE) > $(LOGS)\checkout-duim-examples.log

$(FORMAT_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(FORMAT_SOURCE)

$(FORMAT_TESTS_SOURCE): $(FORMAT_SOURCE)
  $(CHECKOUT_HIERARCHY) D-lib-format-tests -directory $(FORMAT_TESTS_SOURCE) > $(LOGS)\checkout-format-tests.log

$(FUNCTIONAL_EXTENSIONS_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(FUNCTIONAL_EXTENSIONS_SOURCE)

$(FUNCTIONAL_EXTENSIONS_TESTS_SOURCE): $(FUNCTIONAL_EXTENSIONS_SOURCE)
  $(CHECKOUT_HIERARCHY) D-lib-functional-extensions-tests -directory $(HARLEQUIN_EXTENSIONS_TESTS_SOURCE) > $(LOGS)\checkout-functional-extensions-tests.log

$(LOCATORS_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(LOCATORS_SOURCE)

$(LOCATORS_TESTS_SOURCE): $(LOCATORS_SOURCE)
  $(CHECKOUT_HIERARCHY) D-lib-locators-tests -directory $(LOCATORS_TESTS_SOURCE) > $(LOGS)\checkout-locators-tests.log

$(PRINT_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(PRINT_SOURCE)

$(PRINT_TESTS_SOURCE): $(PRINT_SOURCE)
  $(CHECKOUT_HIERARCHY) D-lib-print-tests -directory $(PRINT_TESTS_SOURCE) > $(LOGS)\checkout-print-tests.log

$(STREAMS_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(STREAMS_SOURCE)

$(STREAMS_TESTS_SOURCE): $(STREAMS_SOURCE)
  $(CHECKOUT_HIERARCHY) D-lib-streams-tests -directory $(STREAMS_TESTS_SOURCE) > $(LOGS)\checkout-streams-tests.log

$(THREADS_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(THREADS_SOURCE)

$(THREADS_TESTS_SOURCE): $(THREADS_SOURCE)
  $(CHECKOUT_HIERARCHY) D-lib-threads-tests -directory $(THREADS_TESTS_SOURCE) > $(LOGS)\checkout-threads-tests.log

$(OLE_AUTOMATION_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(OLE_AUTOMATION_SOURCE)

$(OLE_AUTOMATION_TESTS_SOURCE): $(OLE_AUTOMATION_SOURCE)
  $(CHECKOUT_HIERARCHY) D-lib-ole-automation-tests -directory $(OLE_AUTOMATION_TESTS_SOURCE) > $(LOGS)\checkout-ole-automation-tests.log

$(DATABASES_SOURCE): source-skeleton
  $(ENSURE_DIRECTORY) $(DATABASES_SOURCE)

$(SQL_ODBC_TEST_SOURCE): $(DATABASES_SOURCE)
  $(CHECKOUT_HIERARCHY) D-databases-sql-odbc-test -directory $(SQL_ODBC_TEST_SOURCE) > $(LOGS)\checkout-sql-odbc-test.log

#
# TestWorks
#

$(TESTWORKS): source-skeleton
  $(COMPILE_AND_SAVE) testworks > $(LOGS)\compile-testworks.log
  $(BUILD_DLL) testworks

$(TESTWORKS_PLUS): testworks
  $(COMPILE_AND_SAVE) testworks-plus > $(LOGS)\compile-testworks-plus.log
  $(BUILD_DLL) testworks-plus

$(TESTWORKS_SPECS): testworks
  $(COMPILE_AND_SAVE) testworks-specs > $(LOGS)\compile-testworks-specs.log
  $(BUILD_DLL) testworks-specs

$(GUI_TESTWORKS): testworks
  $(COMPILE) gui-testworks > $(LOGS)\compile-gui-testworks.log
  $(BUILD_DLL) gui-testworks

#
# Libraries
#

$(DYLAN_TEST_SUITE): testworks-specs
  $(COMPILE) dylan-test-suite > $(LOGS)\compile-dylan-test-suite.log
  $(BUILD_DLL) dylan-test-suite

$(DEUCE_TEST_SUITE): testworks-specs deuce-tests-source
  $(COMPILE) deuce-test-suite > $(LOGS)\compile-deuce-test-suite.log
  $(BUILD_DLL) deuce-test-suite

$(DUIM_TEST_SUITE): testworks-specs duim-test-source
  $(COMPILE) duim-test-suite > $(LOGS)\compile-duim-test-suite.log
  $(BUILD_DLL) duim-test-suite

#
# Test Applications
#

$(CMU_TEST_SUITE): cmu-test-suite-source
  $(COMPILE) cmu-test-suite > $(LOGS)\compile-cmu-test-suite.log
  $(BUILD_APP) cmu-test-suite
  
$(DYLAN_TEST_SUITE_APP): testworks-specs testworks-plus
  $(COMPILE) dylan-test-suite-app > $(LOGS)\compile-dylan-test-suite-app.log
  $(BUILD_APP) dylan-test-suite-app

$(THREADS_TEST_SUITE_APP): testworks-specs testworks-plus threads-tests-source
  $(COMPILE) threads-test-suite-app > $(LOGS)\compile-threads-test-suite-app.log
  $(BUILD_APP) threads-test-suite-app

$(C_FFI_TEST_SUITE_APP): testworks-specs testworks-plus c-ffi-tests-source
  $(COMPILE) c-ffi-test-suite-app > $(LOGS)\compile-c-ffi-test-suite-app.log
  $(BUILD_APP) c-ffi-test-suite-app

$(LIBRARIES_TEST_SUITE_APP): dylan-test-suite-app testworks-specs testworks-plus format-tests-source functional-extensions-tests-source locators-tests-source print-tests-source streams-tests-source
  $(COMPILE) libraries-test-suite-app > $(LOGS)\compile-libraries-test-suite-app.log
  $(BUILD_APP) libraries-test-suite-app

$(PENTIUM_TEST_SUITE_APP): testworks-specs testworks-plus libraries-test-suite-app
  $(COMPILE) pentium-test-suite-app > $(LOGS)\compile-pentium-test-suite-app.log
  $(BUILD_APP) pentium-test-suite-app

$(DUIM_TEST_SUITE_APP): testworks-specs testworks-plus duim-tests-source
  $(COMPILE) duim-test-suite-app > $(LOGS)\compile-duim-test-suite.log
  $(BUILD_APP) duim-test-suite-app

$(DUIM_GUI_TEST_SUITE): duim-tests-source
  $(COMPILE) duim-gui-test-suite > $(LOGS)\compile-duim-gui-test-suite.log
  $(BUILD_APP) duim-gui-test-suite

$(WIN32_DUIM_GUI_TEST_SUITE): duim-tests-source
  $(COMPILE) win32-duim-gui-test-suite > $(LOGS)\compile-win32-duim-gui-test-suite.log
  $(BUILD_APP) win32-duim-gui-test-suite

$(OLE_TEST): ole-automation-tests-source
  $(COMPILE) ole-test > $(LOGS)\compile-ole-test.log
  $(BUILD_APP) ole-test

$(OLE_COCLASS_TEST): ole-automation-tests-source
  $(COMPILE) ole-coclass-test > $(LOGS)\compile-ole-coclass-test.log
  $(BUILD_APP) ole-coclass-test

$(SQL_ODBC_TEST): testworks-plus sql-odbc-test-source
  $(COMPILE) sql-odbc-test > $(LOGS)\compile-sql-odbc-test.log
  $(BUILD_APP) sql-odbc-test

#
# Examples
#

$(HELLO_WORLD): app-source
  $(COMPILE) hello-world > $(LOGS)\compile-hello-world.log
  $(BUILD_APP) hello-world

$(FACTORIAL_BIG): app-source
  $(COMPILE) factorial-big > $(LOGS)\compile-factorial-big.log
  $(BUILD_APP) factorial-big

$(FACTORIAL_SMALL): app-source
  $(COMPILE) factorial-small > $(LOGS)\compile-factorial-small.log
  $(BUILD_APP) factorial-small

$(STANDALONE_DEUCE): standalone-deuce-source
  $(COMPILE) standalone-deuce > $(LOGS)\compile-standalone-deuce.log
  $(BUILD_APP) standalone-deuce

$(DUIM_RESOURCE_EXAMPLE): app-source
  $(COMPILE) duim-resource-example > $(LOGS)\compile-duim-resource-example.log
  $(BUILD_APP) duim-resource-example

$(LIFE): duim-examples-source
  $(COMPILE) life > $(LOGS)\compile-life.log
  $(BUILD_APP) life

$(OTHELLO): duim-examples-source
  $(COMPILE) othello > $(LOGS)\compile-othello.log
  $(BUILD_APP) othello

$(PENTE): duim-examples-source
  $(COMPILE) pente > $(LOGS)\compile-pente.log
  $(BUILD_APP) pente

$(SCRIBBLE): duim-examples-source
  $(COMPILE) scribble > $(LOGS)\compile-scribble.log
  $(BUILD_APP) scribble

$(TETRIS): duim-examples-source
  $(COMPILE) tetris > $(LOGS)\compile-tetris.log
  $(BUILD_APP) tetris

$(TIC_TAC_TOE): duim-examples-source
  $(COMPILE) tic-tac-toe > $(LOGS)\compile-tic-tac-toe.log
  $(BUILD_APP) tic-tac-toe

$(OLE_SCRIBBLE): app-source
  $(COMPILE) ole-scribble > $(LOGS)\compile-ole-scribble.log
  $(BUILD_APP) ole-scribble

$(OCX_SCRIBBLE): app-source
  $(COMPILE) ocx-scribble > $(LOGS)\compile-ocx-scribble.log
  $(BUILD_APP) ocx-scribble

$(SAMPLE_AUTOMATION_CONTROLLER): app-source
  $(COMPILE) sample-automation-controller > $(LOGS)\compile-sample-automation-controller.log
  $(BUILD_APP) sample-automation-controller

$(SAMPLE_AUTOMATION_SERVER): app-source
  $(COMPILE) sample-automation-server > $(LOGS)\compile-sample-automation-server.log
  $(BUILD_APP) sample-automation-server

$(SAMPLE_OLE_CONTAINER): app-source
  $(COMPILE) sample-ole-container > $(LOGS)\compile-sample-ole-container.log
  $(BUILD_APP) sample-ole-container

$(SAMPLE_OLE_SERVER): app-source
  $(COMPILE) sample-ole-server > $(LOGS)\compile-sample-ole-server.log
  $(BUILD_APP) sample-ole-server

$(WIN32_OLE_SERVER): app-source
  $(COMPILE) win32-ole-server > $(LOGS)\compile-win32-ole-server.log
  $(BUILD_APP) win32-ole-server

$(WINDOWS_FFI_EXAMPLE): app-source
  $(COMPILE) windows-ffi-example > $(LOGS)\compile-windows-ffi-example.log
  $(BUILD_APP) windows-ffi-example

$(WINDOWS_GADGETS_EXAMPLE): app-source
  $(COMPILE) windows-gadgets-example > $(LOGS)\compile-windows-gadgets-example.log
  $(BUILD_APP) windows-gadgets-example

$(WINDOWS_RESOURCE_EXAMPLE): app-source
  $(COMPILE) windows-resource-example > $(LOGS)\compile-windows-resource-example.log
  $(BUILD_APP) windows-resource-example

$(SQL_EXAMPLE): databases-source
  $(COMPILE) sql-example > $(LOGS)\compile-sql-example.log
  $(BUILD_APP) sql-example

# eof
