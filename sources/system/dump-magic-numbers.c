
#include <stddef.h>
#include <stdio.h>

/* TARGET HEADERS */
#include <errno.h>
#include <dirent.h>
#include <pwd.h>
#include <grp.h>
#include <time.h>
#include <sys/stat.h>

#define PRINT_USEDBY(file) \
	printf("\n// Used by %s\n", file)
#define PRINT_CONSTANT(value, name) \
	printf("define constant $%s = %d;\n", name, value);
#define PRINT_SIZEOF(type, name) \
	printf("define constant $%s-size = %zu;\n", name, sizeof(type))
#define PRINT_OFFSETOF(type, member, name) \
	printf("define constant $%s-offset = %zu;\n", name, offsetof(type, member))

int
main(void) {

	printf("Module: system-internals\n");
	printf("License: Public Domain\n");

	printf("\n// WARNING! This file is generated!\n");

	PRINT_USEDBY("file-system/unix-ffi.dylan");

	PRINT_CONSTANT(ENOENT,  "ENOENT");
	PRINT_CONSTANT(EINTR,   "EINTR");
	PRINT_CONSTANT(EACCES,  "EACCES");
	PRINT_CONSTANT(EINVAL,  "EINVAL");
	PRINT_CONSTANT(ETXTBSY, "ETXTBSY");
	PRINT_CONSTANT(EROFS,   "EROFS");

	PRINT_SIZEOF(struct stat, "stat");
	PRINT_OFFSETOF(struct stat, st_mode, "st-mode");
	PRINT_OFFSETOF(struct stat, st_uid,  "st-uid");
	PRINT_OFFSETOF(struct stat, st_gid,  "st-gid");
	PRINT_OFFSETOF(struct stat, st_size, "st-size");
	PRINT_OFFSETOF(struct stat, st_atime, "st-atime");
	PRINT_OFFSETOF(struct stat, st_mtime, "st-mtime");
	PRINT_OFFSETOF(struct stat, st_ctime, "st-ctime");

	PRINT_OFFSETOF(struct passwd, pw_name, "pw-name");
	PRINT_OFFSETOF(struct passwd, pw_dir,  "pw-dir");

	PRINT_OFFSETOF(struct group, gr_name, "gr-name");

	PRINT_OFFSETOF(struct dirent, d_name, "d-name");


	PRINT_USEDBY("unix-date-interface.dylan");

	PRINT_OFFSETOF(struct tm, tm_sec,    "tm-sec");
	PRINT_OFFSETOF(struct tm, tm_min,    "tm-min");
	PRINT_OFFSETOF(struct tm, tm_hour,   "tm-hour");
	PRINT_OFFSETOF(struct tm, tm_mday,   "tm-mday");
	PRINT_OFFSETOF(struct tm, tm_mon,    "tm-mon");
	PRINT_OFFSETOF(struct tm, tm_year,   "tm-year");
	PRINT_OFFSETOF(struct tm, tm_isdst,  "tm-isdst");
	PRINT_OFFSETOF(struct tm, tm_gmtoff, "tm-gmtoff");
	PRINT_OFFSETOF(struct tm, tm_zone,   "tm-zone");

	return 0;
}

