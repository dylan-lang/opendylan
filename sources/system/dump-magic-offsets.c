
#include <stddef.h>
#include <stdio.h>

/* TARGET HEADERS */
#include <dirent.h>
#include <pwd.h>
#include <grp.h>
#include <time.h>
#include <sys/stat.h>

#define PRINT_SIZEOF(typ) \
	printf("%s size is %d\n", #typ, sizeof(typ))
#define PRINT_OFFSETOF(typ, mem) \
	printf("%s member %s is at %d\n", #typ, #mem, offsetof(typ, mem))

int
main(void) {

	printf("\n>>> stuff for sources/system/file-system/unix-ffi.dylan\n");

	PRINT_SIZEOF(struct stat);
	PRINT_OFFSETOF(struct stat, st_mode);
	PRINT_OFFSETOF(struct stat, st_uid);
	PRINT_OFFSETOF(struct stat, st_gid);
	PRINT_OFFSETOF(struct stat, st_size);
	PRINT_OFFSETOF(struct stat, st_atime);
	PRINT_OFFSETOF(struct stat, st_mtime);
	PRINT_OFFSETOF(struct stat, st_ctime);

	PRINT_OFFSETOF(struct passwd, pw_name);
	PRINT_OFFSETOF(struct passwd, pw_dir);

	PRINT_OFFSETOF(struct group, gr_name);

	PRINT_OFFSETOF(struct dirent, d_name);

	printf("\n>>> stuff for sources/system/unix-date-interface.dylan\n");

	PRINT_OFFSETOF(struct tm, tm_sec);
	PRINT_OFFSETOF(struct tm, tm_min);
	PRINT_OFFSETOF(struct tm, tm_hour);
	PRINT_OFFSETOF(struct tm, tm_mday);
	PRINT_OFFSETOF(struct tm, tm_mon);
	PRINT_OFFSETOF(struct tm, tm_year);
	PRINT_OFFSETOF(struct tm, tm_isdst);
	PRINT_OFFSETOF(struct tm, tm_gmtoff);
	PRINT_OFFSETOF(struct tm, tm_zone);

	return 0;
}

