#include <stdio.h>
#include <windows.h>

static void
handle_events(char *buf)
{
	long offset = 0;
	char name[256];
	do {
		FILE_NOTIFY_INFORMATION *fni = (FILE_NOTIFY_INFORMATION *)(buf + offset);

		switch (fni->Action) {
		case FILE_ACTION_MODIFIED: printf("FILE_ACTION_MODIFIED: "); break;
		default:                   printf("unknown (0x%08lx):     ", fni->Action); break;
		}

		int len = WideCharToMultiByte(CP_ACP, 0, fni->FileName, fni->FileNameLength,
		                              name, sizeof(name), 0, 0);
		printf("%*s\n", len, name);

		offset = fni->NextEntryOffset;
	} while (offset);
}

int
main(int argc, char *argv[])
{
	if (argc < 2) {
		printf("Usage: %s PATH [PATH ...]\n", argv[0]);
		return 1;
	}

	/* NOTE: for now assume argv[1] is a directory */
	void *handle = CreateFileA(argv[1], GENERIC_READ, FILE_SHARE_READ, 0, OPEN_ALWAYS,
	                           FILE_FLAG_BACKUP_SEMANTICS, 0);

	if (handle == INVALID_HANDLE_VALUE) {
		printf("Failed to open: %s\n", argv[1]);
		return 1;
	}

	__attribute__ ((aligned(64))) char buf[4096];
	while (1) {
		unsigned long read_bytes;
		int filter  = FILE_NOTIFY_CHANGE_LAST_WRITE;
		/* NOTE: for now this is a blocking call, for the actual implementation we
		 * will probably want to use an IO completion port */
		ReadDirectoryChangesW(handle, buf, sizeof(buf), 0, filter, &read_bytes, 0, 0);
		if (read_bytes)
			handle_events(buf);
	}
	return 0;
}
