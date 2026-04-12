#pragma once

#include <Windows.h>
struct OSFileHandle
{
	unsigned int fileLength;
	int filePointer;
	int osDataHandle;

	OSFileHandle()
	{
		fileLength = -1;
		filePointer = -1;
		osDataHandle = -1;
	}
};

enum OSFileFlags
{
	READ = 1,
	WRITE = 2,
	CREATE = 4,
};

enum OSRelativeFlags
{
	BEGIN = 0,
	CURRENT = 1,
	END = 2,
};

enum OSFileErrorFlags
{
	OS_SUCCESS = 0,
	OS_FAILED_CREATE = -1,
	OS_FAILED_SIZE = -2,
	OS_INVALID_ARGUMENT = -3,
	OS_FAILED_SEEK = -4,
	OS_FAILED_READ = -5,
	OS_FAILED_WRITE = -6,
	OS_FAILED_SEARCH_ITER = -7,
	OS_REACH_ITER_END = -8,
	OS_STD_HANDLE_INVALID = -9,
};

struct OSFileIterator
{
	char currentFileName[250];
	int osDataHandle;

	OSFileIterator()
	{
		osDataHandle = -1;
	}
};

struct OSFileMemoryRequirements
{
	int dataSize;
	int alignment;
};


int OSCreateFile(const char* filename, int nameLength, OSFileFlags flags, OSFileHandle* fileHandle);

int OSOpenFile(const char* filename, int nameLength, OSFileFlags flags, OSFileHandle* fileHandle);

int OSCloseFile(OSFileHandle* fileHandle);

int OSReadFile(OSFileHandle* fileHandle, int size, char* buffer);

int OSSeekFile(OSFileHandle* fileHandle, int pointer, OSRelativeFlags flags);

int OSWriteFile(OSFileHandle* fileHandle, int size, char* buffer);

int OSCreateFileIterator(const char* searchString, int nameLength, OSFileIterator* iterator);

int OSNextFile(OSFileIterator* iterator);

void OSGetSTDInput(OSFileHandle* fileHandle);
void OSGetSTDOutput(OSFileHandle* fileHandle);
void OSGetSTDError(OSFileHandle* fileHandle);


