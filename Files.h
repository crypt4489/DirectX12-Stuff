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
    OS_REACH_ITER_END = -8
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

int OSCreateFile(const char* filename, OSFileFlags flags, OSFileHandle* fileHandle);

int OSOpenFile(const char* filename, OSFileFlags flags, OSFileHandle* fileHandle);

int OSCloseFile(OSFileHandle* fileHandle);

int OSReadFile(OSFileHandle* fileHandle, int size, char* buffer);

int OSSeekFile(OSFileHandle* fileHandle, int pointer, OSRelativeFlags flags);

int OSWriteFile(OSFileHandle* fileHandle, int size, char* buffer);

int OSCreateFileIterator(const char* searchString, OSFileIterator* iterator);

int OSNextFile(OSFileIterator* iterator);


#include <atomic>

static HANDLE intFileHandles[50];
static std::atomic<int> intFileHandleCounter;

static DWORD ConvertOSFlags(OSFileFlags flags, DWORD* shareMode)
{
    DWORD outflags = 0;
    if (flags & READ) {
        outflags |= GENERIC_READ;
        *shareMode |= FILE_SHARE_READ;
    }

    if (flags & WRITE) {
        outflags |= GENERIC_WRITE;
        *shareMode |= FILE_SHARE_WRITE;
    }

    return outflags;
}

int OSCreateFile(const char* filename, OSFileFlags flags, OSFileHandle* fileHandle)
{
    HANDLE hFile;
    DWORD fileShare = 0;
    DWORD hAccess = ConvertOSFlags(flags, &fileShare);



    hFile = CreateFileA(filename, hAccess, fileShare, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);

    if (hFile == INVALID_HANDLE_VALUE)
    {
        return OS_FAILED_CREATE;
    }

    fileHandle->fileLength = 0;
    fileHandle->filePointer = 0;

    int internalHandlePtr = intFileHandleCounter.fetch_add(1);
    intFileHandles[internalHandlePtr] = hFile;

    fileHandle->osDataHandle = internalHandlePtr;

    return OS_SUCCESS;

}

int OSOpenFile(const char* filename, OSFileFlags flags, OSFileHandle* fileHandle)
{
    HANDLE hFile = INVALID_HANDLE_VALUE;
    DWORD fileShare = 0;
    DWORD hAccess = ConvertOSFlags(flags, &fileShare);

    hFile = CreateFileA(filename, hAccess, fileShare, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);

    if (hFile == INVALID_HANDLE_VALUE)
    {
        return OS_FAILED_CREATE;
    }

    DWORD fileSize = GetFileSize(hFile, NULL);

    if (fileSize == INVALID_FILE_SIZE)
    {
        CloseHandle(hFile);
        return OS_FAILED_SIZE;
    }

    fileHandle->fileLength = fileSize;
    fileHandle->filePointer = 0;

    int internalHandlePtr = intFileHandleCounter.fetch_add(1);
    intFileHandles[internalHandlePtr] = hFile;

    fileHandle->osDataHandle = internalHandlePtr;

    return OS_SUCCESS;
}

int OSCloseFile(OSFileHandle* fileHandle)
{
    HANDLE hFile = intFileHandles[fileHandle->osDataHandle];
    CloseHandle(hFile);
    intFileHandles[fileHandle->osDataHandle] = INVALID_HANDLE_VALUE;
    memset(fileHandle, -1, sizeof(OSFileHandle));
    return OS_SUCCESS;
}

int OSReadFile(OSFileHandle* fileHandle, int size, char* buffer)
{
    HANDLE hFile = intFileHandles[fileHandle->osDataHandle];

    DWORD hBytesRead = 0;

    if (ReadFile(hFile, buffer, size, &hBytesRead, NULL) == FALSE)
    {
        return OS_FAILED_READ;
    }

    fileHandle->filePointer += hBytesRead;

    return hBytesRead;
}

int OSWriteFile(OSFileHandle* fileHandle, int size, char* buffer)
{
    HANDLE hFile = intFileHandles[fileHandle->osDataHandle];

    DWORD hBytesWrite = 0;

    if (WriteFile(hFile, buffer, size, &hBytesWrite, NULL) == FALSE)
    {
        return OS_FAILED_WRITE;
    }

    fileHandle->filePointer += hBytesWrite;

    return hBytesWrite;
}

int OSSeekFile(OSFileHandle* fileHandle, int pointer, OSRelativeFlags flags)
{

    HANDLE hFile = intFileHandles[fileHandle->osDataHandle];

    int currentPointer = fileHandle->filePointer;

    DWORD moveMethod = FILE_BEGIN;

    switch (flags)
    {
    case BEGIN:
        currentPointer = pointer;
        break;
    case CURRENT:
        moveMethod = FILE_CURRENT;
        currentPointer += pointer;
        break;
    case END:
        moveMethod = FILE_END;
        currentPointer = fileHandle->fileLength;
        break;
    default:
        return OS_INVALID_ARGUMENT;
    }

    DWORD nRet = SetFilePointer(hFile, pointer, NULL, moveMethod);

    if (nRet == INVALID_SET_FILE_POINTER)
    {
        return OS_FAILED_SEEK;
    }

    fileHandle->filePointer = currentPointer;

    return OS_SUCCESS;
}

int OSCreateFileIterator(const char* searchString, OSFileIterator* iterator)
{
    if (!searchString || !iterator) return OS_INVALID_ARGUMENT;

    int index = intFileHandleCounter.fetch_add(1);

    WIN32_FIND_DATAA data;

    HANDLE searchIdx = FindFirstFileA(searchString, &data);

    if (searchIdx == INVALID_HANDLE_VALUE)
    {
        return OS_FAILED_SEARCH_ITER;
    }

    intFileHandles[index] = searchIdx;

    strncpy(iterator->currentFileName, data.cFileName, 250);
    iterator->osDataHandle = index;

    return 0;
}

int OSNextFile(OSFileIterator* iterator)
{
    if (!iterator) return OS_INVALID_ARGUMENT;

    int index = iterator->osDataHandle;

    WIN32_FIND_DATAA data;

    BOOL ret = FindNextFileA(intFileHandles[index], &data);

    if (!ret)
    {
        CloseHandle(intFileHandles[index]);
        return OS_REACH_ITER_END;
    }

    strncpy(iterator->currentFileName, data.cFileName, 250);

    return 0;
}