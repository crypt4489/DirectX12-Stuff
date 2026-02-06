#define WIN32_LEAN_AND_MEAN

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


HINSTANCE hInst;
HWND hwnd;

bool shouldClose = false; 
LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp);
int CreateDX12Window(int requestedDimensionX, int requestDimensionY);
int PollDX12WindowEvents();

#include <d3d12.h>
#include <dxgi1_6.h>
#include <d3dcompiler.h>
#include <DirectXMath.h>
#include "d3dx12.h"
using namespace DirectX;

constexpr uint32_t MAX_FRAMES_IN_FLIGHT = 3;
bool g_UseWarp = false;
bool g_IsInitialized = false;

ID3D12Device2* deviceHandle;

ID3D12CommandQueue* queueHandle;
IDXGISwapChain4* swapChain;

ID3D12Resource* swapChainImages[MAX_FRAMES_IN_FLIGHT];

ID3D12Resource* swapChainDepthImages[MAX_FRAMES_IN_FLIGHT];

ID3D12GraphicsCommandList* graphicCommandBuffers[MAX_FRAMES_IN_FLIGHT];

ID3D12CommandAllocator* graphicCommandPools[MAX_FRAMES_IN_FLIGHT];

ID3D12CommandAllocator* transferCommandPool;

ID3D12DescriptorHeap* globalRTVDescriptorHeap;
ID3D12DescriptorHeap* globalDSVDescriptorHeap;


UINT globalRTVDescriptorSize;
UINT globalDSVDescriptorSize;
UINT currentFrame;


ID3D12Fence* g_Fence;

uint64_t g_FenceValue = 0;


uint64_t g_FrameFenceValues[MAX_FRAMES_IN_FLIGHT] = {};


HANDLE g_FenceEvent = INVALID_HANDLE_VALUE;

bool g_VSync = true;


bool g_TearingSupported = false;

enum ShaderType
{
    VERTEX = 0,
    PIXEL = 1
};

struct ShaderHandles
{
    ShaderType type;
    ID3DBlob* shader;
};

ShaderHandles shaderHandles[2];

ID3D12Resource* globalHostBufferResource, *globalDeviceBufferResource;

ID3D12Resource* stagingBuffers[MAX_FRAMES_IN_FLIGHT];


ID3D12Resource* bgraImageMemoryPool;

struct TextureDetails
{
    DXGI_FORMAT type;
    uint32_t dataSize;
    uint32_t width, height, miplevels;
    char* data;
};


void ParseBMP(TextureDetails* details, const char* name);

ID3D12CommandQueue* CreateCommandQueue(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type);
ID3D12Device2* CreateDevice(IDXGIAdapter4* adapter, bool debug);
bool CheckTearingSupport();
void EnableRuntimeValidation();
IDXGIAdapter4* GetAdapter(UINT createFactoryFlags);
IDXGISwapChain4* CreateSwapChain(HWND hWnd, ID3D12CommandQueue* commandQueue, int width, int height, int bufferCount, UINT debug);
int Render();
void Flush(ID3D12CommandQueue* commandQueue, ID3D12Fence* fence, uint64_t& fenceValue, HANDLE fenceEvent);
void WaitForFenceValue(ID3D12Fence* fence, uint64_t fenceValue, HANDLE fenceEvent, DWORD duration);
uint64_t Signal(ID3D12CommandQueue* commandQueue, ID3D12Fence* fence, uint64_t& fenceValue);
ID3D12Fence* CreateFence(ID3D12Device2* device);
HANDLE CreateEventHandle();
ID3D12GraphicsCommandList* CreateCommandList(ID3D12Device2* device,
    ID3D12CommandAllocator* commandAllocator, D3D12_COMMAND_LIST_TYPE type);
ID3D12CommandAllocator* CreateCommandAllocator(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type);
int CreateRenderTargetView(ID3D12Device2* device, IDXGISwapChain4* swapChain, ID3D12DescriptorHeap* descriptorHeap, ID3D12Resource** outBuffers, UINT rtvDescriptorSize);
ID3D12DescriptorHeap* CreateDescriptorHeap(ID3D12Device2* device, D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize);
void ReleaseD3D12Resources();
int CreateDepthStencilView(ID3D12Device2* device, ID3D12DescriptorHeap* descriptorHeap, ID3D12Resource** outBuffers, UINT dsvDescriptorSize);
ID3DBlob* CreateShaderBlob(const char* shaderfile);
ID3D12Resource* CreateHostBuffer(ID3D12Device2* device, UINT size, D3D12_RESOURCE_FLAGS flags);
ID3D12Resource* CreateDeviceLocalBuffer(ID3D12Device2* device, UINT size, D3D12_RESOURCE_FLAGS flags);
void WriteToHostMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies);
void WriteToDeviceLocalMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies, D3D12_RESOURCE_STATES destinationStage);

ID3D12Resource* CreateImageResource(ID3D12Device2* device, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);
void WriteToImageDeviceLocalMemory(ID3D12Resource* imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers);


struct Camera
{
    XMMATRIX proj;
    XMMATRIX view;
};

XMMATRIX world[2];

Camera cam;


struct PipelineObject
{
    ID3D12RootSignature* rootSignature;
    ID3D12PipelineState* pipelineState;
    ID3D12DescriptorHeap* descriptorHeap[8];
    int descriptorHeapPointer[8];
    int descriptorHeapCount[8];
    int descriptorRootParameterIndices[8];
    int descriptorHeapSelection[8]; //number of descriptor tables
    int descriptorTableCount;
    D3D_PRIMITIVE_TOPOLOGY topology;//D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST
    int vertexCount;
    int instanceCount;
};


PipelineObject triangles[2];

ID3D12PipelineState* CreatePipelineStateObject(ID3D12Device2* device, ID3D12RootSignature* _rootSignature, ShaderHandles* handles, int count);
ID3D12RootSignature* CreateRootSignature(ID3D12Device2* device, CD3DX12_ROOT_PARAMETER* rootParameters, UINT parameterCount);
ID3D12RootSignature* CreateGenericRootSignature();


struct HostBuffer
{
    ID3D12Resource* bufferHandle;
    size_t sizeOfAlloc;
    size_t currentPointer;
};


struct DescriptorHeap
{
    ID3D12DescriptorHeap* descriptorHeap;
    int descriptorHeapHandlePointer;
    int maxDescriptorHeapHandles;
    UINT descriptorHeapHandleSize;
};

DescriptorHeap mainSRVDescriptorHeap;
DescriptorHeap mainSVDescriptorHeap;


struct Allocation
{
    int copies;
    size_t offset;
    size_t alignment; 
    size_t stridesize; //amount of memory taken up per copy
    size_t requestedsize;
    ID3D12Resource* bufferHandle;
    size_t totalDeviceAlloc;
};

static int allocationHandleIndex = 0;
static size_t allocOffset = 0;

Allocation allocationHandle[50];

enum DescriptorType
{
    PUSHCONSTANTS = 0,
    CONSTANTBUFFER = 1,
    UNIFORMBUFFER = 2,
    SAMPLER2D = 3,
    IMAGESRV = 4,
};

struct DescriptorTypeHeader
{
    DescriptorType type;
};

struct DescriptorTypeConstantBuffer : public DescriptorTypeHeader
{
    int allocationIndex;
};

struct DescriptorTypeUniformBuffer : public DescriptorTypeHeader
{
    int allocationIndex;
    int numberOfElements;
    DXGI_FORMAT format;
};

struct DescriptorTypeImageSRV : public DescriptorTypeHeader
{
    DXGI_FORMAT format;
    ID3D12Resource* image;
};

void CreateDescriptorHeapManager(DescriptorHeap* heap, UINT maxDescriptorHandles, D3D12_DESCRIPTOR_HEAP_TYPE type, D3D12_DESCRIPTOR_HEAP_FLAGS flags);
int CreateUsefulDescriptor(int index, DescriptorHeap* heap);
int CreateDescriptorTable(DescriptorTypeHeader** header, int descriptorCount, int frameCount, DescriptorHeap* heap);
void CreateSRVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT offset, UINT numCount, UINT size, DXGI_FORMAT format, DescriptorHeap* heap, D3D12_SRV_DIMENSION dimension);
void CreateImageSRVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT mipsLevels, DXGI_FORMAT format, DescriptorHeap* heap, D3D12_SRV_DIMENSION dimension);
void CreateUAVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT offset, UINT numCount, UINT size, DXGI_FORMAT format, DescriptorHeap* heap);
void CreateCBVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT offset, UINT size, DescriptorHeap* heap);
void CreateImageSampler(ID3D12Device2* device, DescriptorHeap* samplerDescriptorHeap);
int AllocFromHostBuffer(size_t size, size_t alignment, int copies)
{
 
    size_t allocSize = (size + alignment - 1) & ~((size_t)alignment - 1);

    allocSize *= copies;

    size_t location = (allocOffset + alignment -1) & ~(alignment-1);

    allocOffset += (allocSize + (location - allocOffset));

    int index = allocationHandleIndex++;
    allocationHandle[index].bufferHandle =globalHostBufferResource;
    allocationHandle[index].offset = location;
    allocationHandle[index].totalDeviceAlloc = allocSize;
    allocationHandle[index].requestedsize = size;
    allocationHandle[index].alignment = alignment;
    allocationHandle[index].copies = copies;
    allocationHandle[index].stridesize = (size + alignment - 1) & ~(alignment - 1);

    return index;
}

size_t allocOffset2 = 0;

int AllocFromDeviceBuffer(size_t size, size_t alignment, int copies)
{

    size_t allocSize = (size + alignment - 1) & ~((size_t)alignment - 1);

    allocSize *= copies;

    size_t location = (allocOffset2 + alignment - 1) & ~(alignment - 1);

    allocOffset2 += (allocSize + (location - allocOffset2));

    int index = allocationHandleIndex++;
    allocationHandle[index].bufferHandle = globalDeviceBufferResource;
    allocationHandle[index].offset = location;
    allocationHandle[index].totalDeviceAlloc = allocSize;
    allocationHandle[index].requestedsize = size;
    allocationHandle[index].alignment = alignment;
    allocationHandle[index].copies = copies;
    allocationHandle[index].stridesize = (size + alignment - 1) & ~(alignment - 1);

    return index;
}

DescriptorTypeUniformBuffer cameraBufferResource;
DescriptorTypeConstantBuffer world1Resource, world2Resource;
DescriptorTypeHeader sampler = { SAMPLER2D };
DescriptorTypeImageSRV imageSrv = {};

void DoSceneStuff()
{
    int cameraData = AllocFromHostBuffer(sizeof(Camera), sizeof(Camera), MAX_FRAMES_IN_FLIGHT);
    int world1Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 256, MAX_FRAMES_IN_FLIGHT);
    int world2Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 256, MAX_FRAMES_IN_FLIGHT);

    WriteToHostMemory(cameraData, &cam, sizeof(Camera), 0, MAX_FRAMES_IN_FLIGHT);

    WriteToDeviceLocalMemory(world1Data, world, 64, 0, MAX_FRAMES_IN_FLIGHT, D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER);

    WriteToDeviceLocalMemory(world2Data, &world[1], 64, 0, MAX_FRAMES_IN_FLIGHT, D3D12_RESOURCE_STATE_VERTEX_AND_CONSTANT_BUFFER);

    cameraBufferResource.type = UNIFORMBUFFER;
    cameraBufferResource.format = DXGI_FORMAT_UNKNOWN;
    cameraBufferResource.allocationIndex = cameraData;
    cameraBufferResource.numberOfElements = 1;
    
    world1Resource.type = CONSTANTBUFFER;
    world1Resource.allocationIndex = world1Data;

    world2Resource.type = CONSTANTBUFFER;
    world2Resource.allocationIndex = world2Data;



    TextureDetails details{};

    ParseBMP(&details, "face1.bmp");


    bgraImageMemoryPool = CreateImageResource(deviceHandle, details.width, details.height, 1, details.miplevels, D3D12_RESOURCE_FLAG_NONE, details.type, D3D12_RESOURCE_DIMENSION_TEXTURE2D);


    WriteToImageDeviceLocalMemory(bgraImageMemoryPool, details.data, details.width, details.height, 4, details.dataSize, details.type, details.miplevels, 1);

    imageSrv.type = IMAGESRV;
    imageSrv.format = details.type;
    imageSrv.image = bgraImageMemoryPool;

    DescriptorTypeHeader* types3[2] = { (DescriptorTypeHeader*)&cameraBufferResource, (DescriptorTypeHeader*)&imageSrv};

    int camSRV = CreateDescriptorTable(types3, 2, MAX_FRAMES_IN_FLIGHT, &mainSRVDescriptorHeap);


    DescriptorTypeHeader* types[1] = { (DescriptorTypeHeader*)&world1Resource  };

    DescriptorTypeHeader* what[1] = { &sampler };

    triangles[0].descriptorHeapPointer[0] = camSRV;
    triangles[1].descriptorHeapPointer[0] = camSRV;
    triangles[0].descriptorHeapPointer[1] = CreateDescriptorTable(types, 1, MAX_FRAMES_IN_FLIGHT, &mainSRVDescriptorHeap);
    triangles[0].descriptorHeapPointer[2] = CreateDescriptorTable(what, 1, MAX_FRAMES_IN_FLIGHT, &mainSVDescriptorHeap);
    triangles[1].descriptorHeapPointer[2] = triangles[0].descriptorHeapPointer[2];
    triangles[0].topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
    triangles[0].instanceCount = 1;
    triangles[0].vertexCount = 6;
    triangles[0].descriptorHeap[0] = mainSRVDescriptorHeap.descriptorHeap;
    triangles[0].descriptorHeap[1] = mainSVDescriptorHeap.descriptorHeap;
    triangles[0].descriptorHeapCount[0] = 2;
    triangles[0].descriptorHeapCount[1] = 1;
    triangles[0].descriptorHeapCount[2] = 1;
    triangles[0].descriptorHeapSelection[0] = 0;
    triangles[0].descriptorHeapSelection[1] = 0;
    triangles[0].descriptorHeapSelection[2] = 1;

    triangles[0].descriptorTableCount = 3;
    triangles[0].descriptorRootParameterIndices[0] = 1;
    triangles[0].descriptorRootParameterIndices[1] = 0;
    triangles[0].descriptorRootParameterIndices[2] = 2;

    triangles[1].topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
    triangles[1].instanceCount = 1;
    triangles[1].vertexCount = 6;
    triangles[1].descriptorHeap[0] = mainSRVDescriptorHeap.descriptorHeap;
    triangles[1].descriptorHeap[1] = mainSVDescriptorHeap.descriptorHeap;
    triangles[1].descriptorHeapSelection[0] = 0;
    triangles[1].descriptorHeapSelection[1] = 0;
    triangles[1].descriptorHeapSelection[2] = 1;
    triangles[1].descriptorHeapCount[0] = 2;
    triangles[1].descriptorHeapCount[1] = 1;
    triangles[1].descriptorHeapCount[2] = 1;
    triangles[1].pipelineState = triangles[0].pipelineState;
    triangles[1].rootSignature = triangles[0].rootSignature;

    triangles[1].descriptorTableCount = 3;

    triangles[1].descriptorRootParameterIndices[0] = 1;
    triangles[1].descriptorRootParameterIndices[1] = 0;
    triangles[1].descriptorRootParameterIndices[2] = 2;

    DescriptorTypeHeader* types2[1] = { (DescriptorTypeHeader*)&world2Resource };

    triangles[1].descriptorHeapPointer[1] = CreateDescriptorTable(types2, 1, MAX_FRAMES_IN_FLIGHT, &mainSRVDescriptorHeap);





}

int main()
{
    cam.view = XMMatrixLookAtRH(XMVectorSet(0.0f, 0.0f, 10.0f, 0.0f), XMVectorSet(0.0f, 0.0f, 0.0f, 0.0f), XMVectorSet(0.0f, 1.0f, 0.0f, 0.0f));
    constexpr float fov = XMConvertToRadians(60.0f);
    float aspect = 800.0f / 600.0f;
    float nearZ = 0.1f;
    float farZ = 1000.0f;

    cam.proj = XMMatrixPerspectiveFovRH(fov, aspect, nearZ, farZ);

    if (CreateDX12Window(800, 600) < 0)
        return -1;

    EnableRuntimeValidation();

    IDXGIAdapter4* adapter = GetAdapter(DXGI_CREATE_FACTORY_DEBUG);
    if (!adapter)
        goto end;

    deviceHandle = CreateDevice(adapter, true);
    if (!deviceHandle)
        goto end;

    queueHandle = CreateCommandQueue(deviceHandle, D3D12_COMMAND_LIST_TYPE_DIRECT);
    if (!queueHandle)
        goto end;

    swapChain = CreateSwapChain(
        hwnd,
        queueHandle,
        800,
        600,
        MAX_FRAMES_IN_FLIGHT,
        0
    );
    if (!swapChain)
        goto end;

    globalRTVDescriptorHeap = CreateDescriptorHeap(
        deviceHandle,
        D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
        MAX_FRAMES_IN_FLIGHT,
        D3D12_DESCRIPTOR_HEAP_FLAG_NONE, &globalRTVDescriptorSize
    );
    if (!globalRTVDescriptorHeap)
        goto end;

    globalDSVDescriptorHeap = CreateDescriptorHeap(
        deviceHandle,
        D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
        MAX_FRAMES_IN_FLIGHT,
        D3D12_DESCRIPTOR_HEAP_FLAG_NONE, &globalDSVDescriptorSize
    );

    if (!globalDSVDescriptorHeap)
        goto end;


  

    CreateDescriptorHeapManager(&mainSRVDescriptorHeap, MAX_FRAMES_IN_FLIGHT * 100, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE);

    CreateDescriptorHeapManager(&mainSVDescriptorHeap, MAX_FRAMES_IN_FLIGHT * 100, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER, D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE);

    if (CreateRenderTargetView(
        deviceHandle,
        swapChain,
        globalRTVDescriptorHeap,
        swapChainImages, globalRTVDescriptorSize) < 0)
    {
        goto end;
    }

    if (CreateDepthStencilView(
        deviceHandle,
        globalDSVDescriptorHeap,
        swapChainDepthImages, globalDSVDescriptorSize) < 0)
    {
        goto end;
    }

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        graphicCommandPools[i] =
            CreateCommandAllocator(deviceHandle, D3D12_COMMAND_LIST_TYPE_DIRECT);

        if (!graphicCommandPools[i])
            goto end;
    }

    transferCommandPool = CreateCommandAllocator(deviceHandle, D3D12_COMMAND_LIST_TYPE_DIRECT);

    if (!transferCommandPool)
        goto end;

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        graphicCommandBuffers[i] = CreateCommandList(
            deviceHandle,
            graphicCommandPools[i],
            D3D12_COMMAND_LIST_TYPE_DIRECT
        );
        if (!graphicCommandBuffers[i])
          goto end;
    }
    

    g_Fence = CreateFence(deviceHandle);
    if (!g_Fence)
        goto end;

    g_FenceEvent = CreateEventHandle();
    if (g_FenceEvent == INVALID_HANDLE_VALUE)
        goto end;

    world[0] = XMMatrixIdentity();
    world[1] = XMMatrixTranslation(2.0, 2.0, 0.0);

    shaderHandles[0] = { VERTEX, CreateShaderBlob("VS.bin") };
    shaderHandles[1] = { PIXEL, CreateShaderBlob("PS.bin") };

    triangles[0].rootSignature = CreateGenericRootSignature();

    triangles[0].pipelineState = CreatePipelineStateObject(deviceHandle, triangles[0].rootSignature, shaderHandles, 2);

    globalHostBufferResource = CreateHostBuffer(deviceHandle, 4096, D3D12_RESOURCE_FLAG_NONE);

    globalDeviceBufferResource = CreateDeviceLocalBuffer(deviceHandle, 4096, D3D12_RESOURCE_FLAG_NONE);

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        stagingBuffers[i] = CreateHostBuffer(deviceHandle, 64'000'000, D3D12_RESOURCE_FLAG_NONE);
    }


    

    DoSceneStuff();   

    g_IsInitialized = true;

    while (!shouldClose)
    {
        PollDX12WindowEvents();
        int ret = Render();
        if (ret < 0) break;
    }

    Flush(queueHandle, g_Fence, g_FenceValue, g_FenceEvent);

end:
    if (adapter)
    {
        adapter->Release();
        adapter = nullptr;
    }

    ReleaseD3D12Resources();

 

    CloseWindow(hwnd);
    return 0;
}

ID3DBlob* CreateShaderBlob(const char* shaderfile)
{
    ID3DBlob* shaderBlob;

    WCHAR str[250]; 
    mbstowcs(str, shaderfile, 250);

    HRESULT result = D3DReadFileToBlob(str, &shaderBlob);

    if (FAILED(result))
    {
        return NULL;
    }

    return shaderBlob;
    
}

ID3D12PipelineState* CreatePipelineStateObject(ID3D12Device2* device, ID3D12RootSignature* _rootSignature, ShaderHandles* handles, int count)
{
    ID3D12PipelineState* pipelineState;

    HRESULT hr;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC desc{};
    desc.pRootSignature = _rootSignature;


    for (int i = 0; i < count; i++)
    {
        SIZE_T bcLen = shaderHandles[i].shader->GetBufferSize();
        void* shaderData = shaderHandles[i].shader->GetBufferPointer();
        switch (handles[i].type)
        {
        case VERTEX:
            desc.VS.BytecodeLength = bcLen;
            desc.VS.pShaderBytecode = shaderData;
            break;
        case PIXEL:
            desc.PS.BytecodeLength = bcLen;
            desc.PS.pShaderBytecode = shaderData;
            break;
        }
    }

    desc.InputLayout = { nullptr, 0 };
    desc.BlendState = CD3DX12_BLEND_DESC(D3D12_DEFAULT);
    desc.SampleMask = UINT_MAX;
    desc.DepthStencilState = CD3DX12_DEPTH_STENCIL_DESC(D3D12_DEFAULT);
    desc.DepthStencilState.DepthEnable = TRUE;
    desc.DepthStencilState.DepthWriteMask = D3D12_DEPTH_WRITE_MASK_ALL;
    desc.DepthStencilState.DepthFunc = D3D12_COMPARISON_FUNC_LESS_EQUAL;
    desc.DepthStencilState.StencilEnable = FALSE;
    desc.DSVFormat = DXGI_FORMAT_D32_FLOAT;
    desc.NumRenderTargets = 1;
    desc.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
    desc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
    desc.RasterizerState = CD3DX12_RASTERIZER_DESC(D3D12_DEFAULT);
    desc.SampleDesc.Count = 1;
    desc.SampleDesc.Quality = 0;

    hr = device->CreateGraphicsPipelineState(
        &desc,
        IID_PPV_ARGS(&pipelineState)
    );

    if (FAILED(hr))
    {
        printf("Failed to create PSO\n");
    }

    return pipelineState;
}

void ReleaseD3D12Resources()
{

    if (transferCommandPool)
        transferCommandPool->Release();

    if (triangles[0].rootSignature)
        triangles[0].rootSignature->Release();

    if (triangles[0].pipelineState)
        triangles[0].pipelineState->Release();

    if (globalHostBufferResource)
       globalHostBufferResource->Release();

    if (globalDeviceBufferResource)
        globalDeviceBufferResource->Release();

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        if (stagingBuffers[i])
            stagingBuffers[i]->Release();
    }

    for (int i = 0; i < 2; i++)
    {
        if (shaderHandles[i].shader)
        {
            shaderHandles[i].shader->Release();
        }
    }
     
    // --- Ensure GPU is not using resources (recommended) ---
    if (g_Fence && queueHandle)
    {
        g_FenceValue++;
        queueHandle->Signal(g_Fence, g_FenceValue);

        if (g_Fence->GetCompletedValue() < g_FenceValue)
        {
            g_Fence->SetEventOnCompletion(g_FenceValue, g_FenceEvent);
            WaitForSingleObject(g_FenceEvent, INFINITE);
        }
    }

    // --- Swapchain back buffers ---
    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; ++i)
    {
        if (swapChainImages[i])
        {
            swapChainImages[i]->Release();
            swapChainImages[i] = nullptr;
        }

        if (swapChainDepthImages[i])
        {
            swapChainDepthImages[i]->Release();
            swapChainDepthImages[i] = nullptr;
        }
    }

    // --- Command allocators ---
    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; ++i)
    {
        if (graphicCommandPools[i])
        {
            graphicCommandPools[i]->Release();
            graphicCommandPools[i] = nullptr;
        }
    }

    // --- Command list ---
    
        for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
        {
            if (graphicCommandBuffers[i])
            {
                graphicCommandBuffers[i]->Release();
                graphicCommandBuffers[i] = nullptr;
            }
        }
   

    // --- Descriptor heap ---
    if (globalRTVDescriptorHeap)
    {
        globalRTVDescriptorHeap->Release();
        globalRTVDescriptorHeap = nullptr;
    }

    if (mainSRVDescriptorHeap.descriptorHeap)
    {
        mainSRVDescriptorHeap.descriptorHeap->Release();
        mainSRVDescriptorHeap.descriptorHeap = nullptr;
    }

    if (mainSVDescriptorHeap.descriptorHeap)
    {
        mainSVDescriptorHeap.descriptorHeap->Release();
        mainSVDescriptorHeap.descriptorHeap = nullptr;
    }


    if (globalDSVDescriptorHeap)
    {
        globalDSVDescriptorHeap->Release();
        globalDSVDescriptorHeap = nullptr;
    }

    // --- Fence ---
    if (g_Fence)
    {
        g_Fence->Release();
        g_Fence = nullptr;
    }

    // --- Fence event ---
    if (g_FenceEvent != INVALID_HANDLE_VALUE)
    {
        CloseHandle(g_FenceEvent);
        g_FenceEvent = INVALID_HANDLE_VALUE;
    }

    // --- Swapchain ---
    if (swapChain)
    {
        swapChain->Release();
        swapChain = nullptr;
    }

    // --- Command queue ---
    if (queueHandle)
    {
        queueHandle->Release();
        queueHandle = nullptr;
    }

    // --- Device (last) ---
    if (deviceHandle)
    {
        deviceHandle->Release();
        deviceHandle = nullptr;
    }
}


ID3D12Device2* CreateDevice(IDXGIAdapter4* adapter, bool debug)
{
    ID3D12Device2* d3d12Device2;

    if (FAILED(D3D12CreateDevice(adapter, D3D_FEATURE_LEVEL_11_0, IID_PPV_ARGS(&d3d12Device2))))
    {
        printf("Failed to create device with given adapter\n");
        return NULL;
    }
    if (debug)
    {
        ID3D12InfoQueue* infoQueue;
        if (SUCCEEDED((d3d12Device2->QueryInterface(IID_PPV_ARGS(&infoQueue)))))
        {
            infoQueue->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_CORRUPTION, TRUE);
            infoQueue->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_ERROR, TRUE);
            infoQueue->SetBreakOnSeverity(D3D12_MESSAGE_SEVERITY_WARNING, TRUE);

            D3D12_MESSAGE_SEVERITY Severities[] =
            {
                D3D12_MESSAGE_SEVERITY_INFO
            };

            D3D12_MESSAGE_ID DenyIds[1] = {
                /*

            D3D12_MESSAGE_ID_CLEARRENDERTARGETVIEW_MISMATCHINGCLEARVALUE,  


            D3D12_MESSAGE_ID_MAP_INVALID_NULLRANGE,                         


            D3D12_MESSAGE_ID_UNMAP_INVALID_NULLRANGE,                      
            */

            };

            D3D12_INFO_QUEUE_FILTER NewFilter = {};

            NewFilter.DenyList.NumSeverities = _countof(Severities);


            NewFilter.DenyList.pSeverityList = Severities;

            /*
            if (FAILED(infoQueue->PushStorageFilter(&NewFilter))) {
                printf("Cannot set debug levels when requested");
            }
            */
            infoQueue->Release();
        }
    }
    

    return d3d12Device2;
}


ID3D12CommandQueue* CreateCommandQueue(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type)
{
    ID3D12CommandQueue* d3d12CommandQueue = NULL;

    D3D12_COMMAND_QUEUE_DESC desc = {};


    desc.Type = type;


    desc.Priority = D3D12_COMMAND_QUEUE_PRIORITY_NORMAL;


    desc.Flags = D3D12_COMMAND_QUEUE_FLAG_NONE;


    desc.NodeMask = 0;

    if (FAILED(device->CreateCommandQueue(&desc, IID_PPV_ARGS(&d3d12CommandQueue))))
    {
        printf("Cannot create command queue\n");
    }

    return d3d12CommandQueue;
}



bool CheckTearingSupport()
{
    BOOL allowTearing = FALSE;

    IDXGIFactory4* dxgiFactory4;

    if (SUCCEEDED(CreateDXGIFactory1(IID_PPV_ARGS(&dxgiFactory4))))
    {
        IDXGIFactory5* dxgiFactory5;
        if (SUCCEEDED(dxgiFactory4->QueryInterface(IID_PPV_ARGS(&dxgiFactory5))))
        {
            if (FAILED(dxgiFactory5->CheckFeatureSupport(
                DXGI_FEATURE_PRESENT_ALLOW_TEARING,
                &allowTearing, sizeof(allowTearing))))
            {
                allowTearing = FALSE;
            }

            dxgiFactory5->Release();
        }

        dxgiFactory4->Release();
    }


    return allowTearing == TRUE;
}


void EnableRuntimeValidation()
{
#if defined(_DEBUG)
    ID3D12Debug1* debug = nullptr;
    if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&debug))))
    {
        debug->EnableDebugLayer();
        debug->SetEnableGPUBasedValidation(TRUE); // optional but excellent
        debug->Release();
    }
#endif
}

IDXGIAdapter4* GetAdapter(UINT createFactoryFlags)
{
    IDXGIFactory4* dxgiFactory;

    createFactoryFlags = DXGI_CREATE_FACTORY_DEBUG;

    if (FAILED(CreateDXGIFactory2(DXGI_CREATE_FACTORY_DEBUG, IID_PPV_ARGS(&dxgiFactory))))
    {
        printf("Could not find compatible adapter\n");
        return NULL;
    }

    IDXGIAdapter1* dxgiAdapter1 = NULL;

    IDXGIAdapter4* dxgiAdapter4 = NULL;

    SIZE_T maxDedicatedVideoMemory = 0;


    for (UINT i = 0; dxgiFactory->EnumAdapters1(i, &dxgiAdapter1) != DXGI_ERROR_NOT_FOUND; ++i)


    {
        DXGI_ADAPTER_DESC1 dxgiAdapterDesc1;


        dxgiAdapter1->GetDesc1(&dxgiAdapterDesc1);



        if ((dxgiAdapterDesc1.Flags & DXGI_ADAPTER_FLAG_SOFTWARE) == 0 &&


            SUCCEEDED(D3D12CreateDevice(dxgiAdapter1, D3D_FEATURE_LEVEL_11_0, __uuidof(ID3D12Device), nullptr)) &&
            dxgiAdapterDesc1.DedicatedVideoMemory > maxDedicatedVideoMemory)
        {
            maxDedicatedVideoMemory = dxgiAdapterDesc1.DedicatedVideoMemory;

            if (FAILED(dxgiAdapter1->QueryInterface(IID_PPV_ARGS(&dxgiAdapter4))))
            {
                printf("Cannot convert to adapter 4 %d\n", i);
            }
        }


    }

    dxgiFactory->Release();

    if (dxgiAdapter1)
    {
        dxgiAdapter1->Release();
    }

    return dxgiAdapter4;



}

IDXGISwapChain4* CreateSwapChain(HWND hWnd, ID3D12CommandQueue* commandQueue, int width, int height, int bufferCount, UINT debug)
{
  
    IDXGIFactory4* dxgiFactory4;

    UINT createFactoryFlags = debug;

    if (FAILED(CreateDXGIFactory2(createFactoryFlags, IID_PPV_ARGS(&dxgiFactory4))))
    {
        printf("Failed to create factory for creating swap chain\n");
        return NULL;
    }

    DXGI_SWAP_CHAIN_DESC1 swapChainDesc = {};


    swapChainDesc.Width = width;


    swapChainDesc.Height = height;


    swapChainDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;


    swapChainDesc.Stereo = FALSE;


    swapChainDesc.SampleDesc = { 1, 0 };


    swapChainDesc.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;


    swapChainDesc.BufferCount = bufferCount;


    swapChainDesc.Scaling = DXGI_SCALING_STRETCH;


    swapChainDesc.SwapEffect = DXGI_SWAP_EFFECT_FLIP_DISCARD;


    swapChainDesc.AlphaMode = DXGI_ALPHA_MODE_UNSPECIFIED;

    swapChainDesc.Flags = CheckTearingSupport() ? DXGI_SWAP_CHAIN_FLAG_ALLOW_TEARING : 0;

    IDXGISwapChain1* swapChain1 = NULL;

    IDXGISwapChain4* swapChain4 = NULL;
    
    HRESULT hr;
    if (FAILED(hr = dxgiFactory4->CreateSwapChainForHwnd(commandQueue, hwnd, &swapChainDesc, NULL, NULL, &swapChain1)))
    {
        
        printf("Cannot create swapchain\n");
        dxgiFactory4->Release();
        return NULL;
    }

    dxgiFactory4->MakeWindowAssociation(hWnd, DXGI_MWA_NO_ALT_ENTER);

    if (FAILED(swapChain1->QueryInterface(IID_PPV_ARGS(&swapChain4))))
    {
        printf("Cannot conver to type 4 swapchain");
    }

    swapChain1->Release();
    dxgiFactory4->Release();
    return swapChain4;
}

ID3D12DescriptorHeap* CreateDescriptorHeap(ID3D12Device2* device, D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize)
{
    *descriptorSize = device->GetDescriptorHandleIncrementSize(type);

    ID3D12DescriptorHeap* descriptorHeap = NULL;

    D3D12_DESCRIPTOR_HEAP_DESC desc = {};

    desc.NumDescriptors = numDescriptors;

    desc.Type = type;

    desc.Flags = flags;

    if (FAILED(device->CreateDescriptorHeap(&desc, IID_PPV_ARGS(&descriptorHeap))))
    {
        printf("Cannot create descriptor heap\n");
    }

    return descriptorHeap;

}

int CreateRenderTargetView(ID3D12Device2* device, IDXGISwapChain4* swapChain, ID3D12DescriptorHeap* descriptorHeap, ID3D12Resource** outBuffers, UINT rtvDescriptorSize)
{
    CD3DX12_CPU_DESCRIPTOR_HANDLE rtvHandle(descriptorHeap->GetCPUDescriptorHandleForHeapStart());

    for (int i = 0; i < MAX_FRAMES_IN_FLIGHT; ++i)
    {
        ID3D12Resource* backBuffer;

        if (FAILED(swapChain->GetBuffer(i, IID_PPV_ARGS(&backBuffer))))
        {
            printf("Failed to get back buffer handle from swapchain\n");
            return -1;
        }

        device->CreateRenderTargetView(backBuffer, NULL, rtvHandle);

        rtvHandle.Offset(rtvDescriptorSize);

        outBuffers[i] = backBuffer;
    }

    return 0;
}

int CreateDepthStencilView(ID3D12Device2* device, ID3D12DescriptorHeap* descriptorHeap, ID3D12Resource** outBuffers, UINT dsvDescriptorSize)
{
    CD3DX12_CPU_DESCRIPTOR_HANDLE dsvHandle(descriptorHeap->GetCPUDescriptorHandleForHeapStart());

    for (int i = 0; i < MAX_FRAMES_IN_FLIGHT; ++i)
    {
        ID3D12Resource* depthBuffer = nullptr;

        D3D12_RESOURCE_DESC depthStencilDesc = {};
        depthStencilDesc.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
        depthStencilDesc.Width = 800;
        depthStencilDesc.Height = 600;
        depthStencilDesc.Format = DXGI_FORMAT_D32_FLOAT;
        depthStencilDesc.SampleDesc.Count = 1;
        depthStencilDesc.SampleDesc.Quality = 0;
        depthStencilDesc.MipLevels = 1;
        depthStencilDesc.Flags = D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL;
        depthStencilDesc.DepthOrArraySize = 1;
        depthStencilDesc.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;

        D3D12_CLEAR_VALUE clearValue = {};
        clearValue.Format = DXGI_FORMAT_D32_FLOAT;
        clearValue.DepthStencil.Depth = 1.0f;
        clearValue.DepthStencil.Stencil = 0;

        CD3DX12_HEAP_PROPERTIES props = CD3DX12_HEAP_PROPERTIES(D3D12_HEAP_TYPE_DEFAULT);

        if (FAILED(device->CreateCommittedResource(&props, D3D12_HEAP_FLAG_NONE, &depthStencilDesc, D3D12_RESOURCE_STATE_DEPTH_WRITE, &clearValue, IID_PPV_ARGS(&depthBuffer))))
        {
            printf("Failed to create depth stencil resource\n");
            return -1;
        }

        D3D12_DEPTH_STENCIL_VIEW_DESC dsvDesc{};

        dsvDesc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
        dsvDesc.Format = DXGI_FORMAT_D32_FLOAT;
        dsvDesc.Flags = D3D12_DSV_FLAG_NONE;
        dsvDesc.Texture2D.MipSlice = 0;

        device->CreateDepthStencilView(depthBuffer, &dsvDesc, dsvHandle);

        dsvHandle.Offset(dsvDescriptorSize);

        outBuffers[i] = depthBuffer;
    }

    return 0;
}


ID3D12CommandAllocator* CreateCommandAllocator(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type)
{
    ID3D12CommandAllocator* commandAllocator = NULL;

    if (FAILED(device->CreateCommandAllocator(type, IID_PPV_ARGS(&commandAllocator))))
    {
        printf("Failed to create command pool\n");
    }

    return commandAllocator;
}



ID3D12GraphicsCommandList* CreateCommandList(ID3D12Device2* device,
    ID3D12CommandAllocator* commandAllocator, D3D12_COMMAND_LIST_TYPE type)

{
    ID3D12GraphicsCommandList* commandList = NULL;

    if (FAILED(device->CreateCommandList(0, type, commandAllocator, nullptr, IID_PPV_ARGS(&commandList))))
    {
        printf("Failed to create command list\n");
        return NULL;
    }

    if (FAILED(commandList->Close()))
    {
        printf("Failed to close the command list\n");
        commandList->Release();
        return NULL;
    }

    return commandList;
}



ID3D12Fence* CreateFence(ID3D12Device2* device)
{
    ID3D12Fence* fence = NULL;
    if (FAILED(device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&fence))))
    {
        printf("Failed to create a fence\n");
    }

    return fence;

}

HANDLE CreateEventHandle()
{
    HANDLE fenceEvent;
    fenceEvent = ::CreateEvent(NULL, FALSE, FALSE, NULL);
    if (fenceEvent == INVALID_HANDLE_VALUE)
        printf("Failed to create fence event.\n");

    return fenceEvent;
}

uint64_t Signal(ID3D12CommandQueue* commandQueue, ID3D12Fence* fence, uint64_t& fenceValue)
{
    uint64_t fenceValueForSignal = ++fenceValue;
    if (FAILED(commandQueue->Signal(fence, fenceValueForSignal)))
    {
        printf("Could not signal fence\n");
        return ~0ui64;
    }
    return fenceValueForSignal;

}

void WaitForFenceValue(ID3D12Fence* fence, uint64_t fenceValue, HANDLE fenceEvent, DWORD duration)
{
    if (fence->GetCompletedValue() < fenceValue)
    {
        if (SUCCEEDED(fence->SetEventOnCompletion(fenceValue, fenceEvent)))
        {
            WaitForSingleObject(fenceEvent, duration);
        }
    }
}

void Flush(ID3D12CommandQueue* commandQueue, ID3D12Fence* fence, uint64_t& fenceValue, HANDLE fenceEvent)

{
    uint64_t fenceValueForSignal = Signal(commandQueue, fence, fenceValue);


    WaitForFenceValue(fence, fenceValueForSignal, fenceEvent, UINT32_MAX);
}

int Render()
{
    static int depthBufferExchange = 0;

    auto commandAllocator = graphicCommandPools[currentFrame];

    auto backBuffer = swapChainImages[currentFrame];

    auto depthBuffer = swapChainDepthImages[currentFrame];

    auto graphicCommandBuffer = graphicCommandBuffers[currentFrame];

    commandAllocator->Reset();

    graphicCommandBuffer->Reset(commandAllocator, nullptr);

    CD3DX12_CPU_DESCRIPTOR_HANDLE dsvHandle(globalDSVDescriptorHeap->GetCPUDescriptorHandleForHeapStart(), currentFrame, globalDSVDescriptorSize);

    CD3DX12_CPU_DESCRIPTOR_HANDLE rtvHandle(globalRTVDescriptorHeap->GetCPUDescriptorHandleForHeapStart(), currentFrame, globalRTVDescriptorSize);

    
    {
        CD3DX12_RESOURCE_BARRIER barrier = CD3DX12_RESOURCE_BARRIER::Transition(backBuffer, D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET);

        graphicCommandBuffer->ResourceBarrier(1, &barrier);

        const FLOAT clearColor[4] = {0.0f, 0.0f, 0.0f, 1.0f};

        graphicCommandBuffer->ClearRenderTargetView(rtvHandle, clearColor, 0, nullptr);
    }

    {
        graphicCommandBuffer->ClearDepthStencilView(dsvHandle, D3D12_CLEAR_FLAG_DEPTH, 1.0, 0, 0, nullptr);
    }


    D3D12_VIEWPORT viewport{};
    viewport.Width = 800.0f;
    viewport.Height = 600.0f;
    viewport.MinDepth = 0.0f;
    viewport.MaxDepth = 1.0f;

    D3D12_RECT scissor{};
    scissor.right = 800;
    scissor.bottom = 600;

    graphicCommandBuffer->RSSetViewports(1, &viewport);
    graphicCommandBuffer->RSSetScissorRects(1, &scissor);

    graphicCommandBuffer->OMSetRenderTargets(1, &rtvHandle, FALSE, &dsvHandle);


    for(int i = 0; i<2; i++)
    {
        graphicCommandBuffer->SetGraphicsRootSignature(triangles[i].rootSignature);

  

        graphicCommandBuffer->SetDescriptorHeaps(2, triangles[i].descriptorHeap);

        graphicCommandBuffer->SetPipelineState(triangles[i].pipelineState);
        graphicCommandBuffer->IASetPrimitiveTopology(triangles[i].topology);

        

        
        for (int j = 0; j < triangles[i].descriptorTableCount; j++)
        {
            int heapindex = triangles[i].descriptorHeapSelection[j];
            int step = (heapindex == 0 ? mainSRVDescriptorHeap.descriptorHeapHandleSize : mainSVDescriptorHeap.descriptorHeapHandleSize);
            CD3DX12_GPU_DESCRIPTOR_HANDLE handle = CD3DX12_GPU_DESCRIPTOR_HANDLE(triangles[i].descriptorHeap[heapindex]->GetGPUDescriptorHandleForHeapStart(), triangles[i].descriptorHeapPointer[j] + (currentFrame * triangles[i].descriptorHeapCount[j]), step);
            graphicCommandBuffer->SetGraphicsRootDescriptorTable(triangles[i].descriptorRootParameterIndices[j], handle);
        }
        
        

        

        
        //graphicCommandBuffer->SetGraphicsRoot32BitConstants(0, 32, &cam, 0);
        graphicCommandBuffer->DrawInstanced(triangles[i].vertexCount, triangles[i].instanceCount, 0, 0);
    }



    {
        CD3DX12_RESOURCE_BARRIER barrier = CD3DX12_RESOURCE_BARRIER::Transition(


            backBuffer,


            D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT);


        graphicCommandBuffer->ResourceBarrier(1, &barrier);
    }

    if (FAILED(graphicCommandBuffer->Close()))
    {
        printf("Cannot finish recording command buffer\n");
        return -1;
    }

    ID3D12CommandList* const commandLists[] = {
        graphicCommandBuffer
    };

    queueHandle->ExecuteCommandLists(_countof(commandLists), commandLists);

    UINT syncInterval = g_VSync ? 1 : 0;


    UINT presentFlags = g_TearingSupported && !g_VSync ? DXGI_PRESENT_ALLOW_TEARING : 0;


    if (FAILED(swapChain->Present(syncInterval, presentFlags)))
    {
        printf("Cannot present image\n");
        return -1;
    }

    g_FrameFenceValues[currentFrame] = Signal(queueHandle, g_Fence, g_FenceValue);

    currentFrame = swapChain->GetCurrentBackBufferIndex();

    WaitForFenceValue(g_Fence, g_FrameFenceValues[currentFrame], g_FenceEvent, UINT32_MAX);

    return 0;
}


ID3D12Resource* CreateHostBuffer(ID3D12Device2* device, UINT size, D3D12_RESOURCE_FLAGS flags)
{
    D3D12_RESOURCE_DESC bufferDesc = {};
    bufferDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    bufferDesc.Alignment = 0;
    bufferDesc.Width = size;
    bufferDesc.Height = 1;
    bufferDesc.DepthOrArraySize = 1;
    bufferDesc.MipLevels = 1;
    bufferDesc.Format = DXGI_FORMAT_UNKNOWN;
    bufferDesc.SampleDesc.Count = 1;
    bufferDesc.SampleDesc.Quality = 0;
    bufferDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
    bufferDesc.Flags = flags;


    D3D12_HEAP_PROPERTIES heapProps = {};
    heapProps.Type = D3D12_HEAP_TYPE_UPLOAD; //

    ID3D12Resource* buffer = nullptr;
    device->CreateCommittedResource(
        &heapProps,
        D3D12_HEAP_FLAG_NONE,
        &bufferDesc,
        D3D12_RESOURCE_STATE_GENERIC_READ, // initial state
        nullptr,
        IID_PPV_ARGS(&buffer)
    );

  

    return buffer;

}


ID3D12Resource* CreateDeviceLocalBuffer(ID3D12Device2* device, UINT size, D3D12_RESOURCE_FLAGS flags)
{
    D3D12_RESOURCE_DESC bufferDesc = {};
    bufferDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    bufferDesc.Alignment = 0;
    bufferDesc.Width = size;
    bufferDesc.Height = 1;
    bufferDesc.DepthOrArraySize = 1;
    bufferDesc.MipLevels = 1;
    bufferDesc.Format = DXGI_FORMAT_UNKNOWN;
    bufferDesc.SampleDesc.Count = 1;
    bufferDesc.SampleDesc.Quality = 0;
    bufferDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
    bufferDesc.Flags = flags;


    D3D12_HEAP_PROPERTIES heapProps = {};
    heapProps.Type = D3D12_HEAP_TYPE_DEFAULT; //

    ID3D12Resource* buffer = nullptr;
    device->CreateCommittedResource(
        &heapProps,
        D3D12_HEAP_FLAG_NONE,
        &bufferDesc,
        D3D12_RESOURCE_STATE_COMMON, // initial state
        nullptr,
        IID_PPV_ARGS(&buffer)
    );



    return buffer;
}

//D3D12_RESOURCE_DIMENSION_TEXTURE2D


ID3D12Resource* CreateImageResource(ID3D12Device2* device, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension)
{
    D3D12_RESOURCE_DESC imageDesc = {};
    imageDesc.Dimension = dimension;
    imageDesc.Alignment = 0;
    imageDesc.Width = width;
    imageDesc.Height = height;
    imageDesc.DepthOrArraySize = depth;
    imageDesc.MipLevels = mips;
    imageDesc.Format = format;
    imageDesc.SampleDesc.Count = 1;
    imageDesc.SampleDesc.Quality = 0;
    imageDesc.Layout = D3D12_TEXTURE_LAYOUT_UNKNOWN;
    imageDesc.Flags = flags;


    D3D12_HEAP_PROPERTIES heapProps = {};
    heapProps.Type = D3D12_HEAP_TYPE_DEFAULT; //

    ID3D12Resource* imageHandle = nullptr;
    device->CreateCommittedResource(
        &heapProps,
        D3D12_HEAP_FLAG_NONE,
        &imageDesc,
        D3D12_RESOURCE_STATE_COMMON, // initial state
        nullptr,
        IID_PPV_ARGS(&imageHandle)
    );

    return imageHandle;
}

void WriteToDeviceLocalMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies, D3D12_RESOURCE_STATES destinationStage)
{
     void* mappedData = nullptr;
 
    Allocation* alloc = &allocationHandle[allocationIndex];

    ID3D12Resource* stagingBuffer = stagingBuffers[0];

    stagingBuffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData);

    size_t stride = (alloc->requestedsize + alloc->alignment - 1) & ~(alloc->alignment - 1);

    for (int i = 0; i < copies; i++)
    {
        memcpy((void*)(cdata), data, size);
        cdata += stride;
    }


    stagingBuffer->Unmap(0, nullptr);


    transferCommandPool->Reset();

    

   ID3D12GraphicsCommandList* transferCmdBuffer = CreateCommandList(
        deviceHandle,
        transferCommandPool,
        D3D12_COMMAND_LIST_TYPE_DIRECT
    );

   ID3D12CommandQueue* lqueueHandle = CreateCommandQueue(deviceHandle, D3D12_COMMAND_LIST_TYPE_DIRECT);

   ID3D12Fence* fence = CreateFence(deviceHandle);

   HANDLE fenceEvent = CreateEventHandle();

   transferCmdBuffer->Reset(transferCommandPool, nullptr);

   D3D12_RESOURCE_BARRIER preBarrier = {};
   preBarrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
   preBarrier.Transition.pResource = alloc->bufferHandle;
   preBarrier.Transition.StateBefore = D3D12_RESOURCE_STATE_COMMON;
   preBarrier.Transition.StateAfter = D3D12_RESOURCE_STATE_COPY_DEST;
   preBarrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;

   transferCmdBuffer->ResourceBarrier(1, &preBarrier);

   transferCmdBuffer->CopyBufferRegion(alloc->bufferHandle, alloc->offset + offset, stagingBuffer, 0, stride * copies);

   D3D12_RESOURCE_BARRIER postBarrier = {};
   postBarrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
   postBarrier.Transition.pResource = alloc->bufferHandle;
   postBarrier.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
   postBarrier.Transition.StateAfter = destinationStage;
   postBarrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;

   transferCmdBuffer->ResourceBarrier(1, &postBarrier);

   transferCmdBuffer->Close();

   ID3D12CommandList* const commandLists[] = {
       transferCmdBuffer
   };

   lqueueHandle->ExecuteCommandLists(_countof(commandLists), commandLists);
   
   uint64_t fenceValue = 0;

   uint64_t fencesign = Signal(lqueueHandle, fence, fenceValue);

   Flush(lqueueHandle, fence, fenceValue, fenceEvent);

   transferCmdBuffer->Release();
   lqueueHandle->Release();
   fence->Release();
   CloseHandle(fenceEvent);
}

void WriteToImageDeviceLocalMemory(ID3D12Resource* imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers)
{
    void* mappedData = nullptr;


    ID3D12Resource* stagingBuffer = stagingBuffers[0];

    stagingBuffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData);

    size_t stride = ((width * componentCount) + (255)) & ~255;

    for (int i = 0; i < height; i++)
    {
        memcpy((void*)(cdata), data + (i * width * componentCount), width * componentCount);
        cdata += stride;
    }


    stagingBuffer->Unmap(0, nullptr);


    transferCommandPool->Reset();



    ID3D12GraphicsCommandList* transferCmdBuffer = CreateCommandList(
        deviceHandle,
        transferCommandPool,
        D3D12_COMMAND_LIST_TYPE_DIRECT
    );

    ID3D12CommandQueue* lqueueHandle = CreateCommandQueue(deviceHandle, D3D12_COMMAND_LIST_TYPE_DIRECT);

    ID3D12Fence* fence = CreateFence(deviceHandle);

    HANDLE fenceEvent = CreateEventHandle();

    transferCmdBuffer->Reset(transferCommandPool, nullptr);

    D3D12_RESOURCE_BARRIER preBarrier = {};
    preBarrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    preBarrier.Transition.pResource = imageHandle;
    preBarrier.Transition.StateBefore = D3D12_RESOURCE_STATE_COMMON;
    preBarrier.Transition.StateAfter = D3D12_RESOURCE_STATE_COPY_DEST;
    preBarrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;

    transferCmdBuffer->ResourceBarrier(1, &preBarrier);

    D3D12_TEXTURE_COPY_LOCATION dest{}, src{};

    src.pResource = stagingBuffer;
    src.Type = D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT;
    src.PlacedFootprint.Offset = 0;
    src.PlacedFootprint.Footprint.Depth = 1;
    src.PlacedFootprint.Footprint.Width = width;
    src.PlacedFootprint.Footprint.Height = height;
    src.PlacedFootprint.Footprint.Format = format;
    src.PlacedFootprint.Footprint.RowPitch = stride;


    dest.pResource = imageHandle;
    dest.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
    dest.SubresourceIndex = 0;

    transferCmdBuffer->CopyTextureRegion(&dest, 0, 0, 0, &src, NULL);

    D3D12_RESOURCE_BARRIER postBarrier = {};
    postBarrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    postBarrier.Transition.pResource = imageHandle;
    postBarrier.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
    postBarrier.Transition.StateAfter = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
    postBarrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;

    transferCmdBuffer->ResourceBarrier(1, &postBarrier);

    transferCmdBuffer->Close();

    ID3D12CommandList* const commandLists[] = {
        transferCmdBuffer
    };

    lqueueHandle->ExecuteCommandLists(_countof(commandLists), commandLists);

    uint64_t fenceValue = 0;

    uint64_t fencesign = Signal(lqueueHandle, fence, fenceValue);

    Flush(lqueueHandle, fence, fenceValue, fenceEvent);

    transferCmdBuffer->Release();
    lqueueHandle->Release();
    fence->Release();
    CloseHandle(fenceEvent);
}

void CreateImageSampler(ID3D12Device2* device, DescriptorHeap* samplerDescriptorHeap)
{
    CD3DX12_CPU_DESCRIPTOR_HANDLE samplerHandle(samplerDescriptorHeap->descriptorHeap->GetCPUDescriptorHandleForHeapStart(), samplerDescriptorHeap->descriptorHeapHandlePointer, samplerDescriptorHeap->descriptorHeapHandleSize);


    D3D12_SAMPLER_DESC samplerDesc = {};
    samplerDesc.Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR; 
    samplerDesc.AddressU = D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    samplerDesc.AddressV = D3D12_TEXTURE_ADDRESS_MODE_WRAP; 
    samplerDesc.AddressW = D3D12_TEXTURE_ADDRESS_MODE_WRAP; 
    samplerDesc.MipLODBias = 0.0f;                          
    samplerDesc.MaxAnisotropy = 1;                        
    samplerDesc.ComparisonFunc = D3D12_COMPARISON_FUNC_NONE;
    samplerDesc.BorderColor[0] = 0.0f;                      
    samplerDesc.BorderColor[1] = 0.0f;
    samplerDesc.BorderColor[2] = 0.0f;
    samplerDesc.BorderColor[3] = 0.0f;
    samplerDesc.MinLOD = 0.0f;                              
    samplerDesc.MaxLOD = D3D12_FLOAT32_MAX;                

    deviceHandle->CreateSampler(&samplerDesc, samplerHandle);

    samplerDescriptorHeap->descriptorHeapHandlePointer++;
}

int PollDX12WindowEvents()
{
    MSG msg;

    int ret = 0;

    while (PeekMessage(&msg, hwnd, 0, 0, PM_REMOVE))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return ret;
}

LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp);

int CreateDX12Window(int requestedDimensionX, int requestDimensionY)
{
    HINSTANCE hInst = GetModuleHandle(NULL);

    WNDCLASSEX wc = { };

    wc.cbSize = sizeof(wc);
    wc.style = 0;
    wc.lpfnWndProc = winproc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hInst;
    wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)GetStockObject(WHITE_BRUSH);
    wc.lpszMenuName = NULL;
    wc.lpszClassName = TEXT("DX12 Window");
    wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);

    if (!RegisterClassEx(&wc)) {
        MessageBox(NULL, TEXT("Could not register window class"),
            NULL, MB_ICONERROR);
        return -1;
    }

    RECT wr = { 0, 0, 800, 600 };
    DWORD style = WS_OVERLAPPEDWINDOW;
    DWORD exStyle = 0;

    AdjustWindowRectEx(&wr, style, FALSE, exStyle);

    hwnd = CreateWindowEx(exStyle,
        TEXT("DX12 Window"),
        TEXT("DX12 Window"),
        style,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        wr.right - wr.left,
        wr.bottom - wr.top,
        NULL,
        NULL,
        hInst,
        NULL);


    if (!hwnd) {
        MessageBox(NULL, TEXT("Could not create window"), NULL, MB_ICONERROR);
        return -1;
    }

    SetWindowText(hwnd, TEXT("DX12 Window"));
    ShowWindow(hwnd, 1);
    UpdateWindow(hwnd);

    return 0;
}

LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp)
{
    if (wm == WM_DESTROY)
    {
        shouldClose = true;
    }

    return DefWindowProc(hwnd, wm, wp, lp);
}

void WriteToHostMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies)
{
    void* mappedData = nullptr;
 
    Allocation* alloc = &allocationHandle[allocationIndex];

    alloc->bufferHandle->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData);

    size_t stride = (alloc->requestedsize + alloc->alignment - 1) & ~(alloc->alignment - 1);

    for (int i = 0; i < copies; i++)
    {
        memcpy((void*)(cdata+alloc->offset+offset), data, size);
        offset += stride;
    }


    alloc->bufferHandle->Unmap(0, nullptr);

}

void CreateImageSRVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT mipsLevels, DXGI_FORMAT format, DescriptorHeap* heap, D3D12_SRV_DIMENSION dimension)
{


    CD3DX12_CPU_DESCRIPTOR_HANDLE srvHandle(heap->descriptorHeap->GetCPUDescriptorHandleForHeapStart(), heap->descriptorHeapHandlePointer, heap->descriptorHeapHandleSize);

    D3D12_SHADER_RESOURCE_VIEW_DESC srvDesc = {};
    srvDesc.ViewDimension = dimension;
    srvDesc.Format = format; // structured buffer
    srvDesc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    srvDesc.Texture2D.MipLevels = 1;

    device->CreateShaderResourceView(bufferHandle, &srvDesc, srvHandle);

    heap->descriptorHeapHandlePointer++;
}

void CreateSRVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT offset, UINT numCount, UINT size, DXGI_FORMAT format, DescriptorHeap* heap, D3D12_SRV_DIMENSION dimension)
{
   
    UINT firstElement = offset / size;

    CD3DX12_CPU_DESCRIPTOR_HANDLE srvHandle(heap->descriptorHeap->GetCPUDescriptorHandleForHeapStart(), heap->descriptorHeapHandlePointer, heap->descriptorHeapHandleSize);

    D3D12_SHADER_RESOURCE_VIEW_DESC srvDesc = {};
    srvDesc.ViewDimension = dimension;
    srvDesc.Format = format; // structured buffer
    srvDesc.Buffer.FirstElement = firstElement;
    srvDesc.Buffer.NumElements = numCount;
    srvDesc.Buffer.StructureByteStride = size;
    srvDesc.Buffer.Flags = D3D12_BUFFER_SRV_FLAG_NONE;
    srvDesc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;

    device->CreateShaderResourceView(bufferHandle, &srvDesc, srvHandle);

    heap->descriptorHeapHandlePointer++;

}

void CreateUAVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT offset, UINT numCount, UINT size, DXGI_FORMAT format, DescriptorHeap* heap)
{

    UINT firstElement = offset / size;

    CD3DX12_CPU_DESCRIPTOR_HANDLE uavHandle(heap->descriptorHeap->GetCPUDescriptorHandleForHeapStart(), heap->descriptorHeapHandlePointer, heap->descriptorHeapHandleSize);


    D3D12_UNORDERED_ACCESS_VIEW_DESC uavDesc{};

    uavDesc.ViewDimension = D3D12_UAV_DIMENSION_BUFFER;
    uavDesc.Format = DXGI_FORMAT_UNKNOWN; // structured buffer
    uavDesc.Buffer.FirstElement = firstElement;
    uavDesc.Buffer.NumElements = numCount;
    uavDesc.Buffer.StructureByteStride = size;
    uavDesc.Buffer.Flags = D3D12_BUFFER_UAV_FLAG_NONE;


    device->CreateUnorderedAccessView(bufferHandle, nullptr, &uavDesc, uavHandle);

    heap->descriptorHeapHandlePointer++;

}

void CreateCBVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT offset, UINT size, DescriptorHeap* heap)
{

    CD3DX12_CPU_DESCRIPTOR_HANDLE cbvHandle(heap->descriptorHeap->GetCPUDescriptorHandleForHeapStart(), heap->descriptorHeapHandlePointer, heap->descriptorHeapHandleSize);

    D3D12_CONSTANT_BUFFER_VIEW_DESC cbvDesc = {};

    cbvDesc.BufferLocation = bufferHandle->GetGPUVirtualAddress() + offset;
    cbvDesc.SizeInBytes = (size + (255)) & ~255;

    device->CreateConstantBufferView(&cbvDesc, cbvHandle);

    heap->descriptorHeapHandlePointer++;

}

ID3D12RootSignature* CreateRootSignature(ID3D12Device2* device, CD3DX12_ROOT_PARAMETER* rootParameters, UINT parameterCount)
{
    ID3D12RootSignature* rootSignature;

    ID3DBlob* rootSigDescriptorLayout;

    CD3DX12_ROOT_SIGNATURE_DESC rsigDesc = {};

    rsigDesc.Init(parameterCount, rootParameters, 0, nullptr);


    D3D12SerializeRootSignature(&rsigDesc, D3D_ROOT_SIGNATURE_VERSION_1, &rootSigDescriptorLayout, nullptr);

    HRESULT hr = device->CreateRootSignature(0, rootSigDescriptorLayout->GetBufferPointer(), rootSigDescriptorLayout->GetBufferSize(), IID_PPV_ARGS(&rootSignature));

    if (FAILED(hr))
    {
        printf("Failed to create root signature\n");
    }

    rootSigDescriptorLayout->Release();

    return rootSignature;
};


ID3D12RootSignature* CreateGenericRootSignature()
{
    CD3DX12_ROOT_PARAMETER rootParameters[3]{};

    CD3DX12_DESCRIPTOR_RANGE range;
    range.Init(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 0); // 1 SRV at t0

    CD3DX12_DESCRIPTOR_RANGE irange;
    irange.Init(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 1); // 1 SRV at t1

    CD3DX12_DESCRIPTOR_RANGE crange;
    crange.Init(D3D12_DESCRIPTOR_RANGE_TYPE_CBV, 1, 0); // 1 SRV at b0

    CD3DX12_DESCRIPTOR_RANGE srange;
    srange.Init(D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER, 1, 0, 0); // 1 Sampler at s0

    CD3DX12_DESCRIPTOR_RANGE arr[4] = { crange, range, irange, srange };

    rootParameters[0].InitAsDescriptorTable(1, arr, D3D12_SHADER_VISIBILITY_VERTEX);
    rootParameters[1].InitAsDescriptorTable(2, arr + 1,  D3D12_SHADER_VISIBILITY_ALL);
    rootParameters[2].InitAsDescriptorTable(1, arr + 3, D3D12_SHADER_VISIBILITY_PIXEL);

    ID3D12RootSignature* rootSignature = CreateRootSignature(deviceHandle, rootParameters, 3);
    
    return rootSignature;
}

void CreateDescriptorHeapManager(DescriptorHeap* heap, UINT maxDescriptorHandles, D3D12_DESCRIPTOR_HEAP_TYPE type, D3D12_DESCRIPTOR_HEAP_FLAGS flags)
{
    heap->descriptorHeap = CreateDescriptorHeap(deviceHandle, type, maxDescriptorHandles, flags, &heap->descriptorHeapHandleSize);
    heap->maxDescriptorHeapHandles = maxDescriptorHandles;
    heap->descriptorHeapHandlePointer = 0;
}

int CreateUsefulDescriptor(int index, DescriptorHeap* heap)
{
    int ret = -1;
    if (index == 1)
    {
        DescriptorTypeHeader* types[2] = {  (DescriptorTypeHeader*)&world2Resource, (DescriptorTypeHeader*)&cameraBufferResource, };
        ret = CreateDescriptorTable(types, 2, MAX_FRAMES_IN_FLIGHT, heap);
    }
    else
    {
        DescriptorTypeHeader* types[2] = {  (DescriptorTypeHeader*)&world1Resource, (DescriptorTypeHeader*)&cameraBufferResource, };
        ret =  CreateDescriptorTable(types, 2, MAX_FRAMES_IN_FLIGHT, heap);
    }

    return ret;
}

int CreateDescriptorTable(DescriptorTypeHeader** header, int descriptorCount, int frameCount, DescriptorHeap* heap)
{
    int heapstart = heap->descriptorHeapHandlePointer;
    for (int i = 0; i < frameCount; i++)
    {
        for (int j = 0; j < descriptorCount; j++)
        {
            switch (header[j]->type)
            {
            case PUSHCONSTANTS:
                break;
            case CONSTANTBUFFER:
            {
                DescriptorTypeConstantBuffer* cbType = (DescriptorTypeConstantBuffer*)header[j];

                Allocation* alloc = &allocationHandle[cbType->allocationIndex];

                CreateCBVDescriptorHandle(deviceHandle, alloc->bufferHandle, i * alloc->stridesize + alloc->offset, alloc->stridesize , heap);
                break;
            }
            case UNIFORMBUFFER:
            {
                DescriptorTypeUniformBuffer* ubType = (DescriptorTypeUniformBuffer*)header[j];

                Allocation* alloc = &allocationHandle[ubType->allocationIndex];
                CreateSRVDescriptorHandle(deviceHandle,
                    alloc->bufferHandle, i * alloc->stridesize + alloc->offset,
                    ubType->numberOfElements, alloc->requestedsize,
                    ubType->format,
                    heap,
                    D3D12_SRV_DIMENSION_BUFFER
                );
                break;
            }

            case IMAGESRV:
            {
                DescriptorTypeImageSRV* ubType = (DescriptorTypeImageSRV*)header[j];

     
                CreateImageSRVDescriptorHandle(deviceHandle,
                    ubType->image,
                    1,
                    ubType->format,
                    heap,
                    D3D12_SRV_DIMENSION_TEXTURE2D
                );
                break;
            }
            case SAMPLER2D:
                CreateImageSampler(deviceHandle, &mainSVDescriptorHeap);
                break;
            }
        }
    }

    return heapstart;
}


#pragma pack(push, 1)
typedef struct BitmapFileHeader
{
    uint16_t  bfType;
    uint32_t  bfSize;
    uint16_t  bfReserved1;
    uint16_t  bfReserved2;
    uint32_t  bfOffBits;
} BitmapFileHeader;

typedef struct BitmapInfoHeader
{
    uint32_t biSize;
    uint32_t biWidth;
    uint32_t biHeight;
    uint16_t biPlanes;
    uint16_t biBitCount;
    uint32_t biCompression;
    uint32_t biSizeImage;
    uint32_t biXPelsPerMeter;
    uint32_t biYPelsPerMeter;
    uint32_t biClrUsed;
    uint32_t biClrImportant;
} BitmapInfoHeader;
#pragma pack(pop)

static_assert(sizeof(BitmapFileHeader) == 14, "no match for bfh");
static_assert(sizeof(BitmapInfoHeader) == 40, "no match for bih");


void ParseBMP(TextureDetails* details, const char* name)
{
    std::vector<char> data; 
    OSFileHandle outHandle{};

    OSFileFlags openingFlags = READ;

    int nRet = OSOpenFile(name, openingFlags, &outHandle);

    if (nRet)
    {
        return;
    }

    int size = outHandle.fileLength;

    data.resize(size);

    OSReadFile(&outHandle, size, data.data());

    OSCloseFile(&outHandle);

    auto iter = data.begin();
    BitmapFileHeader fh;
    std::copy(iter, iter + sizeof(BitmapFileHeader), reinterpret_cast<char*>(&fh));
    if (fh.bfType != 0x4D42)
    {
        return;
    }

    iter += sizeof(BitmapFileHeader);

    BitmapInfoHeader ih;

    std::copy(iter, iter + sizeof(BitmapInfoHeader), reinterpret_cast<char*>(&ih));

    uint32_t width = details->width = ih.biWidth;
    uint32_t height = details->height = ih.biHeight;

    uint32_t bitcount = ih.biBitCount;
    uint32_t bytesPerRow = 0;

    switch (bitcount)
    {
    case 32:
        details->type = DXGI_FORMAT_B8G8R8A8_UNORM_SRGB;
        bytesPerRow = 4 * width;
        break;
    default:
        break;
    }

    if (ih.biSizeImage)
    {
        details->dataSize = ih.biSizeImage;
    }
    else
    {
        details->dataSize = fh.bfSize - fh.bfOffBits;
    }

    unsigned int uLine;
    unsigned int bottomLine = height - 1;
    uint32_t i = 0;

    int offset = 0;

    uLine = bottomLine;


    auto iter2 = data.data() + (fh.bfOffBits + (uLine * bytesPerRow));

    details->data = new char[details->dataSize];

    auto copy = details->data;

    for (; i < bottomLine; i++)
    {
        memcpy(copy, iter2, bytesPerRow);
        copy += bytesPerRow;
        iter2 -= bytesPerRow;
    }

    memcpy(copy, iter2, bytesPerRow);

    //TexUtils::BGRATexture((char*)details->data, height, width, (bitcount == 24 ? 3 : 4));
    details->miplevels = 1;
}

