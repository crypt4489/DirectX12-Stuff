#define WIN32_LEAN_AND_MEAN

#include <Windows.h>
#include <string>
#include <array>

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

enum class ShaderResourceType
{
    SAMPLER2D = 1,
    STORAGE_BUFFER = 2,
    UNIFORM_BUFFER = 4,
    CONSTANT_BUFFER = 8,
    IMAGESTORE2D = 16,
    IMAGESTORE3D = 32,
    SAMPLERBINDLESS = 64,
    BUFFER_VIEW = 128,
    SAMPLER3D = 129,
    SAMPLERCUBE = 130,
    SAMPLERSTATE = 131,
    INVALID_SHADER_RESOURCE = 0x7FFFFFFF
};

enum class ShaderResourceAction
{
    SHADERREAD = 1,
    SHADERWRITE = 2,
    SHADERREADWRITE = 3
};

enum ShaderStageTypeBits
{
    VERTEXSHADERSTAGE = 1,
    FRAGMENTSHADERSTAGE = 2,
    COMPUTESHADERSTAGE = 4,
};

typedef int ShaderStageType;


struct ShaderSetLayout
{
    int vulkanDescLayout;
    int bindingCount;
    int resourceStart;
};

struct ShaderResource
{
    ShaderStageType stages;
    ShaderResourceAction action;
    ShaderResourceType type;
    int set;
    int binding;
    int arrayCount;
    int size;
    int offset;
};

struct ShaderResourceSet
{
    int bindingCount;
    int layoutHandle;
    int setCount;
    int barrierCount;
};

struct ShaderResourceHeader
{
    ShaderResourceType type;
    ShaderResourceAction action;
    int binding;
    int arrayCount;
};

struct ShaderMap
{
    ShaderStageType type;
    int shaderReference;
    int GetMapSize() const;
};

struct ShaderGraph
{
    int shaderMapCount;
    int resourceSetCount;
    int resourceCount;

    uintptr_t GetSet(int setIndex);

    uintptr_t GetResource(int resourceIndex);

    uintptr_t GetMap(int mapIndex);

    int GetGraphSize() const;
};

struct ShaderComputeLayout
{
    unsigned long x;
    unsigned long y;
    unsigned long z;
};

struct ShaderGraphReader
{
    struct ShaderXMLTag
    {
        unsigned long hashCode;
    };

    struct ShaderGLSLShaderXMLTag : ShaderXMLTag //followed by shaderNameLen Bytes
    {
        ShaderStageType type;
    };

    struct ShaderComputeLayoutXMLTag : ShaderXMLTag
    {
        ShaderComputeLayout comps;
    };

    struct ShaderResourceItemXMLTag : ShaderXMLTag
    {
        ShaderStageType shaderstage;
        ShaderResourceType resourceType;
        ShaderResourceAction resourceAction;
        int arrayCount;
        int size;
        int offset;
    };

    struct ShaderResourceSetXMLTag : ShaderXMLTag
    {
        int resourceCount;
    };

    static constexpr unsigned long
        hash(char* str);

    static constexpr unsigned long
        hash(const std::string& string);


    static ShaderGraph* CreateShaderGraph(const std::string& filename);

    static int ProcessTag(char* fileData, int currentLocation, unsigned long* hash, bool* opening);

    static int SkipLine(char* fileData, int currentLocation);
    static int ReadValue(char* fileData, int currentLocation, char* str, int* len);

    static int ReadAttributeName(char* fileData, int currentLocation, unsigned long* hash);

    static int ReadAttributeValueHash(char* fileData, int currentLocation, unsigned long* hash);

    static int ReadAttributeValueVal(char* fileData, int currentLocation, unsigned long* val);

    static int ReadAttributes(char* fileData, int currentLocation, unsigned long* hashes, int* stackSize, int valType);

    static int HandleGLSLShader(char* fileData, int currentLocation, uintptr_t* offset, void* shaderData, int* shaderDataSize);

    static int HandleShaderResourceItem(char* fileData, int currentLocation, uintptr_t* offset);

    static constexpr int ASCIIToInt(char* str);

    static int HandleComputeLayout(char* fileData, int currentLocation, uintptr_t* offset);
};



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

#define KiB 1024
#define MiB 1024*1024

#define TEMP_MEM_SIZE 64*MiB
#define STORAGE_MEM_SIZE 128*MiB

static char globalMemoryPool[STORAGE_MEM_SIZE];
static char tempGlobalMemoryPool[TEMP_MEM_SIZE];
static size_t tempGlobalAllocator = 0;
static size_t tempGlobalAllocatorSize = TEMP_MEM_SIZE;
static size_t storageGlobalAllocator = 0;
static size_t storageGlobalAllocatorSize = STORAGE_MEM_SIZE;


void* AllocFromTemp(size_t size, size_t alignment)
{
    size_t current = tempGlobalAllocator;

    if (current + size >= TEMP_MEM_SIZE)
        current = 0;

    current = (current + alignment - 1) & ~(alignment - 1);

    tempGlobalAllocator += (size)+(current - tempGlobalAllocator);

    return (void*)&tempGlobalMemoryPool[current];
}


enum class ComponentFormatType
{
    NO_BUFFER_FORMAT = 0,
    RAW_8BIT_BUFFER = 1,
    R32_UINT = 2,
    R32_SINT = 3,
    R32G32B32A32_SFLOAT = 4,
    R32G32B32_SFLOAT = 5,
    R32G32_SFLOAT = 6,
    R32_SFLOAT = 7,
    R32G32_SINT = 8,
    R8G8_UINT = 9,
    R16G16_SINT = 10,
    R16G16B16_SINT = 11

};

enum class VertexUsage
{
    POSITION = 0,
    TEX0 = 1,
    TEX1 = 2,
    TEX2 = 3,
    TEX3 = 4,
    NORMAL = 5,
    BONES = 6,
    WEIGHTS = 7
};

struct VertexInputDescription
{
    ComponentFormatType format;
    int byteoffset;
    VertexUsage vertexusage;
};

const char* ConvertToSemanticName(VertexUsage usage, UINT* sematicIndex)
{
    switch (usage)
    {
    case VertexUsage::POSITION:
        return "POSITION";

    case VertexUsage::TEX0:
        return "TEXCOORD";

    case VertexUsage::TEX1:
        *sematicIndex = 1;
        return "TEXCOORD";

    case VertexUsage::TEX2:
        *sematicIndex = 2;
        return "TEXCOORD";

    case VertexUsage::TEX3:
        *sematicIndex = 3;
        return "TEXCOORD";

    case VertexUsage::NORMAL:
        return "NORMAL";

    case VertexUsage::BONES:
        return "BLENDINDICES";

    case VertexUsage::WEIGHTS:
        return "BLENDWEIGHT";

    default:
        return "";
    }
}

DXGI_FORMAT ConvertToDXGIFormat(ComponentFormatType format)
{
    switch (format)
    {
    case ComponentFormatType::NO_BUFFER_FORMAT:
        return DXGI_FORMAT_UNKNOWN;

    case ComponentFormatType::RAW_8BIT_BUFFER:
        return DXGI_FORMAT_R8_UINT;

    case ComponentFormatType::R32_UINT:
        return DXGI_FORMAT_R32_UINT;

    case ComponentFormatType::R32_SINT:
        return DXGI_FORMAT_R32_SINT;

    case ComponentFormatType::R32G32B32A32_SFLOAT:
        return DXGI_FORMAT_R32G32B32A32_FLOAT;

    case ComponentFormatType::R32G32B32_SFLOAT:
        return DXGI_FORMAT_R32G32B32_FLOAT;

    case ComponentFormatType::R32G32_SFLOAT:
        return DXGI_FORMAT_R32G32_FLOAT;

    case ComponentFormatType::R32_SFLOAT:
        return DXGI_FORMAT_R32_FLOAT;

    case ComponentFormatType::R32G32_SINT:
        return DXGI_FORMAT_R32G32_SINT;

    case ComponentFormatType::R8G8_UINT:
        return DXGI_FORMAT_R8G8_UINT;

    case ComponentFormatType::R16G16_SINT:
        return DXGI_FORMAT_R16G16_SINT;

    case ComponentFormatType::R16G16B16_SINT:
        return DXGI_FORMAT_UNKNOWN;

    default:
        return DXGI_FORMAT_UNKNOWN;
    }
}

void ConvertVertexLayoutToD3D12InputDesc(VertexInputDescription* inputVertexDesc, D3D12_INPUT_ELEMENT_DESC* desc, int vertexbufferid)
{
    DXGI_FORMAT currFormat = ConvertToDXGIFormat(inputVertexDesc->format);

    desc->Format = currFormat;
    desc->InputSlotClass = D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA;
    desc->SemanticName = ConvertToSemanticName(inputVertexDesc->vertexusage, &desc->SemanticIndex);
    desc->InputSlot = vertexbufferid;
    desc->AlignedByteOffset = inputVertexDesc->byteoffset;
    desc->InstanceDataStepRate = 0;
}

ID3D12Device2* deviceHandle;

ID3D12CommandQueue* queueHandle;
IDXGISwapChain4* swapChain;

ID3D12Resource* swapChainImages[MAX_FRAMES_IN_FLIGHT];

ID3D12Resource* swapChainDepthImages[MAX_FRAMES_IN_FLIGHT];

ID3D12GraphicsCommandList7* graphicCommandBuffers[MAX_FRAMES_IN_FLIGHT];

ID3D12CommandAllocator* graphicCommandPools[MAX_FRAMES_IN_FLIGHT];

ID3D12CommandAllocator* transferCommandPool;
ID3D12GraphicsCommandList7* transferCommandBuffer;
int transferCommandsUploaded = 0;

ID3D12DescriptorHeap* globalRTVDescriptorHeap;
ID3D12DescriptorHeap* globalDSVDescriptorHeap;


UINT globalRTVDescriptorSize;
UINT globalDSVDescriptorSize;
UINT currentFrame;


ID3D12Fence* g_Fence;

uint64_t g_FenceValue = 0;

ID3D12Fence* t_Fence;

uint64_t t_FenceValue = 0;

HANDLE t_FenceEvent = INVALID_HANDLE_VALUE;

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

ShaderHandles shaderHandles[2]{};

ID3D12Resource* bgraImageMemoryPool;

struct TextureDetails
{
    DXGI_FORMAT type;
    uint32_t dataSize;
    uint32_t width, height, miplevels;
    char* data;
};


struct DriverMemoryBuffer
{
    ID3D12Resource* bufferHandle;
    size_t sizeOfAlloc;
    size_t currentPointer;
    D3D12_RESOURCE_STATES currentState;
    D3D12_RESOURCE_STATES initialState;
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
ID3D12GraphicsCommandList7* CreateCommandList(ID3D12Device2* device,
    ID3D12CommandAllocator* commandAllocator, D3D12_COMMAND_LIST_TYPE type);
ID3D12CommandAllocator* CreateCommandAllocator(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type);
int CreateRenderTargetView(ID3D12Device2* device, IDXGISwapChain4* swapChain, ID3D12DescriptorHeap* descriptorHeap, ID3D12Resource** outBuffers, UINT rtvDescriptorSize);
ID3D12DescriptorHeap* CreateDescriptorHeap(ID3D12Device2* device, D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize);
void ReleaseD3D12Resources();
int CreateDepthStencilView(ID3D12Device2* device, ID3D12DescriptorHeap* descriptorHeap, ID3D12Resource** outBuffers, UINT dsvDescriptorSize);
ID3DBlob* CreateShaderBlob(const char* shaderfile);
void CreateHostBuffer(DriverMemoryBuffer* dmb, UINT size, D3D12_RESOURCE_FLAGS flags);
ID3D12Resource* CreateDeviceLocalBuffer(ID3D12Device2* device, UINT size, D3D12_RESOURCE_FLAGS flags);
void WriteToHostMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies);
void WriteToDeviceLocalMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies);

ID3D12Resource* CreateImageResource(ID3D12Device2* device, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);
void WriteToImageDeviceLocalMemory(ID3D12Resource* imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers);
void TransitionBufferBarrier(ID3D12GraphicsCommandList7* cmdBuffer, ID3D12Resource* resource, D3D12_BARRIER_SYNC srcSync, D3D12_BARRIER_ACCESS srcAccess, D3D12_BARRIER_SYNC dstSync, D3D12_BARRIER_ACCESS dstAccess);
ID3D12RootSignature* CreateRootSignatureFromShaderGraph(ShaderGraph* graph);
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
    int heapsCount;
    ID3D12DescriptorHeap* descriptorHeap[8];
    int descriptorTableCount;
    int descriptorHeapPointer[8];
    int resourceCount[8];
    int descriptorHeapSelection[8]; //number of descriptor tables
    D3D_PRIMITIVE_TOPOLOGY topology;//D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST
    int instanceCount;
    
    ID3D12Resource* vertexBuffer;
    UINT vertexBufferOffset;
    UINT vertexBufferSize;
    int vertexSize;
    int vertexCount;
    ID3D12Resource* indexBuffer;
    UINT indexBufferOffset;
    UINT indexBufferSize;
    int indexSize;
    int indexCount;

};


PipelineObject triangles[2];

ID3D12PipelineState* CreatePipelineStateObject(ID3D12Device2* device, ID3D12RootSignature* _rootSignature, ShaderHandles* handles, int count);
ID3D12RootSignature* CreateRootSignature(ID3D12Device2* device, CD3DX12_ROOT_PARAMETER* rootParameters, UINT parameterCount, D3D12_ROOT_SIGNATURE_FLAGS flags);


size_t AllocFromDriverMemoryBuffer(DriverMemoryBuffer* dmb, size_t allocSize, size_t alignment)
{
    size_t current = dmb->currentPointer;
    size_t start = current;

    current = (current + alignment - 1) & ~(alignment - 1);

    dmb->currentPointer += (allocSize + (current - start));

    return current;
}

DriverMemoryBuffer hostBuffer{};
DriverMemoryBuffer deviceLocalBuffer{};

DriverMemoryBuffer stagingBuffers[MAX_FRAMES_IN_FLIGHT]{};

struct DescriptorHeap
{
    D3D12_DESCRIPTOR_HEAP_TYPE type;
    ID3D12DescriptorHeap* descriptorHeap;
    int descriptorHeapHandlePointer;
    int maxDescriptorHeapHandles;
    UINT descriptorHeapHandleSize;
};

DescriptorHeap mainSRVDescriptorHeap;
DescriptorHeap mainSamplerDescriptorHeap;


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

Allocation allocationHandle[50];

enum DescriptorType
{
    PUSHCONSTANTS = 0,
    CONSTANTBUFFER = 1,
    UNIFORMBUFFER = 2,
    SAMPLERSTATE = 3,
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
int CreateDescriptorTable(uintptr_t header, int descriptorCount, int frameCount, DescriptorHeap* heap);
void CreateSRVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT offset, UINT numCount, UINT size, DXGI_FORMAT format, DescriptorHeap* heap, D3D12_SRV_DIMENSION dimension);
void CreateImageSRVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT mipsLevels, DXGI_FORMAT format, DescriptorHeap* heap, D3D12_SRV_DIMENSION dimension);
void CreateUAVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT offset, UINT numCount, UINT size, DXGI_FORMAT format, DescriptorHeap* heap);
void CreateCBVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT offset, UINT size, DescriptorHeap* heap);
void CreateImageSampler(ID3D12Device2* device, DescriptorHeap* samplerDescriptorHeap);
void CreateTablesFromResourceSet(int* descriptorSets, int numDescriptorSet, PipelineObject* object);

static uint16_t BoxIndices[36] = {
        2,  1,  0,
        1,  2,  3,
        4,  5,  6,
        7,  6,  5,
        8,  9,  10,
        11, 10, 9,
       14, 13, 12,
       13, 14, 15,
       18, 17, 16,
       17, 18, 19,
       20, 21, 22,
       23, 22, 21
};

static XMVECTOR BoxVerts[48] =
{
    XMVectorSet(1.0f,  1.0f,  1.0f, 1.0f), XMVectorSet(0.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f,  1.0f, -1.0f, 1.0f), XMVectorSet(1.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f, -1.0f,  1.0f, 1.0f), XMVectorSet(0.0f, 1.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f, -1.0f, -1.0f, 1.0f), XMVectorSet(1.0f, 1.0f, 1.0f, 0.0f),

    XMVectorSet(-1.0f,  1.0f,  1.0f, 1.0f), XMVectorSet(0.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(-1.0f,  1.0f, -1.0f, 1.0f), XMVectorSet(1.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(-1.0f, -1.0f,  1.0f, 1.0f), XMVectorSet(0.0f, 1.0f, 1.0f, 0.0f),
    XMVectorSet(-1.0f, -1.0f, -1.0f, 1.0f), XMVectorSet(1.0f, 1.0f, 1.0f, 0.0f),

    XMVectorSet(-1.0f,  1.0f,  1.0f, 1.0f), XMVectorSet(0.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f,  1.0f,  1.0f, 1.0f), XMVectorSet(1.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(-1.0f,  1.0f, -1.0f, 1.0f), XMVectorSet(0.0f, 1.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f,  1.0f, -1.0f, 1.0f), XMVectorSet(1.0f, 1.0f, 1.0f, 0.0f),

    XMVectorSet(-1.0f, -1.0f,  1.0f, 1.0f), XMVectorSet(0.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f, -1.0f,  1.0f, 1.0f), XMVectorSet(1.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(-1.0f, -1.0f, -1.0f, 1.0f), XMVectorSet(0.0f, 1.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f, -1.0f, -1.0f, 1.0f), XMVectorSet(1.0f, 1.0f, 1.0f, 0.0f),

    XMVectorSet(-1.0f,  1.0f,  1.0f, 1.0f), XMVectorSet(0.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f,  1.0f,  1.0f, 1.0f), XMVectorSet(1.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(-1.0f, -1.0f,  1.0f, 1.0f), XMVectorSet(0.0f, 1.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f, -1.0f,  1.0f, 1.0f), XMVectorSet(1.0f, 1.0f, 1.0f, 0.0f),

    XMVectorSet(-1.0f,  1.0f, -1.0f, 1.0f), XMVectorSet(0.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f,  1.0f, -1.0f, 1.0f), XMVectorSet(1.0f, 0.0f, 1.0f, 0.0f),
    XMVectorSet(-1.0f, -1.0f, -1.0f, 1.0f), XMVectorSet(0.0f, 1.0f, 1.0f, 0.0f),
    XMVectorSet(1.0f, -1.0f, -1.0f, 1.0f), XMVectorSet(1.0f, 1.0f, 1.0f, 0.0f)
};


enum class ImageLayout
{
    UNDEFINED = 0,
    WRITEABLE = 1,
    SHADERREADABLE = 2,
    COLORATTACHMENT = 3,
    DEPTHSTENCILATTACHMENT = 4
};

enum class ImageUsage
{
    DEPTHSTENCIL = 0,
    COLOR = 1
};

enum BarrierActionBits
{
    WRITE_SHADER_RESOURCE = 1,
    READ_SHADER_RESOURCE = 2,
    READ_UNIFORM_BUFFER = 4,
    READ_VERTEX_INPUT = 8,
    READ_INDIRECT_COMMAND = 16
};

enum BarrierStageBits
{
    VERTEX_SHADER_BARRIER = 1,
    VERTEX_INPUT_BARRIER = 2,
    COMPUTE_BARRIER = 4,
    FRAGMENT_BARRIER = 8,
    BEGINNING_OF_PIPE = 16,
    INDIRECT_DRAW_BARRIER = 32,
};

typedef int BarrierAction;

typedef int BarrierStage;

enum class MemoryBarrierType
{
    MEMORY_BARRIER = 0,
    IMAGE_BARRIER = 1,
    BUFFER_BARRIER = 2,
    BARRIER_MAX_ENUM
};

struct ShaderResourceBarrier
{
    MemoryBarrierType type;
    BarrierStage srcStage;
    BarrierStage dstStage;
    BarrierAction srcAction;
    BarrierAction dstAction;
};

struct ImageShaderResourceBarrier : public ShaderResourceBarrier
{
    ImageLayout srcResourceLayout;
    ImageLayout dstResourceLayout;
    ImageUsage imageType;
};

struct ShaderResourceBufferBarrier : public ShaderResourceBarrier
{
};

struct ShaderResourceSampler : public ShaderResourceHeader
{
    void* samplerHandle;
};

struct ShaderResourceImage : public ShaderResourceHeader
{
    void* textureHandle;
};

struct ShaderResourceSamplerBindless : public ShaderResourceHeader
{
    void** textureHandles;
    int textureCount;
};

struct ShaderResourceBuffer : public ShaderResourceHeader
{
    int allocation;
    int offset;
};

struct ShaderResourceBufferView : public ShaderResourceHeader
{
    int subAllocations;
    int allocationIndex;
};


struct ShaderResourceConstantBuffer : public ShaderResourceHeader
{
    ShaderStageType stage;
    int size;
    int offset;
    void* data;
    int allocationIndex;
};






static char AllocateDSMemory[16 * KiB];
uintptr_t DSAllocator = 0;

std::array<uintptr_t, 50> descriptorSets;
static int DSIndex = 0;

constexpr std::array<int, 50> makeArray(int val) {
    std::array<int, 50> arr{};
    std::fill(arr.begin(), arr.end(), val);
    return arr;
}

std::array<int, 50> srvDescriptorTablesStart = makeArray(-1);

void BindBufferToShaderResource(int descriptorSet, int allocationIndex, int bindingIndex, int offset)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceBuffer* header = (ShaderResourceBuffer*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::UNIFORM_BUFFER && header->type != ShaderResourceType::STORAGE_BUFFER)
        return;

    header->allocation = allocationIndex;
    header->offset = offset;
}

void BindImageResourceToShaderResource(int descriptorSet, void* index, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceImage* header = (ShaderResourceImage*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::IMAGESTORE2D)
        return;

    header->textureHandle = index;
}

void BindSamplerResourceToShaderResource(int descriptorSet, void* index, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceSampler* header = (ShaderResourceSampler*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::SAMPLERSTATE)
        return;

    header->samplerHandle  = index;
}

void BindSampledImageToShaderResource(int descriptorSet, void* index, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceImage* header = (ShaderResourceImage*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::SAMPLER2D && header->type != ShaderResourceType::SAMPLERCUBE && header->type != ShaderResourceType::SAMPLER3D)
        return;

    header->textureHandle = index;
}

void BindSampledImageArrayToShaderResource(int descriptorSet, void** indices, uint32_t texCount, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceSamplerBindless* header = (ShaderResourceSamplerBindless*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::SAMPLERBINDLESS)
        return;

    header->textureHandles = indices;
    header->textureCount = texCount;
}

void BindBufferView(int descriptorSet, int allocationIndex, int bindingIndex, int subAllocations)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceBufferView* header = (ShaderResourceBufferView*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::BUFFER_VIEW)
        return;


    header->subAllocations = subAllocations;
    header->allocationIndex = allocationIndex;
}

void BindBarrier(int descriptorSet, int binding, BarrierStage stage, BarrierAction action)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));


    head = offsets[binding];
    ShaderResourceHeader* desc = (ShaderResourceHeader*)offsets[binding];



    switch (desc->type)
    {
    case ShaderResourceType::IMAGESTORE2D:
    case ShaderResourceType::SAMPLER2D:
    {
        head += sizeof(ShaderResourceImage);
        ShaderResourceBarrier* barrier = (ShaderResourceBarrier*)head;

        barrier->dstAction = action;
        barrier->dstStage = stage;
        break;
    }
    case ShaderResourceType::BUFFER_VIEW:
    {
        head += sizeof(ShaderResourceBufferView);
        ShaderResourceBufferBarrier* barrier = (ShaderResourceBufferBarrier*)head;
        barrier->dstStage = stage;
        barrier->dstAction = action;
        break;
    }
    case ShaderResourceType::STORAGE_BUFFER:
    case ShaderResourceType::UNIFORM_BUFFER:
    {

        head += sizeof(ShaderResourceBuffer);
        ShaderResourceBufferBarrier* barrier = (ShaderResourceBufferBarrier*)head;
        barrier->dstStage = stage;
        barrier->dstAction = action;
        break;
    }
    }


}

void BindImageBarrier(int descriptorSet, int binding, int barrierIndex, BarrierStage stage, BarrierAction action, ImageLayout oldLayout, ImageLayout dstLayout, bool location)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));


    head = offsets[binding];
    ShaderResourceHeader* desc = (ShaderResourceHeader*)offsets[binding];

    if (desc->type != ShaderResourceType::SAMPLER2D && desc->type != ShaderResourceType::SAMPLERCUBE && desc->type != ShaderResourceType::SAMPLER3D && desc->type != ShaderResourceType::IMAGESTORE2D)
        return;

    switch (desc->type)
    {
    case ShaderResourceType::IMAGESTORE2D:
    case ShaderResourceType::SAMPLER2D:
        head += sizeof(ShaderResourceImage);

        break;
    case ShaderResourceType::STORAGE_BUFFER:
    case ShaderResourceType::UNIFORM_BUFFER:

        head += sizeof(ShaderResourceBuffer);
        break;
    }


    ImageShaderResourceBarrier* imageBarrier = (ImageShaderResourceBarrier*)head;


    imageBarrier[barrierIndex].imageType = ImageUsage::COLOR;
    imageBarrier[barrierIndex].dstResourceLayout = dstLayout;
    imageBarrier[barrierIndex].srcResourceLayout = oldLayout;

    if (location)
    {
        imageBarrier[barrierIndex].dstAction = action;
        imageBarrier[barrierIndex].dstStage = stage;
    }
    else {
        imageBarrier[barrierIndex].srcAction = action;
        imageBarrier[barrierIndex].srcStage = stage;
    }
}

ShaderResourceHeader* GetConstantBuffer(int descriptorSet, int constantBuffer)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceHeader* ret = (ShaderResourceHeader*)(offsets[set->bindingCount - (constantBuffer + 1)]);

    if (ret->type != ShaderResourceType::CONSTANT_BUFFER) return nullptr;

    return ret;
}

int GetConstantBufferCount(int descriptorSet)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    int iter = set->bindingCount - 1;

    int count = 0;

    while (iter >= 0)
    {
        ShaderResourceHeader* ret = (ShaderResourceHeader*)(offsets[iter--]);
        if (ret->type == ShaderResourceType::CONSTANT_BUFFER) count++;
        else break;
    }

    return count;
}

int GetBarrierCount(int descriptorSet)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    return set->barrierCount;
}

void UploadConstant(int descriptorset, void* data, int bufferLocation)
{
    ShaderResourceConstantBuffer* header = (ShaderResourceConstantBuffer*)GetConstantBuffer(descriptorset, bufferLocation);
    if (!header) return;
    header->data = data;
}

void UploadConstant(int descriptorset, int allocationIndex, int bufferLocation)
{
    ShaderResourceConstantBuffer* header = (ShaderResourceConstantBuffer*)GetConstantBuffer(descriptorset, bufferLocation);
    if (!header) return;
    header->allocationIndex = allocationIndex;
}

int AllocateShaderResourceSet(ShaderGraph* graph, uint32_t targetSet, int setCount)
{
    uintptr_t head = (uintptr_t)AllocateDSMemory + DSAllocator;

    uintptr_t ptr = head;
    ShaderResourceSet* set = (ShaderResourceSet*)ptr;
    ptr += sizeof(ShaderResourceSet);

    ShaderSetLayout* resourceSet = (ShaderSetLayout*)graph->GetSet(targetSet);

    set->bindingCount = resourceSet->bindingCount;
    set->layoutHandle = resourceSet->vulkanDescLayout;
    set->setCount = setCount;
    set->barrierCount = 0;

    uintptr_t* offset = (uintptr_t*)ptr;

    ptr += sizeof(uintptr_t) * (set->bindingCount);


    int constantCount = set->bindingCount;
    for (int h = 0; h < set->bindingCount; h++)
    {
        MemoryBarrierType memBarrierType = MemoryBarrierType::MEMORY_BARRIER;

        ShaderResource* resource = (ShaderResource*)graph->GetResource(resourceSet->resourceStart + h);

        if (resource->set != targetSet) continue;

        ShaderResourceHeader* desc = (ShaderResourceHeader*)ptr;

        if (resource->binding != ~0)
            desc->binding = resource->binding;
        else
            desc->binding = --constantCount;

        desc->type = resource->type;
        desc->action = resource->action;
        desc->arrayCount = resource->arrayCount;

        offset[desc->binding] = ptr;

        switch (resource->type)
        {
        case ShaderResourceType::SAMPLERSTATE:
        {

            ptr += sizeof(ShaderResourceSampler);
            break;
        }
        case ShaderResourceType::IMAGESTORE2D:
        {
            ptr += sizeof(ShaderResourceImage);
            memBarrierType = MemoryBarrierType::IMAGE_BARRIER;
            if (resource->action == ShaderResourceAction::SHADERWRITE || resource->action == ShaderResourceAction::SHADERREADWRITE)
            {
                /*
                ImageShaderResourceBarrier* barriers = (ImageShaderResourceBarrier*)ptr;
                barriers->dstStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers->dstAction = WRITE_SHADER_RESOURCE;
                barriers->type = memBarrierType;

                barriers[1].srcStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers[1].srcAction = WRITE_SHADER_RESOURCE;
                barriers[1].type = memBarrierType;

                ptr += (sizeof(ImageShaderResourceBarrier) * 2);
                set->barrierCount += 2;
                */
            }
            break;
        }
        case ShaderResourceType::SAMPLER3D:
        case ShaderResourceType::SAMPLER2D:
        case ShaderResourceType::SAMPLERCUBE:
        {
            ptr += sizeof(ShaderResourceImage);
            memBarrierType = MemoryBarrierType::IMAGE_BARRIER;
            if (resource->action == ShaderResourceAction::SHADERWRITE || resource->action == ShaderResourceAction::SHADERREADWRITE)
            {
                /*
                ImageShaderResourceBarrier* barriers = (ImageShaderResourceBarrier*)ptr;
                barriers->srcStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers->srcAction = WRITE_SHADER_RESOURCE;
                barriers->type = memBarrierType;
                ptr += (sizeof(ImageShaderResourceBarrier));
                set->barrierCount++;
                */
            }
            break;
        }
        case ShaderResourceType::SAMPLERBINDLESS:
        {
            memBarrierType = MemoryBarrierType::IMAGE_BARRIER;
            memset((void*)ptr, 0, sizeof(ShaderResourceSamplerBindless));
            ptr += sizeof(ShaderResourceSamplerBindless);
            break;
        }
        case ShaderResourceType::CONSTANT_BUFFER:
        {
            ShaderResourceConstantBuffer* constants = (ShaderResourceConstantBuffer*)ptr;
            constants->size = resource->size;
            constants->offset = resource->offset;
            constants->stage = resource->stages;
            constants->data = NULL;
            constants->allocationIndex = -1;
            ptr += sizeof(ShaderResourceConstantBuffer);
            break;
        }
        case ShaderResourceType::STORAGE_BUFFER:
        case ShaderResourceType::UNIFORM_BUFFER:
        {
            memBarrierType = MemoryBarrierType::BUFFER_BARRIER;
            ptr += sizeof(ShaderResourceBuffer);
            if (resource->action == ShaderResourceAction::SHADERWRITE || resource->action == ShaderResourceAction::SHADERREADWRITE)
            {
                /*
                ShaderResourceBarrier* barriers = (ShaderResourceBarrier*)ptr;
                barriers->srcStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers->srcAction = WRITE_SHADER_RESOURCE;
                barriers->type = memBarrierType;
                ptr += (sizeof(ShaderResourceBufferBarrier));
                set->barrierCount++;
                */
            }
            break;
        }
        case ShaderResourceType::BUFFER_VIEW:
        {
            memBarrierType = MemoryBarrierType::BUFFER_BARRIER;
            ptr += sizeof(ShaderResourceBufferView);
            if (resource->action == ShaderResourceAction::SHADERWRITE || resource->action == ShaderResourceAction::SHADERREADWRITE)
            {
                /*
                ShaderResourceBarrier* barriers = (ShaderResourceBarrier*)ptr;
                barriers->srcStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers->srcAction = WRITE_SHADER_RESOURCE;
                barriers->type = memBarrierType;
                ptr += (sizeof(ShaderResourceBufferBarrier));
                set->barrierCount++;
                (*/
            }
            break;
        }
        }


    }

    DSAllocator += ptr - head;


    int ret = DSIndex++;

    descriptorSets[ret] = head;

    return ret;
}





int AllocFromHostBuffer(size_t size, size_t alignment, int copies)
{
 
    size_t allocSize = (size + alignment - 1) & ~((size_t)alignment - 1);

    allocSize *= copies;

    size_t location = AllocFromDriverMemoryBuffer(&hostBuffer, allocSize, alignment);

    int index = allocationHandleIndex++;

    allocationHandle[index].bufferHandle = hostBuffer.bufferHandle;
    allocationHandle[index].offset = location;
    allocationHandle[index].totalDeviceAlloc = allocSize;
    allocationHandle[index].requestedsize = size;
    allocationHandle[index].alignment = alignment;
    allocationHandle[index].copies = copies;
    allocationHandle[index].stridesize = (size + alignment - 1) & ~(alignment - 1);

    return index;
}

int AllocFromDeviceBuffer(size_t size, size_t alignment, int copies)
{

    size_t allocSize = (size + alignment - 1) & ~((size_t)alignment - 1);

    allocSize *= copies;

    size_t location = AllocFromDriverMemoryBuffer(&deviceLocalBuffer, allocSize, alignment);

    int index = allocationHandleIndex++;

    allocationHandle[index].bufferHandle = deviceLocalBuffer.bufferHandle;
    allocationHandle[index].offset = location;
    allocationHandle[index].totalDeviceAlloc = allocSize;
    allocationHandle[index].requestedsize = size;
    allocationHandle[index].alignment = alignment;
    allocationHandle[index].copies = copies;
    allocationHandle[index].stridesize = (size + alignment - 1) & ~(alignment - 1);

    return index;
}

void DoSceneStuff()
{

    ShaderGraph* mainLayout = ShaderGraphReader::CreateShaderGraph("DirectLayout.xml");

    

    triangles[0].rootSignature = CreateRootSignatureFromShaderGraph(mainLayout);//CreateGenericRootSignature();

    triangles[0].pipelineState = CreatePipelineStateObject(deviceHandle, triangles[0].rootSignature, shaderHandles, 2);


    TextureDetails details{};

    ParseBMP(&details, "face1.bmp");

    int cameraData = AllocFromHostBuffer(sizeof(Camera), sizeof(Camera), MAX_FRAMES_IN_FLIGHT);
    int world1Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 256, MAX_FRAMES_IN_FLIGHT);
    int world2Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 256, MAX_FRAMES_IN_FLIGHT);
    int vertexOffset = AllocFromDeviceBuffer(sizeof(BoxVerts), 16, 1);
    int indexOffset = AllocFromDeviceBuffer(sizeof(BoxIndices), 16, 1);

    TransitionBufferBarrier(transferCommandBuffer, deviceLocalBuffer.bufferHandle, D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS, D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST);

    WriteToDeviceLocalMemory(vertexOffset, BoxVerts, sizeof(BoxVerts), 0, 1);

    WriteToDeviceLocalMemory(indexOffset, BoxIndices, sizeof(BoxIndices), 0, 1);

    WriteToHostMemory(cameraData, &cam, sizeof(Camera), 0, MAX_FRAMES_IN_FLIGHT);

    WriteToDeviceLocalMemory(world1Data, world, 64, 0, MAX_FRAMES_IN_FLIGHT);

    WriteToDeviceLocalMemory(world2Data, &world[1], 64, 0, MAX_FRAMES_IN_FLIGHT);

    TransitionBufferBarrier(transferCommandBuffer, deviceLocalBuffer.bufferHandle, D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST, D3D12_BARRIER_SYNC_DRAW | D3D12_BARRIER_SYNC_INDEX_INPUT, D3D12_BARRIER_ACCESS_VERTEX_BUFFER | D3D12_BARRIER_ACCESS_CONSTANT_BUFFER | D3D12_BARRIER_ACCESS_INDEX_BUFFER);

    bgraImageMemoryPool = CreateImageResource(deviceHandle, details.width, details.height, 1, details.miplevels, D3D12_RESOURCE_FLAG_NONE, details.type, D3D12_RESOURCE_DIMENSION_TEXTURE2D);

    WriteToImageDeviceLocalMemory(bgraImageMemoryPool, details.data, details.width, details.height, 4, details.dataSize, details.type, details.miplevels, 1);

    int camSRVDS = AllocateShaderResourceSet(mainLayout, 0, MAX_FRAMES_IN_FLIGHT);

    BindBufferToShaderResource(camSRVDS, cameraData, 0, 0);
    BindImageResourceToShaderResource(camSRVDS, bgraImageMemoryPool, 1);
    BindSamplerResourceToShaderResource(camSRVDS, bgraImageMemoryPool, 2);

    int worldOne = AllocateShaderResourceSet(mainLayout, 1, MAX_FRAMES_IN_FLIGHT);
    UploadConstant(worldOne, world1Data, 0);
    
    int worldTwo = AllocateShaderResourceSet(mainLayout, 1, MAX_FRAMES_IN_FLIGHT);
    UploadConstant(worldTwo, world2Data, 0);

    int basic1[2] = { camSRVDS, worldOne };
    int basic2[2] = { camSRVDS, worldTwo };

   

    CreateTablesFromResourceSet(basic1, 2, &triangles[0]);
    CreateTablesFromResourceSet(basic2, 2, &triangles[1]);
    
    triangles[0].topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
    triangles[0].heapsCount = 2;
    triangles[0].instanceCount = 1;
    triangles[0].vertexCount = 24;
    triangles[0].indexCount = 36;
    triangles[0].descriptorHeap[0] = mainSRVDescriptorHeap.descriptorHeap;
    triangles[0].descriptorHeap[1] = mainSamplerDescriptorHeap.descriptorHeap;

    triangles[0].descriptorTableCount = 3;

    triangles[1].topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
    triangles[1].heapsCount = 2;
    triangles[1].instanceCount = 1;
    triangles[1].vertexCount = 24;
    triangles[1].indexCount = 36;
    triangles[1].descriptorHeap[0] = mainSRVDescriptorHeap.descriptorHeap;
    triangles[1].descriptorHeap[1] = mainSamplerDescriptorHeap.descriptorHeap;

    triangles[1].pipelineState = triangles[0].pipelineState;
    triangles[1].rootSignature = triangles[0].rootSignature;

    triangles[1].descriptorTableCount = 3;

    triangles[0].indexBuffer = deviceLocalBuffer.bufferHandle;
    triangles[1].indexBuffer = deviceLocalBuffer.bufferHandle;
    triangles[0].vertexBuffer = deviceLocalBuffer.bufferHandle;
    triangles[1].vertexBuffer = deviceLocalBuffer.bufferHandle;
    triangles[0].vertexBufferOffset = allocationHandle[vertexOffset].offset;
    triangles[1].vertexBufferOffset = allocationHandle[vertexOffset].offset;
    triangles[0].indexBufferOffset = allocationHandle[indexOffset].offset;
    triangles[1].indexBufferOffset = allocationHandle[indexOffset].offset;
    triangles[0].vertexSize = sizeof(XMVECTOR)*2;
    triangles[1].vertexSize = sizeof(XMVECTOR)*2;
    triangles[0].indexSize = 2;
    triangles[1].indexSize = 2;
    triangles[0].indexBufferSize = sizeof(BoxIndices);
    triangles[1].indexBufferSize = sizeof(BoxIndices);
    triangles[0].vertexBufferSize = sizeof(BoxVerts);
    triangles[1].vertexBufferSize = sizeof(BoxVerts);


}

int main()
{
    cam.view = XMMatrixLookAtRH(XMVectorSet(0.0f, 0.0f, 5.0f, 0.0f), XMVectorSet(0.0f, 0.0f, 0.0f, 0.0f), XMVectorSet(0.0f, 1.0f, 0.0f, 0.0f));
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

    CreateDescriptorHeapManager(&mainSamplerDescriptorHeap, MAX_FRAMES_IN_FLIGHT * 100, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER, D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE);

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


    t_Fence = CreateFence(deviceHandle);
    if (!t_Fence)
        goto end;

    t_FenceEvent = CreateEventHandle();
    if (t_FenceEvent == INVALID_HANDLE_VALUE)
        goto end;

    transferCommandBuffer = CreateCommandList(
        deviceHandle,
        transferCommandPool,
        D3D12_COMMAND_LIST_TYPE_DIRECT
    );


    world[0] = XMMatrixIdentity();
    world[1] = XMMatrixTranslation(2.0, 2.0, 0.0);

    shaderHandles[0] = { VERTEX, CreateShaderBlob("VS.bin") };
    shaderHandles[1] = { PIXEL, CreateShaderBlob("PS.bin") };

    

    CreateHostBuffer(&hostBuffer, 4096, D3D12_RESOURCE_FLAG_NONE);

    deviceLocalBuffer.bufferHandle = CreateDeviceLocalBuffer(deviceHandle, 4096, D3D12_RESOURCE_FLAG_NONE);

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        CreateHostBuffer(&stagingBuffers[i], 64'000'000, D3D12_RESOURCE_FLAG_NONE);
    }


    transferCommandPool->Reset();

    transferCommandBuffer->Reset(transferCommandPool, NULL);

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

    D3D12_INPUT_ELEMENT_DESC inputLayout[2]{};

    VertexInputDescription inputDesc[2] = {
        {
            ComponentFormatType::R32G32B32A32_SFLOAT,
            0, 
            VertexUsage::POSITION
        },
        {
            ComponentFormatType::R32G32B32A32_SFLOAT,
            sizeof(XMVECTOR),
            VertexUsage::TEX0
        }
    };

    for (int i = 0; i < 2; i++)
    {
        ConvertVertexLayoutToD3D12InputDesc(&inputDesc[i], &inputLayout[i], 0);
    }

    desc.InputLayout = { inputLayout, 2 };
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
    desc.RasterizerState.CullMode = D3D12_CULL_MODE_FRONT;
    desc.RasterizerState.FrontCounterClockwise = FALSE;

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

    if (transferCommandBuffer)
        transferCommandBuffer->Release();

    if (triangles[0].rootSignature)
        triangles[0].rootSignature->Release();

    if (triangles[0].pipelineState)
        triangles[0].pipelineState->Release();

    if (hostBuffer.bufferHandle)
        hostBuffer.bufferHandle->Release();

    if (deviceLocalBuffer.bufferHandle)
        deviceLocalBuffer.bufferHandle->Release();

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        if (stagingBuffers[i].bufferHandle)
            stagingBuffers[i].bufferHandle->Release();
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

    if (mainSamplerDescriptorHeap.descriptorHeap)
    {
        mainSamplerDescriptorHeap.descriptorHeap->Release();
        mainSamplerDescriptorHeap.descriptorHeap = nullptr;
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

    // --- Fence ---
    if (t_Fence)
    {
        t_Fence->Release();
        t_Fence = nullptr;
    }

    // --- Fence event ---
    if (t_FenceEvent != INVALID_HANDLE_VALUE)
    {
        CloseHandle(t_FenceEvent);
        t_FenceEvent = INVALID_HANDLE_VALUE;
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

            
            if (FAILED(infoQueue->PushStorageFilter(&NewFilter))) {
                printf("Cannot set debug levels when requested");
            }
            
            infoQueue->Release();
        }
    }

    D3D12_FEATURE_DATA_D3D12_OPTIONS12 opts12{};
    d3d12Device2->CheckFeatureSupport(
        D3D12_FEATURE_D3D12_OPTIONS12,
        &opts12,
        sizeof(opts12)
    );

    assert(opts12.EnhancedBarriersSupported);
    

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



ID3D12GraphicsCommandList7* CreateCommandList(ID3D12Device2* device,
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


    ID3D12GraphicsCommandList7* commandList7 = NULL;

    if (FAILED(commandList->QueryInterface(IID_PPV_ARGS(&commandList7))))
    {
        printf("Failed to make newest interface\n");
        commandList->Release();
        return NULL;
    }


    return commandList7;
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

    if (transferCommandsUploaded)
    {
        transferCommandBuffer->Close();

        ID3D12CommandList* const commandLists[] = {
            transferCommandBuffer
        };

        queueHandle->ExecuteCommandLists(_countof(commandLists), commandLists);

        uint64_t fenceValue = 0;

        uint64_t fencesign = Signal(queueHandle, t_Fence, fenceValue);

        Flush(queueHandle, t_Fence, fenceValue, t_FenceEvent);

        transferCommandPool->Reset();

        transferCommandBuffer->Reset(transferCommandPool, NULL);

        transferCommandsUploaded = 0;
    }

    commandAllocator->Reset();

    graphicCommandBuffer->Reset(commandAllocator, nullptr);

    CD3DX12_CPU_DESCRIPTOR_HANDLE dsvHandle(globalDSVDescriptorHeap->GetCPUDescriptorHandleForHeapStart(), currentFrame, globalDSVDescriptorSize);

    CD3DX12_CPU_DESCRIPTOR_HANDLE rtvHandle(globalRTVDescriptorHeap->GetCPUDescriptorHandleForHeapStart(), currentFrame, globalRTVDescriptorSize);

    
    {

        D3D12_TEXTURE_BARRIER barrierInfo{};
        barrierInfo.Flags = D3D12_TEXTURE_BARRIER_FLAG_NONE;
        barrierInfo.pResource = backBuffer;
        barrierInfo.Subresources.FirstArraySlice = 0;
        barrierInfo.Subresources.IndexOrFirstMipLevel = 0;
        barrierInfo.Subresources.FirstPlane = 0;
        barrierInfo.Subresources.NumArraySlices = 1;
        barrierInfo.Subresources.NumMipLevels = 1;
        barrierInfo.Subresources.NumPlanes = 1;
        barrierInfo.SyncBefore = D3D12_BARRIER_SYNC_NONE;
        barrierInfo.SyncAfter = D3D12_BARRIER_SYNC_RENDER_TARGET;
        barrierInfo.AccessBefore = D3D12_BARRIER_ACCESS_NO_ACCESS;
            
        barrierInfo.AccessAfter = D3D12_BARRIER_ACCESS_RENDER_TARGET;
        barrierInfo.LayoutBefore = D3D12_BARRIER_LAYOUT_PRESENT;
        barrierInfo.LayoutAfter = D3D12_BARRIER_LAYOUT_RENDER_TARGET;

     
        D3D12_BARRIER_GROUP barrierGroup = {};
        barrierGroup.Type = D3D12_BARRIER_TYPE_TEXTURE;
        barrierGroup.NumBarriers = 1;
        barrierGroup.pTextureBarriers = &barrierInfo;

        graphicCommandBuffer->Barrier(1, &barrierGroup);
        const FLOAT clearColor[4] = {0.0f, 0.0f, 0.0f, 1.0f};
    }

    


    D3D12_VIEWPORT viewport{};
    viewport.Width = 800.0f;
    viewport.Height = 600.0f;
    viewport.MinDepth = 0.0f;
    viewport.MaxDepth = 1.0f;

    D3D12_RECT scissor{};
    scissor.right = 800;
    scissor.bottom = 600;

   

    D3D12_RENDER_PASS_DEPTH_STENCIL_DESC depthDesc{};
    D3D12_RENDER_PASS_RENDER_TARGET_DESC rtvDesc{};

    const FLOAT clearColor[4] = { 0.0f, 0.0f, 0.0f, 1.0f };


    rtvDesc.BeginningAccess.Type = D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_CLEAR;
    memcpy(rtvDesc.BeginningAccess.Clear.ClearValue.Color, clearColor, sizeof(FLOAT) * 4);
    rtvDesc.BeginningAccess.Clear.ClearValue.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    rtvDesc.cpuDescriptor = rtvHandle;
    rtvDesc.EndingAccess.Type = D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_PRESERVE;
 /*   rtvDesc.EndingAccess.Resolve.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    rtvDesc.EndingAccess.Resolve.PreserveResolveSource = FALSE;
    rtvDesc.EndingAccess.Resolve.pSrcResource = backBuffer;
    rtvDesc.EndingAccess.Resolve.pDstResource = NULL;
    rtvDesc.EndingAccess.Resolve.SubresourceCount = 0;
    rtvDesc.EndingAccess.Resolve.ResolveMode = D3D12_RESOLVE_MODE_AVERAGE;
    rtvDesc.EndingAccess.Resolve.pSubresourceParameters = NULL; */

    depthDesc.cpuDescriptor = dsvHandle;
    depthDesc.DepthBeginningAccess.Type = D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_CLEAR;
    depthDesc.DepthBeginningAccess.Clear.ClearValue.DepthStencil.Depth = 1.0f;
    depthDesc.DepthBeginningAccess.Clear.ClearValue.DepthStencil.Stencil = 0;
    depthDesc.DepthEndingAccess.Type = D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_DISCARD;
    depthDesc.StencilBeginningAccess.Type = D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_NO_ACCESS;
    depthDesc.StencilEndingAccess.Type = D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_NO_ACCESS;


    graphicCommandBuffer->BeginRenderPass(1, &rtvDesc, &depthDesc, D3D12_RENDER_PASS_FLAG_NONE);


    graphicCommandBuffer->RSSetViewports(1, &viewport);
    graphicCommandBuffer->RSSetScissorRects(1, &scissor);

    for (int i = 0; i < 2; i++)
    {
        graphicCommandBuffer->SetGraphicsRootSignature(triangles[i].rootSignature);

        graphicCommandBuffer->SetDescriptorHeaps(triangles[i].heapsCount, triangles[i].descriptorHeap);

        graphicCommandBuffer->SetPipelineState(triangles[i].pipelineState);
        graphicCommandBuffer->IASetPrimitiveTopology(triangles[i].topology);




        for (int j = 0; j < triangles[i].descriptorTableCount; j++)
        {
            int heapindex = triangles[i].descriptorHeapSelection[j];
            int step = (heapindex == 0 ? mainSRVDescriptorHeap.descriptorHeapHandleSize : mainSamplerDescriptorHeap.descriptorHeapHandleSize);
            CD3DX12_GPU_DESCRIPTOR_HANDLE handle = CD3DX12_GPU_DESCRIPTOR_HANDLE(triangles[i].descriptorHeap[heapindex]->GetGPUDescriptorHandleForHeapStart(), triangles[i].descriptorHeapPointer[j] + (currentFrame * triangles[i].resourceCount[j]), step);
            graphicCommandBuffer->SetGraphicsRootDescriptorTable(j, handle);
        }


        if (triangles[i].vertexBuffer != NULL)
        {
            D3D12_VERTEX_BUFFER_VIEW vertexView{};

            vertexView.BufferLocation = triangles[i].vertexBuffer->GetGPUVirtualAddress() + triangles[i].vertexBufferOffset;
            vertexView.SizeInBytes = triangles[i].vertexBufferSize;
            vertexView.StrideInBytes = triangles[i].vertexSize;

            graphicCommandBuffer->IASetVertexBuffers(0, 1, &vertexView);
        }

        if (triangles[i].indexBuffer != NULL)
        {
        
            D3D12_INDEX_BUFFER_VIEW indexView{};

            indexView.BufferLocation = triangles[i].indexBuffer->GetGPUVirtualAddress() + triangles[i].indexBufferOffset;
            indexView.SizeInBytes = triangles[i].indexBufferSize;
            indexView.Format = (triangles[i].indexSize == 2) ? DXGI_FORMAT_R16_UINT : DXGI_FORMAT_R32_UINT;

            graphicCommandBuffer->IASetIndexBuffer(&indexView);

            graphicCommandBuffer->DrawIndexedInstanced(triangles[i].indexCount, triangles[i].instanceCount, 0, 0, 0);

        }
        else 
        {
            graphicCommandBuffer->DrawInstanced(triangles[i].vertexCount, triangles[i].instanceCount, 0, 0);
        }

        

        //graphicCommandBuffer->SetGraphicsRoot32BitConstants(0, 32, &cam, 0);
        
    }

    graphicCommandBuffer->EndRenderPass();

    {
      

            D3D12_TEXTURE_BARRIER barrierInfo{};
            barrierInfo.Flags = D3D12_TEXTURE_BARRIER_FLAG_NONE;
            barrierInfo.pResource = backBuffer;
            barrierInfo.Subresources.FirstArraySlice = 0;
            barrierInfo.Subresources.IndexOrFirstMipLevel = 0;
            barrierInfo.Subresources.FirstPlane = 0;
            barrierInfo.Subresources.NumArraySlices = 1;
            barrierInfo.Subresources.NumMipLevels = 1;
            barrierInfo.Subresources.NumPlanes = 1;
            barrierInfo.SyncBefore = D3D12_BARRIER_SYNC_RENDER_TARGET;
            barrierInfo.SyncAfter = D3D12_BARRIER_SYNC_NONE;
            barrierInfo.AccessBefore = D3D12_BARRIER_ACCESS_RENDER_TARGET;
            barrierInfo.AccessAfter = D3D12_BARRIER_ACCESS_NO_ACCESS;
            barrierInfo.LayoutBefore =  D3D12_BARRIER_LAYOUT_RENDER_TARGET;
            barrierInfo.LayoutAfter = D3D12_BARRIER_LAYOUT_PRESENT;

    
            D3D12_BARRIER_GROUP barrierGroup = {};
            barrierGroup.Type = D3D12_BARRIER_TYPE_TEXTURE;
            barrierGroup.NumBarriers = 1;
            barrierGroup.pTextureBarriers = &barrierInfo;

            graphicCommandBuffer->Barrier(1, &barrierGroup);

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


void CreateHostBuffer(DriverMemoryBuffer* dmb, UINT size, D3D12_RESOURCE_FLAGS flags)
{
    dmb->sizeOfAlloc = size;
    dmb->currentPointer = 0;

    //create the committed resource
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
    deviceHandle->CreateCommittedResource(
        &heapProps,
        D3D12_HEAP_FLAG_NONE,
        &bufferDesc,
        D3D12_RESOURCE_STATE_GENERIC_READ, // initial state
        nullptr,
        IID_PPV_ARGS(&buffer)
    );


    dmb->currentState = dmb->initialState = D3D12_RESOURCE_STATE_GENERIC_READ;
 
    dmb->bufferHandle = buffer;

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

void TransitionBufferBarrier(ID3D12GraphicsCommandList7* cmdBuffer, ID3D12Resource* resource, D3D12_BARRIER_SYNC srcSync, D3D12_BARRIER_ACCESS srcAccess,  D3D12_BARRIER_SYNC dstSync, D3D12_BARRIER_ACCESS dstAccess)
{
    D3D12_BUFFER_BARRIER barrierInfo{};
    barrierInfo.pResource = resource;
    barrierInfo.SyncBefore = srcSync;
    barrierInfo.SyncAfter = dstSync;
    barrierInfo.AccessBefore = srcAccess;
    barrierInfo.AccessAfter = dstAccess;
    barrierInfo.Offset = 0;
    barrierInfo.Size = UINT64_MAX;

    D3D12_BARRIER_GROUP barrierGroup = {};
    barrierGroup.Type = D3D12_BARRIER_TYPE_BUFFER;
    barrierGroup.NumBarriers = 1;
    barrierGroup.pBufferBarriers = &barrierInfo;

    cmdBuffer->Barrier(1, &barrierGroup);
}

void WriteToDeviceLocalMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies)
{
	void* mappedData = nullptr;

	Allocation* alloc = &allocationHandle[allocationIndex];

    size_t stride = (alloc->requestedsize + alloc->alignment - 1) & ~(alloc->alignment - 1);

    size_t allocLoc = AllocFromDriverMemoryBuffer(&stagingBuffers[currentFrame], stride*copies, alloc->alignment);

    ID3D12Resource* stagingBuffer = stagingBuffers[currentFrame].bufferHandle;

	stagingBuffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData)+allocLoc;

	for (int i = 0; i < copies; i++)
	{
		memcpy((void*)(cdata), data, size);
		cdata += stride;
	}
	stagingBuffer->Unmap(0, nullptr);

	transferCommandBuffer->CopyBufferRegion(alloc->bufferHandle, alloc->offset + offset, stagingBuffer, allocLoc, stride * copies);

	transferCommandsUploaded++;
}

void WriteToImageDeviceLocalMemory(ID3D12Resource* imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers)
{
    void* mappedData = nullptr;

    size_t stride = ((width * componentCount) + (255)) & ~255;

    size_t allocLoc = AllocFromDriverMemoryBuffer(&stagingBuffers[currentFrame], stride*height, 255);

    ID3D12Resource* stagingBuffer = stagingBuffers[currentFrame].bufferHandle;

    stagingBuffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData)+allocLoc;

    for (int i = 0; i < height; i++)
    {
        memcpy((void*)(cdata), data + (i * width * componentCount), width * componentCount);
        cdata += stride;
    }


    stagingBuffer->Unmap(0, nullptr);
    
    D3D12_RESOURCE_BARRIER preBarrier = {};
    preBarrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    preBarrier.Transition.pResource = imageHandle;
    preBarrier.Transition.StateBefore = D3D12_RESOURCE_STATE_COMMON;
    preBarrier.Transition.StateAfter = D3D12_RESOURCE_STATE_COPY_DEST;
    preBarrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;


    D3D12_TEXTURE_BARRIER barrierInfo{};
    barrierInfo.Flags = D3D12_TEXTURE_BARRIER_FLAG_NONE;
    barrierInfo.pResource = imageHandle;
    barrierInfo.Subresources.FirstArraySlice = 0;
    barrierInfo.Subresources.IndexOrFirstMipLevel = 0;
    barrierInfo.Subresources.FirstPlane = 0;
    barrierInfo.Subresources.NumArraySlices = layers;
    barrierInfo.Subresources.NumMipLevels = mipLevels;
    barrierInfo.Subresources.NumPlanes = 1;
    barrierInfo.SyncBefore = D3D12_BARRIER_SYNC_NONE;
    barrierInfo.SyncAfter = D3D12_BARRIER_SYNC_COPY;
    barrierInfo.AccessBefore = D3D12_BARRIER_ACCESS_NO_ACCESS;
    barrierInfo.AccessAfter = D3D12_BARRIER_ACCESS_COPY_DEST;
    barrierInfo.LayoutBefore = D3D12_BARRIER_LAYOUT_COMMON;
    barrierInfo.LayoutAfter = D3D12_BARRIER_LAYOUT_COPY_DEST;


    D3D12_BARRIER_GROUP barrierGroup = {};
    barrierGroup.Type = D3D12_BARRIER_TYPE_TEXTURE;
    barrierGroup.NumBarriers = 1;
    barrierGroup.pTextureBarriers = &barrierInfo;

    transferCommandBuffer->Barrier(1, &barrierGroup);

    D3D12_TEXTURE_COPY_LOCATION dest{}, src{};

    src.pResource = stagingBuffer;
    src.Type = D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT;
    src.PlacedFootprint.Offset = allocLoc;
    src.PlacedFootprint.Footprint.Depth = 1;
    src.PlacedFootprint.Footprint.Width = width;
    src.PlacedFootprint.Footprint.Height = height;
    src.PlacedFootprint.Footprint.Format = format;
    src.PlacedFootprint.Footprint.RowPitch = stride;


    dest.pResource = imageHandle;
    dest.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
    dest.SubresourceIndex = 0;

    transferCommandBuffer->CopyTextureRegion(&dest, 0, 0, 0, &src, NULL);

    D3D12_RESOURCE_BARRIER postBarrier = {};
    postBarrier.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
    postBarrier.Transition.pResource = imageHandle;
    postBarrier.Transition.StateBefore = D3D12_RESOURCE_STATE_COPY_DEST;
    postBarrier.Transition.StateAfter = D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE;
    postBarrier.Transition.Subresource = D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES;

    barrierInfo.Flags = D3D12_TEXTURE_BARRIER_FLAG_NONE;
    barrierInfo.pResource = imageHandle;
    barrierInfo.Subresources.FirstArraySlice = 0;
    barrierInfo.Subresources.IndexOrFirstMipLevel = 0;
    barrierInfo.Subresources.FirstPlane = 0;
    barrierInfo.Subresources.NumArraySlices = layers;
    barrierInfo.Subresources.NumMipLevels = mipLevels;
    barrierInfo.Subresources.NumPlanes = 1;
    barrierInfo.SyncBefore = D3D12_BARRIER_SYNC_COPY;
    barrierInfo.SyncAfter = D3D12_BARRIER_SYNC_PIXEL_SHADING;
    barrierInfo.AccessBefore = D3D12_BARRIER_ACCESS_COPY_DEST;
    barrierInfo.AccessAfter = D3D12_BARRIER_ACCESS_SHADER_RESOURCE;
    barrierInfo.LayoutBefore = D3D12_BARRIER_LAYOUT_COPY_DEST;
    barrierInfo.LayoutAfter = D3D12_BARRIER_LAYOUT_SHADER_RESOURCE;

    transferCommandBuffer->Barrier(1, &barrierGroup);

    transferCommandsUploaded++;
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

ID3D12RootSignature* CreateRootSignature(ID3D12Device2* device, CD3DX12_ROOT_PARAMETER* rootParameters, UINT parameterCount, D3D12_ROOT_SIGNATURE_FLAGS flags)
{
    ID3D12RootSignature* rootSignature;

    ID3DBlob* rootSigDescriptorLayout;

    CD3DX12_ROOT_SIGNATURE_DESC rsigDesc = {};

    rsigDesc.Init(parameterCount, rootParameters, 0, nullptr, flags);


    D3D12SerializeRootSignature(&rsigDesc, D3D_ROOT_SIGNATURE_VERSION_1, &rootSigDescriptorLayout, nullptr);

    HRESULT hr = device->CreateRootSignature(0, rootSigDescriptorLayout->GetBufferPointer(), rootSigDescriptorLayout->GetBufferSize(), IID_PPV_ARGS(&rootSignature));

    if (FAILED(hr))
    {
        printf("Failed to create root signature\n");
    }

    rootSigDescriptorLayout->Release();

    return rootSignature;
};




uintptr_t ShaderGraph::GetSet(int setIndex)
{
    uintptr_t head = (uintptr_t)(this) + sizeof(ShaderGraph) + (setIndex * sizeof(ShaderSetLayout));
    return head;
}

uintptr_t ShaderGraph::GetResource(int resourceIndex)
{
    uintptr_t head = (uintptr_t)(this) + sizeof(ShaderGraph) + (resourceSetCount * sizeof(ShaderSetLayout)) + (shaderMapCount * sizeof(ShaderMap));

    head += (sizeof(ShaderResource) * resourceIndex);

    return head;
}

uintptr_t ShaderGraph::GetMap(int mapIndex)
{
    uintptr_t head = (uintptr_t)(this) + sizeof(ShaderGraph) + (resourceSetCount * sizeof(ShaderSetLayout)) + (mapIndex * sizeof(ShaderMap));

    return head;
}

int ShaderGraph::GetGraphSize() const
{
    int size = sizeof(ShaderGraph) + (resourceSetCount * sizeof(ShaderSetLayout)) + (shaderMapCount * sizeof(ShaderMap)) + (resourceCount * sizeof(ShaderResource));
    return size;
}
int ShaderMap::GetMapSize() const
{
    return sizeof(ShaderMap);
}
#include <stdexcept>

struct ShaderDetails
{
    int shaderNameSize;
    int shaderDataSize;

    ShaderDetails* GetNext();

    char* GetString();

    void* GetShaderData();
};

ShaderDetails* ShaderDetails::GetNext()
{
    return (ShaderDetails*)((uintptr_t)this + sizeof(ShaderDetails) + shaderDataSize + shaderNameSize);
}

char* ShaderDetails::GetString()
{
    return (char*)((uintptr_t)this + sizeof(ShaderDetails));
}

void* ShaderDetails::GetShaderData()
{
    return (void*)((uintptr_t)this + sizeof(ShaderDetails) + shaderNameSize);
}

constexpr unsigned long
ShaderGraphReader::hash(char* str)
{
    unsigned long hash = 5381;
    int c;

    while (c = *str++) {
        if (((c - 'A') >= 0 && (c - 'Z') <= 0) || ((c - 'a') >= 0 && (c - 'z') <= 0))
            hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}

constexpr unsigned long
ShaderGraphReader::hash(const std::string& string)
{
    unsigned long hash = 5381;
    int c;

    const char* str = string.c_str();

    while (c = *str++) {
        if (((c - 'A') >= 0 && (c - 'Z') <= 0) || ((c - 'a') >= 0 && (c - 'z') <= 0) || ((c - '0') >= 0 && (c - '9') <= 0))
            hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}

size_t shaderGraphSize = 0;
char readerMemBuffer[64 * KiB];
size_t readerMemBufferAllocate = 0;

ShaderGraph* ShaderGraphReader::CreateShaderGraph(
    const std::string& filename
)
{

    uintptr_t TreeNodes[50]{};
    int SetNodes[15]{};
    int ShaderRefs[5]{};
    ShaderDetails* details[5]{};

    OSFileHandle fileHandle;

   auto ret = OSOpenFile(filename.c_str(), READ, &fileHandle);

    if (ret)
    {
        throw std::runtime_error("Shader Init file is unable to be opened");
    }

    char* data = (char*)AllocFromTemp(fileHandle.fileLength, 64);

    shaderGraphSize = fileHandle.fileLength;

    OSReadFile(&fileHandle, fileHandle.fileLength, data);

    OSCloseFile(&fileHandle);

    int shaderCount = 0;
    int shaderResourceCount = 0;
    int setCount = 0;

    int lastShader = 0;
    int shaderDetailDataStride = 0;

    int tagCount = 0;
    int curr = 0;

    int stride = SkipLine(data, curr);
    curr += stride;

    while (curr + stride < shaderGraphSize)
    {

        unsigned long hashl = 0;
        bool opening = true;
        stride = ProcessTag(data, curr, &hashl, &opening);
        curr += stride;

        stride = SkipLine(data, curr);

        if (opening)
        {
            tagCount++;
        }



        switch (hashl)
        {
        case hash("ShaderGraph"):
            //std::cout << "ShaderGraph" << std::endl;
            if (!opening)
                curr = shaderGraphSize;
            break;
           
        case hash("HLSLShader"):
            //std::cout << "GLSLShader" << std::endl;
            if (opening) {
                curr += stride;
                stride = SkipLine(data, curr);
                shaderCount++;
            }
            break;
            
        case hash("ShaderResourceSet"):
            //std::cout << "ShaderResourceSet" << std::endl;
            if (opening) {
                SetNodes[setCount] = tagCount;
                setCount++;
            }
            break;
        case hash("ShaderResourceItem"):
            //std::cout << "ShaderResourceItem" << std::endl;
            if (opening) {
                stride = HandleShaderResourceItem(data, curr, &TreeNodes[tagCount]);
                shaderResourceCount++;
            }
            break;
        case hash("ComputeLayout"):
            //std::cout << "ComputeLayout" << std::endl;
            if (opening) {
                stride = HandleComputeLayout(data, curr, &TreeNodes[tagCount]);
            }

            break;
        }

        curr += stride;


    }

    

    ShaderGraph* graph = (ShaderGraph*)(AllocFromTemp(sizeof(ShaderGraph) + (setCount * sizeof(ShaderSetLayout)) + (shaderResourceCount * sizeof(ShaderResource)) + (shaderCount * sizeof(ShaderMap)), 4));

    memset(graph, 0, sizeof(ShaderGraph) + (setCount * sizeof(ShaderSetLayout)) + (shaderResourceCount * sizeof(ShaderResource)));

    graph->shaderMapCount = shaderCount;
    graph->resourceSetCount = setCount;

    /*
    int shaderIndex = 0;

    for (int j = 0; j < shaderCount; j++)
    {
        ShaderMap* map = (ShaderMap*)graph->GetMap(j);

        ShaderGLSLShaderXMLTag* tag = (ShaderGLSLShaderXMLTag*)TreeNodes[ShaderRefs[j]];

        ShaderStageType type = tag->type;

        if (type == ShaderStageTypeBits::COMPUTESHADERSTAGE)
        {
            ShaderComputeLayoutXMLTag* ctag = (ShaderComputeLayoutXMLTag*)TreeNodes[ShaderRefs[j] + 1];
            ShaderDetails* deats = details[j];
            ShaderComputeLayout* layout = (ShaderComputeLayout*)deats->GetShaderData();
            layout->x = ctag->comps.x;
            layout->y = ctag->comps.y;
            layout->z = ctag->comps.z;
        }

        map->type = type;


    }
    */

    int resourceIter = 0;

    for (int i = 0; i < setCount; i++)
    {

        ShaderSetLayout* setLay = (ShaderSetLayout*)graph->GetSet(i);
        int resIter = SetNodes[i] + 1;
        ShaderResourceItemXMLTag* tag = (ShaderResourceItemXMLTag*)TreeNodes[resIter];
        int bindingCount = 0;

        setLay->resourceStart = resourceIter;
        while (tag && tag->hashCode == hash("ShaderResourceItem"))
        {

            ShaderResource* resource = (ShaderResource*)graph->GetResource(resourceIter++);
            if (tag->resourceType == ShaderResourceType::CONSTANT_BUFFER)
            {
                resource->binding = ~0;
            }
            else
            {
                resource->binding = bindingCount;
            }

            resource->stages = tag->shaderstage;

            resource->action = tag->resourceAction;
            resource->type = tag->resourceType;
            resource->arrayCount = tag->arrayCount;
            resource->set = i;
            resource->size = tag->size;
            resource->offset = tag->offset;
            setLay->bindingCount++;


            if (!(tag->resourceType == ShaderResourceType::CONSTANT_BUFFER))
            {
                bindingCount++;
            }

            tag = (ShaderResourceItemXMLTag*)TreeNodes[++resIter];
        }
    }

    graph->resourceCount = resourceIter;

    readerMemBufferAllocate = 0;

    return graph;
}

int ShaderGraphReader::ProcessTag(char* fileData, int currentLocation, unsigned long* hash, bool* opening)
{
    int count = 0;
    char* data = fileData + currentLocation;
    size_t size = shaderGraphSize;

    unsigned long hashl = 5381;

    while (currentLocation + count < size)
    {
        if (std::isspace(data[count]) && hashl == 5381)
        {
            count++;
            continue;
        }

        if (data[count] != '<' && hashl == 5381)
            throw std::runtime_error("malformed xml tag");

        if (data[count] == '\n' || data[count] == ' ' || data[count] == '>')
        {
            count++;
            break;
        }

        if (data[count] == '<')
        {
            count++;
            if (data[count] == '/')
            {
                *opening = false;
                count++;
            }
        }

        hashl = ((hashl << 5) + hashl) + data[count];

        count++;
    }

    *hash = hashl;

    return count;
}

int ShaderGraphReader::SkipLine(char* fileData, int currentLocation)
{
    int count = 0;
    char* data = fileData + currentLocation;
    size_t size = shaderGraphSize;
    while (currentLocation + count < size && data[count++] != '\n');
    data = fileData + currentLocation + count;
    return count;
}

int ShaderGraphReader::ReadValue(char* fileData, int currentLocation, char* str, int* len)
{
    int memCounter = 0;
    int count = 0;
    char* data = fileData + currentLocation;
    while (true)
    {
        int test = count++;

        int c = data[test];

        if (c == '\n')
        {
            count = count - 1;
            break;
        }

        if (std::isspace(c))
            continue;

        str[memCounter++] = c;

    }

    str[memCounter++] = 0;

    *len = (memCounter);

    return count;
}
#define MAX_ATTRIBUTE_LEN 50
int ShaderGraphReader::ReadAttributeName(char* fileData, int currentLocation, unsigned long* hash)
{
    int count = 0;
    char* data = fileData + currentLocation;

    unsigned long hashl = 5381;


    while (true)
    {
        int test = count++;

        int c = data[test];

        if (c == '>')
        {
            count = count - 1;
            break;
        }

        if (c == '=')
            break;

        if (std::isspace(c))
            continue;

        if (count == MAX_ATTRIBUTE_LEN + currentLocation)
            throw std::runtime_error("malformed xml attribute");

        hashl = ((hashl << 5) + hashl) + c;

    }

    *hash = hashl;

    return count;
}

int ShaderGraphReader::ReadAttributeValueHash(char* fileData, int currentLocation, unsigned long* hash)
{
    int count = 0;
    char* data = fileData + currentLocation;

    unsigned long hashl = 5381;

    while (true)
    {
        int test = count++;

        int c = data[test];

        if (c == '>')
        {
            count = count - 1;
            break;
        }

        if (std::isspace(c))
        {
            if (hashl == 5381)
                continue;
            else
                break;
        }

        if (c == '\"' || c == '\'')
            continue;

        if (count == MAX_ATTRIBUTE_LEN + currentLocation) {
            throw std::runtime_error("malformed xml attribute");
        }

        hashl = ((hashl << 5) + hashl) + c;

    }

    *hash = hashl;

    return count;
}

int ShaderGraphReader::ReadAttributeValueVal(char* fileData, int currentLocation, unsigned long* val)
{
    int count = 0;
    char* data = fileData + currentLocation;

    unsigned long out = 0;

    while (true)
    {
        int test = count++;

        int c = data[test];

        if (c == '>')
        {
            count = count - 1;
            break;
        }

        if (std::isspace(c))
        {
            if (out == 0)
                continue;
            else
                break;
        }

        if (c == '\"' || c == '\'')
            continue;

        if (count == (MAX_ATTRIBUTE_LEN + currentLocation) || ((c - '0') < 0 || (c - '9') > 0)) {
            throw std::runtime_error("malformed xml attribute");
        }

        out = (out * 10) + (c - '0');

    }

    *val = out;

    return count;
}

#define MAX_ATTRIBUTE_LINE_LEN 200

int ShaderGraphReader::ReadAttributes(char* fileData, int currentLocation, unsigned long* hashes, int* stackSize, int valType)
{
    int count = 0;
    char* data = fileData + currentLocation;
    size_t size = shaderGraphSize;

    int ret = 0;
    char c = data[ret];

    while (c != '>' && ret < MAX_ATTRIBUTE_LINE_LEN && (currentLocation + ret) < size)
    {
        ret += ReadAttributeName(fileData, currentLocation + ret, &hashes[count]);

        switch (hashes[count])
        {
        case hash("type"):
        case hash("used"):
        case hash("rw"):
        {
            ret += ReadAttributeValueHash(fileData, currentLocation + ret, &hashes[count + 1]);
            break;
        }
        case hash("offset"):
        case hash("size"):
        case hash("count"):
        case hash("x"):
        case hash("y"):
        case hash("z"):
        {
            ret += ReadAttributeValueVal(fileData, currentLocation + ret, &hashes[count + 1]);
            break;
        }
        }
        c = data[ret];
        count += 2;
    }

    *stackSize = count;

    while (c != '\n' && ret < MAX_ATTRIBUTE_LINE_LEN && (currentLocation + ret) < size)
    {
        c = data[ret++];
    }

    return ret;
}



int ShaderGraphReader::HandleGLSLShader(char* fileData, int currentLocation, uintptr_t* offset, void* shaderData, int* shaderDataSize)
{
    unsigned long hashes[6];

    int size = 0;

    int ret = ReadAttributes(fileData, currentLocation, hashes, &size, 0);

    uintptr_t detailHead = (uintptr_t)shaderData;

    ShaderDetails* details = (ShaderDetails*)detailHead;

    details->shaderDataSize = 0;
    details->shaderNameSize = 0;

    detailHead += sizeof(ShaderDetails);

    ShaderGLSLShaderXMLTag* tag = (ShaderGLSLShaderXMLTag*)&readerMemBuffer[readerMemBufferAllocate];

    *offset = (uintptr_t)tag;

    tag->hashCode = hash("HLSLShader");

    int stackIter = 0;

    while (size > stackIter)
    {
        unsigned long code = hashes[stackIter];
        unsigned long codeV = hashes[stackIter + 1];

        switch (code)
        {
        case hash("type"):
        {
            switch (codeV)
            {
            case hash("compute"):
                details->shaderDataSize = 12;
                tag->type = ShaderStageTypeBits::COMPUTESHADERSTAGE;
                break;
            case hash("vert"):
                tag->type = ShaderStageTypeBits::VERTEXSHADERSTAGE;
                break;
            case hash("fragment"):
                tag->type = ShaderStageTypeBits::FRAGMENTSHADERSTAGE;
                break;
            }
            break;
        }

        default:
            throw std::runtime_error("Failed GLSL Type of Shader");
            break;
        }

        stackIter += 2;
    }

    readerMemBufferAllocate += sizeof(ShaderGLSLShaderXMLTag);

    ret += ReadValue(fileData, currentLocation + ret, details->GetString(), &details->shaderNameSize);

    *shaderDataSize = (sizeof(ShaderDetails) + details->shaderNameSize + details->shaderDataSize);

    return ret;
}

int ShaderGraphReader::HandleShaderResourceItem(char* fileData, int currentLocation, uintptr_t* offset)
{
    unsigned long hashes[12];

    int size = 0;

    int ret = ReadAttributes(fileData, currentLocation, hashes, &size, 0);

    ShaderResourceItemXMLTag* tag = (ShaderResourceItemXMLTag*)&readerMemBuffer[readerMemBufferAllocate];

    *offset = (uintptr_t)tag;

    tag->hashCode = hash("ShaderResourceItem");

    tag->arrayCount = 1;

    int stackIter = 0;

    while (size > stackIter)
    {
        unsigned long code = hashes[stackIter];
        unsigned long codeV = hashes[stackIter + 1];
        switch (code)
        {
        case hash("type"):
        {
            switch (codeV)
            {
            case hash("samplerCube"):
                tag->resourceType = ShaderResourceType::SAMPLERCUBE;
                tag->resourceAction = ShaderResourceAction::SHADERREAD;
                break;
            case hash("sampler2d"):
                tag->resourceType = ShaderResourceType::SAMPLER2D;
                tag->resourceAction = ShaderResourceAction::SHADERREAD;
                break;
            case hash("image2D"):
                tag->resourceType = ShaderResourceType::IMAGESTORE2D;
                break;
            case hash("sampler"):
                tag->resourceType = ShaderResourceType::SAMPLERSTATE;
                break;
            case hash("storage"):
                tag->resourceType = ShaderResourceType::STORAGE_BUFFER;
                break;
            case hash("uniform"):
                tag->resourceType = ShaderResourceType::UNIFORM_BUFFER;
                tag->resourceAction = ShaderResourceAction::SHADERREAD;
                break;
            case hash("constantbuffer"):
                tag->resourceType = ShaderResourceType::CONSTANT_BUFFER;
                tag->resourceAction = ShaderResourceAction::SHADERREAD;
                break;
            case hash("sampler2dBindless"):
                tag->resourceType = ShaderResourceType::SAMPLERBINDLESS;
                tag->resourceAction = ShaderResourceAction::SHADERREAD;
                break;
            case hash("sampler3d"):
                tag->resourceType = ShaderResourceType::SAMPLER3D;
                tag->resourceAction = ShaderResourceAction::SHADERREAD;
                break;
            case hash("bufferView"):
                tag->resourceType = ShaderResourceType::BUFFER_VIEW;
                break;
            default:
                throw std::runtime_error("Failed Resource Type");
            }

            break;
        }
        case hash("used"):
        {
            switch (codeV)
            {
            case hash("c"):
                tag->shaderstage = ShaderStageTypeBits::COMPUTESHADERSTAGE;
                break;
            case hash("v"):
                tag->shaderstage = ShaderStageTypeBits::VERTEXSHADERSTAGE;
                break;
            case hash("f"):
                tag->shaderstage = ShaderStageTypeBits::FRAGMENTSHADERSTAGE;
                break;
            case hash("vf"):
                tag->shaderstage = ShaderStageTypeBits::VERTEXSHADERSTAGE | ShaderStageTypeBits::FRAGMENTSHADERSTAGE;
                break;
            default:
                throw std::runtime_error("Failed Used Type");
            }

            break;
        }
        case hash("rw"):
        {
            switch (codeV)
            {
            case hash("read"):
                tag->resourceAction = ShaderResourceAction::SHADERREAD;
                break;
            case hash("write"):
                tag->resourceAction = ShaderResourceAction::SHADERWRITE;
                break;
            case hash("readwrite"):
                tag->resourceAction = ShaderResourceAction::SHADERREADWRITE;
                break;
            default:
                throw std::runtime_error("Failed Action Type");
            }

            break;
        }
        case hash("count"):
        {
            tag->arrayCount = codeV;
            break;
        }
        case hash("size"):
        {
            tag->size = codeV;
            break;
        }
        case hash("offset"):
        {
            tag->offset = codeV;
            break;
        }
        }

        stackIter += 2;
    }

    readerMemBufferAllocate += sizeof(ShaderResourceItemXMLTag);

    return ret;
}


constexpr int ShaderGraphReader::ASCIIToInt(char* str)
{
    int c;
    int out = 0;

    while (c = *str++) {
        if ((c - '0') >= 0 && (c - '9') <= 0)
            out = (out * 10) + (c - '0');
    }

    return out;
}

int ShaderGraphReader::HandleComputeLayout(char* fileData, int currentLocation, uintptr_t* offset)
{
    unsigned long hashesAndVals[6];

    int size = 0;

    int ret = ReadAttributes(fileData, currentLocation, hashesAndVals, &size, 1);

    ShaderComputeLayoutXMLTag* tag = (ShaderComputeLayoutXMLTag*)&readerMemBuffer[readerMemBufferAllocate];

    *offset = (uintptr_t)tag;

    tag->hashCode = hash("ComputeLayout");

    int stackIter = 0;

    while (size > stackIter)
    {
        unsigned long code = hashesAndVals[stackIter];
        unsigned long comp = hashesAndVals[stackIter + 1];

        switch (code)
        {
        case hash("x"):
        {
            tag->comps.x = comp;

            break;
        }
        case hash("y"):
        {
            tag->comps.y = comp;
            break;
        }
        case hash("z"):
        {
            tag->comps.z = comp;

            break;
        }
        }

        stackIter += 2;
    }

    readerMemBufferAllocate += sizeof(ShaderComputeLayoutXMLTag);

    return ret;
}


ID3D12RootSignature* CreateRootSignatureFromShaderGraph(ShaderGraph* graph)
{
    UINT numDescriptorsTables = graph->resourceSetCount, numRootParameters = graph->resourceSetCount, numOfRanges = graph->resourceCount;

    UINT samplerCount = 0;

    
  
    for (int i = 0; i < graph->resourceCount; i++)
    {
        ShaderResource* resource = (ShaderResource*)graph->GetResource(i);

        switch (resource->type)
        {
        case ShaderResourceType::UNIFORM_BUFFER:

            break;
        case ShaderResourceType::SAMPLERSTATE:
           
            samplerCount++;
            break;

        case ShaderResourceType::CONSTANT_BUFFER:
           
            break;
        case ShaderResourceType::IMAGESTORE2D:

            break;
        }
    }

    if (samplerCount)
    {
        numDescriptorsTables++;
        numRootParameters++;
    }

    UINT* rangeCount = (UINT*)AllocFromTemp(sizeof(UINT) * numRootParameters, 4);
    memset(rangeCount, 0, sizeof(UINT) * numRootParameters);
    CD3DX12_DESCRIPTOR_RANGE* ranges = (CD3DX12_DESCRIPTOR_RANGE*)AllocFromTemp(sizeof(CD3DX12_DESCRIPTOR_RANGE) * numOfRanges, 4);
    ShaderStageType* visibility = (ShaderStageType*)AllocFromTemp(sizeof(ShaderStageType) * numRootParameters, 4);

    UINT samplerIndex = 0, srvIndex = 0, cbvIndex = 0, samplerRangeIndex = numOfRanges - 1, samplerRangeParameterIndex = numOfRanges-samplerCount;
    int rangeParameterIndex = 0;


    for (int i = 0; i < graph->resourceCount; i++)
    {
        ShaderResource* resource = (ShaderResource*)graph->GetResource(i);

        switch (resource->type)
        {
        case ShaderResourceType::CONSTANT_BUFFER:
        {
            CD3DX12_DESCRIPTOR_RANGE* range = &ranges[rangeParameterIndex++];
            range->Init(D3D12_DESCRIPTOR_RANGE_TYPE_CBV, 1, cbvIndex++);
            visibility[resource->set] |= resource->stages;
            rangeCount[resource->set] += 1;
            break;
        }
        case ShaderResourceType::IMAGESTORE2D:
        {
            CD3DX12_DESCRIPTOR_RANGE* range = &ranges[rangeParameterIndex++];
            range->Init(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, srvIndex++);

       
            rangeCount[resource->set] += 1;
       
            visibility[resource->set] |= resource->stages;
           
            break;

        }

        case ShaderResourceType::SAMPLERSTATE:
        {
            CD3DX12_DESCRIPTOR_RANGE* range = &ranges[samplerRangeParameterIndex++];
            range->Init(D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER, 1, samplerIndex++);
            visibility[numRootParameters - 1] |= resource->stages;
            rangeCount[numRootParameters - 1] += 1;
            break;
        }
        case ShaderResourceType::UNIFORM_BUFFER:
        {
            CD3DX12_DESCRIPTOR_RANGE* range = &ranges[rangeParameterIndex++];
            range->Init(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, srvIndex++);

            rangeCount[resource->set] += 1;
            visibility[resource->set] |= resource->stages;
            break;
        }
        }
    }

    CD3DX12_ROOT_PARAMETER* rootParameters = (CD3DX12_ROOT_PARAMETER*)AllocFromTemp(sizeof(CD3DX12_ROOT_PARAMETER) * numRootParameters, 4);

    UINT rootParamIndex = 0, rangeIterIndex = 0;


    for (int i = 0; i < graph->resourceSetCount; i++)
    {
        ShaderSetLayout* layout = (ShaderSetLayout*)graph->GetSet(i);

        D3D12_SHADER_VISIBILITY visible = D3D12_SHADER_VISIBILITY_ALL;

        if (!(visibility[rootParamIndex] & (visibility[rootParamIndex] - 1)))
        {
            switch (visibility[rootParamIndex])
            {
            case ShaderStageTypeBits::COMPUTESHADERSTAGE:
                visible = D3D12_SHADER_VISIBILITY_ALL;
                break;

            case ShaderStageTypeBits::VERTEXSHADERSTAGE:
                visible = D3D12_SHADER_VISIBILITY_VERTEX;
                break;

            case ShaderStageTypeBits::FRAGMENTSHADERSTAGE:
                visible = D3D12_SHADER_VISIBILITY_PIXEL;
                break;
            }
        }

        rootParameters[rootParamIndex].InitAsDescriptorTable(rangeCount[i], ranges + rangeIterIndex, visible);

        rootParamIndex++;

        rangeIterIndex += rangeCount[i];
    }

    if (samplerCount)
    {
        D3D12_SHADER_VISIBILITY visible = D3D12_SHADER_VISIBILITY_ALL;

        if (!(visibility[numRootParameters-1] & (visibility[numRootParameters - 1] - 1)))
        {
            switch (visibility[numRootParameters - 1])
            {
            case ShaderStageTypeBits::COMPUTESHADERSTAGE:
                visible = D3D12_SHADER_VISIBILITY_ALL;
                break;

            case ShaderStageTypeBits::VERTEXSHADERSTAGE:
                visible = D3D12_SHADER_VISIBILITY_VERTEX;
                break;

            case ShaderStageTypeBits::FRAGMENTSHADERSTAGE:
                visible = D3D12_SHADER_VISIBILITY_PIXEL;
                break;
            }
        }

        rootParameters[numRootParameters - 1].InitAsDescriptorTable(samplerCount, ranges + rangeIterIndex, visible);
    }
   
    ID3D12RootSignature* rootSignature = CreateRootSignature(deviceHandle, rootParameters, numRootParameters, D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT);

    return rootSignature;
}

void CreateDescriptorHeapManager(DescriptorHeap* heap, UINT maxDescriptorHandles, D3D12_DESCRIPTOR_HEAP_TYPE type, D3D12_DESCRIPTOR_HEAP_FLAGS flags)
{
    heap->descriptorHeap = CreateDescriptorHeap(deviceHandle, type, maxDescriptorHandles, flags, &heap->descriptorHeapHandleSize);
    heap->maxDescriptorHeapHandles = maxDescriptorHandles;
    heap->descriptorHeapHandlePointer = 0;
    heap->type = type;
}

int CreateDescriptorTable(uintptr_t header, int descriptorCount, int frameCount, DescriptorHeap* heap)
{
    int heapstart = heap->descriptorHeapHandlePointer;
    for (int i = 0; i < frameCount; i++)
    {
        uintptr_t ptr = header;
        for (int j = 0; j < descriptorCount; j++)
        {


            DescriptorTypeHeader* header = (DescriptorTypeHeader*)ptr;
            switch (header->type)
            {
            case PUSHCONSTANTS:
                break;
            case CONSTANTBUFFER:
            {
                DescriptorTypeConstantBuffer* cbType = (DescriptorTypeConstantBuffer*)header;

                Allocation* alloc = &allocationHandle[cbType->allocationIndex];

                CreateCBVDescriptorHandle(deviceHandle, alloc->bufferHandle, i * alloc->stridesize + alloc->offset, alloc->stridesize , heap);

                ptr += sizeof(DescriptorTypeConstantBuffer);
                break;
            }
            case UNIFORMBUFFER:
            {
                DescriptorTypeUniformBuffer* ubType = (DescriptorTypeUniformBuffer*)header;

                Allocation* alloc = &allocationHandle[ubType->allocationIndex];
                CreateSRVDescriptorHandle(deviceHandle,
                    alloc->bufferHandle, i * alloc->stridesize + alloc->offset,
                    ubType->numberOfElements, alloc->requestedsize,
                    ubType->format,
                    heap,
                    D3D12_SRV_DIMENSION_BUFFER
                );

                ptr += sizeof(DescriptorTypeUniformBuffer);
                break;
            }

            case IMAGESRV:
            {
                DescriptorTypeImageSRV* ubType = (DescriptorTypeImageSRV*)header;

     
                CreateImageSRVDescriptorHandle(deviceHandle,
                    ubType->image,
                    1,
                    ubType->format,
                    heap,
                    D3D12_SRV_DIMENSION_TEXTURE2D
                );

                ptr += sizeof(DescriptorTypeImageSRV);

                break;
            }
            case SAMPLERSTATE:
                CreateImageSampler(deviceHandle, heap);
                ptr += sizeof(DescriptorTypeImageSRV);
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
    char* data = nullptr; 
    OSFileHandle outHandle{};

    OSFileFlags openingFlags = READ;

    int nRet = OSOpenFile(name, openingFlags, &outHandle);

    if (nRet)
    {
        return;
    }

    int size = outHandle.fileLength;

    data = (char*)AllocFromTemp(size, 64);

    OSReadFile(&outHandle, size, data);

    OSCloseFile(&outHandle);

    auto iter = data;
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


    auto iter2 = data + (fh.bfOffBits + (uLine * bytesPerRow));

    details->data = (char*)AllocFromTemp(details->dataSize, 64);

    auto copy = details->data;

    for (; i < bottomLine; i++)
    {
        memcpy(copy, iter2, bytesPerRow);
        copy += bytesPerRow;
        iter2 -= bytesPerRow;
    }

    memcpy(copy, iter2, bytesPerRow);

    details->miplevels = 1;
}

void CreateTablesFromResourceSet(int* descriptorsets, int numDescriptorSet, PipelineObject* obj)
{
    int samplerCount = 0;

    uintptr_t dx12Sampler = (uintptr_t)AllocFromTemp(512, 4);
    uintptr_t dx12SamplerPtr = dx12Sampler;

    for (int i = 0; i < numDescriptorSet; i++)
    {
        
        uintptr_t head = descriptorSets[descriptorsets[i]];



        ShaderResourceSet* set = (ShaderResourceSet*)head;
        uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));
        int count = set->bindingCount;


        uintptr_t dx12Descriptions = (uintptr_t)AllocFromTemp(512, 4);
        uintptr_t dx12Ptr = dx12Descriptions;

        int currentTableCount = 0;

        for (int i = 0; i < count; i++)
        {
            ShaderResourceHeader* header = (ShaderResourceHeader*)offsets[i];

            switch (header->type)
            {
            case ShaderResourceType::IMAGESTORE2D:
            {
                ShaderResourceImage* image = (ShaderResourceImage*)header;
                DescriptorTypeImageSRV* srv = (DescriptorTypeImageSRV*)dx12Ptr;
                srv->type = IMAGESRV;
                srv->format = DXGI_FORMAT_B8G8R8A8_UNORM_SRGB;
                srv->image = bgraImageMemoryPool;

                dx12Ptr += sizeof(DescriptorTypeImageSRV);
                currentTableCount++;
                break;
            }
            case ShaderResourceType::SAMPLERSTATE:
            {
                ShaderResourceSampler* image = (ShaderResourceSampler*)header;
                samplerCount++;
                DescriptorTypeHeader* header = (DescriptorTypeHeader*)dx12SamplerPtr;
                header->type = SAMPLERSTATE;
                dx12SamplerPtr += sizeof(DescriptorTypeHeader);
                break;
            }

            case ShaderResourceType::SAMPLER3D:
            case ShaderResourceType::SAMPLER2D:
            case ShaderResourceType::SAMPLERCUBE:
            {
                ShaderResourceImage* image = (ShaderResourceImage*)header;

                break;
            }
            case ShaderResourceType::CONSTANT_BUFFER:
            {
                ShaderResourceConstantBuffer* buffer = (ShaderResourceConstantBuffer*)header;

                DescriptorTypeConstantBuffer* cbv = (DescriptorTypeConstantBuffer*)dx12Ptr;
                cbv->type = CONSTANTBUFFER;
                cbv->allocationIndex = buffer->allocationIndex;

                dx12Ptr += sizeof(DescriptorTypeConstantBuffer);
                currentTableCount++;
                break;
            }

            case ShaderResourceType::STORAGE_BUFFER:
            {
                ShaderResourceBuffer* buffer = (ShaderResourceBuffer*)header;
                auto alloc = allocationHandle[buffer->allocation];

                break;
            }
            case ShaderResourceType::UNIFORM_BUFFER:
            {
                ShaderResourceBuffer* buffer = (ShaderResourceBuffer*)header;
                DescriptorTypeUniformBuffer* ubv = (DescriptorTypeUniformBuffer*)dx12Ptr;
                ubv->type = UNIFORMBUFFER;
                ubv->allocationIndex = buffer->allocation;
                ubv->format = DXGI_FORMAT_UNKNOWN;
                ubv->numberOfElements = 1;
                dx12Ptr += sizeof(DescriptorTypeUniformBuffer);
                currentTableCount++;
                break;
            }

            case ShaderResourceType::SAMPLERBINDLESS:
            {
                ShaderResourceSamplerBindless* samplers = (ShaderResourceSamplerBindless*)header;
                break;
            }


            case ShaderResourceType::BUFFER_VIEW:
            {
                ShaderResourceBufferView* bufferView = (ShaderResourceBufferView*)header;

                break;
            }
            }
        }

        if (srvDescriptorTablesStart[descriptorsets[i]] != -1)
        {
            obj->descriptorHeapPointer[i] = srvDescriptorTablesStart[descriptorsets[i]];
        }
        else 
        {
            obj->descriptorHeapPointer[i] = CreateDescriptorTable(dx12Descriptions, currentTableCount, MAX_FRAMES_IN_FLIGHT, &mainSRVDescriptorHeap);
            srvDescriptorTablesStart[descriptorsets[i]] = obj->descriptorHeapPointer[i];
        }

        obj->resourceCount[i] = currentTableCount;
        obj->descriptorHeapSelection[i] = 0;
        
    }

    if (samplerCount)
    {
        obj->descriptorHeapPointer[numDescriptorSet] = CreateDescriptorTable(dx12Sampler, samplerCount, MAX_FRAMES_IN_FLIGHT, &mainSamplerDescriptorHeap);
        obj->resourceCount[numDescriptorSet] = samplerCount;
        obj->descriptorHeapSelection[numDescriptorSet] = 1;
    }
}