#define WIN32_LEAN_AND_MEAN

#include <Windows.h>
#include <string>
#include <array>
#include "Types.h"
#include "Files.h"






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


enum DX12ComType
{
    DXGISWAPCHAIN = 0,
    D12QUEUE = 1,
    D12COMMANDBUFFER7 = 2,
    D12DESCRIPTORHEAP = 3,
    D12SHADERBLOB = 4,
    D12MEMHEAP = 5,
    D12ROOTSIGNATURE = 6,
    D12PIPELINESTATE = 7,
    D12COMMANDPOOL = 8,
    D12RESOURCEVIEW = 9,
};

typedef size_t EntryHandle;

static void* DriverCOMHandlePool[50];
static size_t DriverCOMHandlePoolAllocator = 0;

struct PoolItem
{
    DX12ComType comType;
    size_t allocationOffset;
};

static PoolItem handles[50];
static EntryHandle handlesPointer = 0;



void ReleaseAllDriverCOMHandles()
{
    for (size_t i = 0; i < handlesPointer; ++i)
    {
        size_t offset = handles[i].allocationOffset;

        if (offset >= 50)
            continue;

        void* rawPtr = DriverCOMHandlePool[offset];
        if (!rawPtr)
            continue;

        switch (handles[i].comType)
        {
        case DXGISWAPCHAIN:
        case D12QUEUE:
        case D12COMMANDBUFFER7:
        case D12DESCRIPTORHEAP:
        case D12SHADERBLOB:
        case D12MEMHEAP:
        case D12ROOTSIGNATURE:
        case D12PIPELINESTATE:
        case D12COMMANDPOOL:
        {
            IUnknown* unknown = static_cast<IUnknown*>(rawPtr);
            unknown->Release();
        }
        break;
        }

        DriverCOMHandlePool[offset] = nullptr;
    }

    handlesPointer = 0;
    DriverCOMHandlePoolAllocator = 0;
}


void* AllocFromTemp(size_t size, size_t alignment)
{
    size_t current = tempGlobalAllocator;

    if (current + size >= TEMP_MEM_SIZE)
        current = 0;

    current = (current + alignment - 1) & ~(alignment - 1);

    tempGlobalAllocator += (size)+(current - tempGlobalAllocator);

    return (void*)&tempGlobalMemoryPool[current];
}

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


EntryHandle queueHandle;

EntryHandle swapChain;

ID3D12Resource* swapChainImages[MAX_FRAMES_IN_FLIGHT];

ID3D12Resource* swapChainDepthImages[MAX_FRAMES_IN_FLIGHT];

EntryHandle graphicCommandBuffers[MAX_FRAMES_IN_FLIGHT];

EntryHandle graphicCommandPools[MAX_FRAMES_IN_FLIGHT];

EntryHandle transferCommandPool;
EntryHandle transferCommandBuffer;
int transferCommandsUploaded = 0;

EntryHandle globalRTVDescriptorHeap;
EntryHandle globalDSVDescriptorHeap;


UINT globalRTVDescriptorSize;
UINT globalDSVDescriptorSize;
UINT currentFrame;


struct D12Fence
{
    EntryHandle fenceHandle;
    uint64_t fenceValue;
    HANDLE fenceEvent;
};

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
    EntryHandle shader;
};

ShaderHandles shaderHandles[2]{};

EntryHandle bgraImageMemoryPool;

struct TextureDetails
{
    DXGI_FORMAT type;
    uint32_t dataSize;
    uint32_t width, height, miplevels;
    char* data;
};


struct DriverMemoryBuffer
{
    EntryHandle bufferHandle;
    size_t sizeOfAlloc;
    size_t currentPointer;
    D3D12_RESOURCE_STATES currentState;
    D3D12_RESOURCE_STATES initialState;
};


struct TextureMemoryPool
{
    EntryHandle heap;
    size_t currentPointer;
    size_t sizeOfHeap;
    size_t alignment;
};

TextureMemoryPool bgraPool;

TextureMemoryPool rsvdsvPool;

void CreateTextureMemoryPool(TextureMemoryPool* pool, size_t sizeOfPool, size_t alignment);
void CreateDSVRSVMemoryPool(TextureMemoryPool* pool, size_t sizeOfPool, size_t alignment, bool msaa);

void ParseBMP(TextureDetails* details, const char* name);

ID3D12CommandQueue* CreateCommandQueue(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type);
EntryHandle CreateCommandQueue(D3D12_COMMAND_LIST_TYPE type);








ID3D12Device2* CreateDevice(IDXGIAdapter4* adapter, bool debug);
bool CheckTearingSupport();
void EnableRuntimeValidation();
IDXGIAdapter4* GetAdapter(UINT createFactoryFlags);
IDXGISwapChain4* CreateSwapChain(HWND hWnd, ID3D12CommandQueue* commandQueue, int width, int height, int bufferCount, UINT debug);

EntryHandle CreateSwapChain(HWND hWnd, EntryHandle commandQueue, int width, int height, int bufferCount, UINT debug);


int Render();
void Flush(ID3D12CommandQueue* commandQueue, ID3D12Fence* fence, uint64_t& fenceValue, HANDLE fenceEvent);
void WaitForFenceValue(ID3D12Fence* fence, uint64_t fenceValue, HANDLE fenceEvent, DWORD duration);
uint64_t Signal(ID3D12CommandQueue* commandQueue, ID3D12Fence* fence, uint64_t& fenceValue);
ID3D12Fence* CreateFence(ID3D12Device2* device);
HANDLE CreateEventHandle();
ID3D12GraphicsCommandList7* CreateCommandList(ID3D12Device2* device,
    ID3D12CommandAllocator* commandAllocator, D3D12_COMMAND_LIST_TYPE type);

EntryHandle CreateCommandList(EntryHandle commandAllocator, D3D12_COMMAND_LIST_TYPE type);


ID3D12CommandAllocator* CreateCommandAllocator(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type);

EntryHandle CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE type);


int CreateRenderTargetView(ID3D12Device2* device, IDXGISwapChain4* swapChain, ID3D12DescriptorHeap* descriptorHeap, ID3D12Resource** outBuffers, UINT rtvDescriptorSize);
ID3D12DescriptorHeap* CreateDescriptorHeap(ID3D12Device2* device, D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize);
EntryHandle CreateDescriptorHeap(D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize);
void ReleaseD3D12Resources();
int CreateDepthStencilView(ID3D12Device2* device, ID3D12DescriptorHeap* descriptorHeap, TextureMemoryPool* pool, ID3D12Resource** outBuffers, UINT dsvDescriptorSize);
EntryHandle CreateShaderBlob(const char* shaderfile);
void CreateHostBuffer(DriverMemoryBuffer* dmb, UINT size, D3D12_RESOURCE_FLAGS flags);
void CreateDeviceBuffer(DriverMemoryBuffer* dmb, UINT size, D3D12_RESOURCE_FLAGS flags);
ID3D12Resource* CreateDeviceLocalBuffer(ID3D12Device2* device, UINT size, D3D12_RESOURCE_FLAGS flags);
void WriteToHostMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies);
void WriteToDeviceLocalMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies);

ID3D12Resource* CreateCommittedImageResource(ID3D12Device2* device, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);

ID3D12Resource* CreatePlacedImageResource(ID3D12Device2* device, TextureMemoryPool* pool, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);

EntryHandle CreatePlacedImageResource(TextureMemoryPool* pool, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);




void WriteToImageDeviceLocalMemory(ID3D12Resource* imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers);
void TransitionBufferBarrier(ID3D12GraphicsCommandList7* cmdBuffer, ID3D12Resource* resource, D3D12_BARRIER_SYNC srcSync, D3D12_BARRIER_ACCESS srcAccess, D3D12_BARRIER_SYNC dstSync, D3D12_BARRIER_ACCESS dstAccess);
EntryHandle CreateRootSignatureFromShaderGraph(ShaderGraph* graph);
ID3D12Heap* CreateDX12Heap(SIZE_T size, SIZE_T alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType);
struct Camera
{
    XMMATRIX proj;
    XMMATRIX view;
};

XMMATRIX world[2];

Camera cam;


struct PipelineObject
{
    EntryHandle rootSignature;
    EntryHandle pipelineState;
    int heapsCount;
    ID3D12DescriptorHeap* descriptorHeap[8];
    int descriptorTableCount;
    int descriptorHeapPointer[8];
    int resourceCount[8];
    int descriptorHeapSelection[8]; //number of descriptor tables
    D3D_PRIMITIVE_TOPOLOGY topology;//D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST
    int instanceCount;
    
    EntryHandle vertexBuffer = ~0ui64;
    UINT vertexBufferOffset;
    UINT vertexBufferSize;
    int vertexSize;
    int vertexCount;
    EntryHandle indexBuffer = ~0ui64;
    UINT indexBufferOffset;
    UINT indexBufferSize;
    int indexSize;
    int indexCount;

};


PipelineObject triangles[2];

ID3D12PipelineState* CreatePipelineStateObject(ID3D12Device2* device, ID3D12RootSignature* _rootSignature, ShaderHandles* handles, int count);

EntryHandle CreatePipelineStateObject(EntryHandle _rootSignature, ShaderHandles* handles, int count);


EntryHandle CreateRootSignature(CD3DX12_ROOT_PARAMETER* rootParameters, UINT parameterCount, D3D12_ROOT_SIGNATURE_FLAGS flags);
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
    EntryHandle descriptorHeap;
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
    EntryHandle bufferHandle;
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
    EntryHandle image;
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

EntryHandle AllocTypeForEntry(void* data, DX12ComType type)
{
    size_t comPtrLoc = DriverCOMHandlePoolAllocator++;
    EntryHandle poolHandle = handlesPointer++;

    DriverCOMHandlePool[comPtrLoc] = data;
    handles[poolHandle].allocationOffset = comPtrLoc;
    handles[poolHandle].comType = type;

    return poolHandle;
}

void* GetAndValidateItem(EntryHandle poolHandle, DX12ComType type)
{
    PoolItem* item = &handles[poolHandle];
    if (type != item->comType)
    {
        printf("Mismatched entry in pools");
        return NULL;
    }

    return DriverCOMHandlePool[item->allocationOffset];
}

void DoSceneStuff()
{
    ID3D12CommandAllocator* transCommandPool = (ID3D12CommandAllocator*)GetAndValidateItem(transferCommandPool, D12COMMANDPOOL);
    
    transCommandPool->Reset();

    ID3D12GraphicsCommandList7* transCommandBuffer = (ID3D12GraphicsCommandList7*)GetAndValidateItem(transferCommandBuffer, D12COMMANDBUFFER7);

    transCommandBuffer->Reset(transCommandPool, NULL);

    CreateTextureMemoryPool(&bgraPool, 256 * MiB, D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT);

    ShaderGraph* mainLayout = ShaderGraphReader::CreateShaderGraph("DirectLayout.xml");

    

    triangles[0].rootSignature = CreateRootSignatureFromShaderGraph(mainLayout);//CreateGenericRootSignature();

    triangles[0].pipelineState = CreatePipelineStateObject(triangles[0].rootSignature, shaderHandles, 2);


    TextureDetails details{};

    ParseBMP(&details, "face1.bmp");

    int cameraData = AllocFromHostBuffer(sizeof(Camera), sizeof(Camera), MAX_FRAMES_IN_FLIGHT);
    int world1Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 256, MAX_FRAMES_IN_FLIGHT);
    int world2Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 256, MAX_FRAMES_IN_FLIGHT);
    int vertexOffset = AllocFromDeviceBuffer(sizeof(BoxVerts), 16, 1);
    int indexOffset = AllocFromDeviceBuffer(sizeof(BoxIndices), 16, 1);

    ID3D12GraphicsCommandList7* transCmdBuffer = (ID3D12GraphicsCommandList7*)GetAndValidateItem(transferCommandBuffer, D12COMMANDBUFFER7);

    ID3D12Resource* deviceLocalHandle = (ID3D12Resource*)GetAndValidateItem(deviceLocalBuffer.bufferHandle, D12RESOURCEVIEW);

    TransitionBufferBarrier(transCmdBuffer, deviceLocalHandle, D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS, D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST);

    WriteToDeviceLocalMemory(vertexOffset, BoxVerts, sizeof(BoxVerts), 0, 1);

    WriteToDeviceLocalMemory(indexOffset, BoxIndices, sizeof(BoxIndices), 0, 1);

    WriteToHostMemory(cameraData, &cam, sizeof(Camera), 0, MAX_FRAMES_IN_FLIGHT);

    WriteToDeviceLocalMemory(world1Data, world, 64, 0, MAX_FRAMES_IN_FLIGHT);

    WriteToDeviceLocalMemory(world2Data, &world[1], 64, 0, MAX_FRAMES_IN_FLIGHT);

    TransitionBufferBarrier(transCmdBuffer, deviceLocalHandle, D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST, D3D12_BARRIER_SYNC_DRAW | D3D12_BARRIER_SYNC_INDEX_INPUT, D3D12_BARRIER_ACCESS_VERTEX_BUFFER | D3D12_BARRIER_ACCESS_CONSTANT_BUFFER | D3D12_BARRIER_ACCESS_INDEX_BUFFER);

    bgraImageMemoryPool = CreatePlacedImageResource(&bgraPool, details.width, details.height, 1, details.miplevels, D3D12_RESOURCE_FLAG_NONE, details.type, D3D12_RESOURCE_DIMENSION_TEXTURE2D);

    ID3D12Resource* imageHandle = (ID3D12Resource*)GetAndValidateItem(bgraImageMemoryPool, D12RESOURCEVIEW);

    WriteToImageDeviceLocalMemory(imageHandle, details.data, details.width, details.height, 4, details.dataSize, details.type, details.miplevels, 1);

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

    ID3D12DescriptorHeap* srvHeap = (ID3D12DescriptorHeap*)GetAndValidateItem(mainSRVDescriptorHeap.descriptorHeap, D12DESCRIPTORHEAP);
    ID3D12DescriptorHeap* samplerHeap = (ID3D12DescriptorHeap*)GetAndValidateItem(mainSamplerDescriptorHeap.descriptorHeap, D12DESCRIPTORHEAP);
    
    triangles[0].topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
    triangles[0].heapsCount = 2;
    triangles[0].instanceCount = 1;
    triangles[0].vertexCount = 24;
    triangles[0].indexCount = 36;
    triangles[0].descriptorHeap[0] = srvHeap;
    triangles[0].descriptorHeap[1] = samplerHeap;


    triangles[1].topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
    triangles[1].heapsCount = 2;
    triangles[1].instanceCount = 1;
    triangles[1].vertexCount = 24;
    triangles[1].indexCount = 36;
    triangles[1].descriptorHeap[0] = srvHeap;
    triangles[1].descriptorHeap[1] = samplerHeap;

    triangles[1].pipelineState = triangles[0].pipelineState;
    triangles[1].rootSignature = triangles[0].rootSignature;

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
  
    queueHandle = CreateCommandQueue(D3D12_COMMAND_LIST_TYPE_DIRECT);

    swapChain = CreateSwapChain(
        hwnd,
        queueHandle,
        800,
        600,
        MAX_FRAMES_IN_FLIGHT,
        0
    );

    globalRTVDescriptorHeap = CreateDescriptorHeap(
        D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
        MAX_FRAMES_IN_FLIGHT,
        D3D12_DESCRIPTOR_HEAP_FLAG_NONE, &globalRTVDescriptorSize
    );

 
    globalDSVDescriptorHeap = CreateDescriptorHeap(
        D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
        MAX_FRAMES_IN_FLIGHT,
        D3D12_DESCRIPTOR_HEAP_FLAG_NONE, &globalDSVDescriptorSize
    );

    CreateDescriptorHeapManager(&mainSRVDescriptorHeap, MAX_FRAMES_IN_FLIGHT * 100, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE);

    CreateDescriptorHeapManager(&mainSamplerDescriptorHeap, MAX_FRAMES_IN_FLIGHT * 100, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER, D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE);

    if (CreateRenderTargetView(
        deviceHandle,
        (IDXGISwapChain4*)GetAndValidateItem(swapChain, DXGISWAPCHAIN),
        (ID3D12DescriptorHeap*)GetAndValidateItem(globalRTVDescriptorHeap, D12DESCRIPTORHEAP),
        swapChainImages, globalRTVDescriptorSize) < 0)
    {
        goto end;
    }

    CreateDSVRSVMemoryPool(&rsvdsvPool, 24 * MiB, (1<<22), false);

    if (CreateDepthStencilView(
        deviceHandle,
        (ID3D12DescriptorHeap*)GetAndValidateItem(globalDSVDescriptorHeap, D12DESCRIPTORHEAP),
        &rsvdsvPool,
        swapChainDepthImages, globalDSVDescriptorSize) < 0)
    {
        goto end;
    }

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        graphicCommandPools[i] =
            CreateCommandAllocator( D3D12_COMMAND_LIST_TYPE_DIRECT);
    }

    transferCommandPool = CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT);

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        graphicCommandBuffers[i] = CreateCommandList(
            graphicCommandPools[i],
            D3D12_COMMAND_LIST_TYPE_DIRECT
        );
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
        transferCommandPool,
        D3D12_COMMAND_LIST_TYPE_DIRECT
    );


    world[0] = XMMatrixIdentity();
    world[1] = XMMatrixTranslation(2.0, 2.0, 0.0);

    shaderHandles[0] = { VERTEX, CreateShaderBlob("VS.bin") };
    shaderHandles[1] = { PIXEL, CreateShaderBlob("PS.bin") };

    

    CreateHostBuffer(&hostBuffer, 8 * KiB, D3D12_RESOURCE_FLAG_NONE);

    CreateDeviceBuffer(&deviceLocalBuffer, 8 * KiB, D3D12_RESOURCE_FLAG_NONE);

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        CreateHostBuffer(&stagingBuffers[i], 64 * MiB, D3D12_RESOURCE_FLAG_NONE);
    }


   

    DoSceneStuff();   

    g_IsInitialized = true;

    while (!shouldClose)
    {
        PollDX12WindowEvents();
        int ret = Render();
        if (ret < 0) break;
    }

    //Flush(queueHandle, g_Fence, g_FenceValue, g_FenceEvent);

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

EntryHandle CreateShaderBlob(const char* shaderfile)
{
    ID3DBlob* shaderBlob;

    WCHAR str[250]; 
    mbstowcs(str, shaderfile, 250);

    HRESULT result = D3DReadFileToBlob(str, &shaderBlob);

    if (FAILED(result))
    {
        return NULL;
    }

    return AllocTypeForEntry(shaderBlob, D12SHADERBLOB);
    
}

EntryHandle CreatePipelineStateObject(EntryHandle _rootSignature, ShaderHandles* handles, int count)
{

    ID3D12RootSignature* rootSignH = (ID3D12RootSignature*)GetAndValidateItem(_rootSignature, D12ROOTSIGNATURE);
    ID3D12PipelineState* pipelineState = CreatePipelineStateObject(deviceHandle, rootSignH, handles, count);

    return AllocTypeForEntry(pipelineState, D12PIPELINESTATE);
}

ID3D12PipelineState* CreatePipelineStateObject(ID3D12Device2* device, ID3D12RootSignature* _rootSignature, ShaderHandles* handles, int count)
{
    ID3D12PipelineState* pipelineState;

    HRESULT hr;

    D3D12_GRAPHICS_PIPELINE_STATE_DESC desc{};
    desc.pRootSignature = _rootSignature;


    for (int i = 0; i < count; i++)
    {

        ID3DBlob* blob = (ID3DBlob*)GetAndValidateItem(shaderHandles[i].shader, D12SHADERBLOB);

        SIZE_T bcLen = blob->GetBufferSize();
        void* shaderData = blob->GetBufferPointer();
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

    ReleaseAllDriverCOMHandles();

 
    if (g_Fence && queueHandle)
    {
        g_FenceValue++;
        //queueHandle->Signal(g_Fence, g_FenceValue);

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

EntryHandle CreateCommandQueue(D3D12_COMMAND_LIST_TYPE type)
{
    ID3D12CommandQueue* queue = CreateCommandQueue(deviceHandle, type);
    return AllocTypeForEntry(queue, D12QUEUE);
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


EntryHandle CreateSwapChain(HWND hWnd, EntryHandle commandQueue, int width, int height, int bufferCount, UINT debug)
{
    ID3D12CommandQueue* queue = (ID3D12CommandQueue*)GetAndValidateItem(commandQueue, D12QUEUE);

    IDXGISwapChain4* swcChain = CreateSwapChain(hWnd, queue, width, height, bufferCount, debug);

    return AllocTypeForEntry(swcChain, DXGISWAPCHAIN);
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

EntryHandle CreateDescriptorHeap(D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize)
{
    ID3D12DescriptorHeap* heap = CreateDescriptorHeap(deviceHandle, type, numDescriptors, flags, descriptorSize);
    return AllocTypeForEntry(heap, D12DESCRIPTORHEAP);
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

int CreateDepthStencilView(ID3D12Device2* device, ID3D12DescriptorHeap* descriptorHeap, TextureMemoryPool *pool, ID3D12Resource** outBuffers, UINT dsvDescriptorSize)
{
    CD3DX12_CPU_DESCRIPTOR_HANDLE dsvHandle(descriptorHeap->GetCPUDescriptorHandleForHeapStart());

    ID3D12Heap* heap = (ID3D12Heap*)GetAndValidateItem(pool->heap, D12MEMHEAP);

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

        D3D12_RESOURCE_ALLOCATION_INFO allocInfo =
            device->GetResourceAllocationInfo(0, 1, &depthStencilDesc);

        UINT64 location = (pool->currentPointer + allocInfo.Alignment - 1) & ~(allocInfo.Alignment - 1);

        pool->currentPointer += (allocInfo.SizeInBytes + (location - pool->currentPointer));

        if (FAILED(device->CreatePlacedResource(heap, location, &depthStencilDesc, D3D12_RESOURCE_STATE_DEPTH_WRITE, &clearValue, IID_PPV_ARGS(&depthBuffer))))
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



EntryHandle CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE type)
{
    ID3D12CommandAllocator* commandAllocator = CreateCommandAllocator(deviceHandle, type);

    return AllocTypeForEntry(commandAllocator, D12COMMANDPOOL);
}


EntryHandle CreateCommandList(EntryHandle commandAllocator, D3D12_COMMAND_LIST_TYPE type)
{
    ID3D12CommandAllocator* commAllocator = (ID3D12CommandAllocator*)GetAndValidateItem(commandAllocator, D12COMMANDPOOL);

    ID3D12GraphicsCommandList* commandList = CreateCommandList(deviceHandle, commAllocator, type);

    return AllocTypeForEntry(commandList, D12COMMANDBUFFER7);
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

    auto commandAllocator = graphicCommandPools[currentFrame];

    auto backBuffer = swapChainImages[currentFrame];

    auto depthBuffer = swapChainDepthImages[currentFrame];

    auto graphicCommandBuffer = graphicCommandBuffers[currentFrame];

    ID3D12CommandQueue* lQueueHandle = (ID3D12CommandQueue*)GetAndValidateItem(queueHandle, D12QUEUE);

    ID3D12GraphicsCommandList7* gCommandBuffer = (ID3D12GraphicsCommandList7*)GetAndValidateItem(graphicCommandBuffer, D12COMMANDBUFFER7);

    ID3D12CommandAllocator* commandPool = (ID3D12CommandAllocator*)GetAndValidateItem(commandAllocator, D12COMMANDPOOL);


    if (transferCommandsUploaded)
    {

        ID3D12GraphicsCommandList7* transCommandBuffer = (ID3D12GraphicsCommandList7*)GetAndValidateItem(transferCommandBuffer, D12COMMANDBUFFER7);

      
        transCommandBuffer->Close();

        ID3D12CommandList* const commandLists[] = {
            transCommandBuffer
        };

        lQueueHandle->ExecuteCommandLists(_countof(commandLists), commandLists);

        uint64_t fenceValue = 0;

        uint64_t fencesign = Signal(lQueueHandle, t_Fence, fenceValue);

        Flush(lQueueHandle, t_Fence, fenceValue, t_FenceEvent);

        ID3D12CommandAllocator* transCommandPool = (ID3D12CommandAllocator*)GetAndValidateItem(transferCommandPool, D12COMMANDPOOL);

        transCommandPool->Reset();

        transCommandBuffer->Reset(transCommandPool, NULL);

        transferCommandsUploaded = 0;
    }

    commandPool->Reset();

    gCommandBuffer->Reset(commandPool, nullptr);


    ID3D12DescriptorHeap* dsvDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(globalDSVDescriptorHeap, D12DESCRIPTORHEAP);
    ID3D12DescriptorHeap* rsvDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(globalRTVDescriptorHeap, D12DESCRIPTORHEAP);

    CD3DX12_CPU_DESCRIPTOR_HANDLE dsvHandle(dsvDescriptor->GetCPUDescriptorHandleForHeapStart(), currentFrame, globalDSVDescriptorSize);

    CD3DX12_CPU_DESCRIPTOR_HANDLE rtvHandle(rsvDescriptor->GetCPUDescriptorHandleForHeapStart(), currentFrame, globalRTVDescriptorSize);

    
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

        gCommandBuffer->Barrier(1, &barrierGroup);
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


    gCommandBuffer->BeginRenderPass(1, &rtvDesc, &depthDesc, D3D12_RENDER_PASS_FLAG_NONE);


    gCommandBuffer->RSSetViewports(1, &viewport);
    gCommandBuffer->RSSetScissorRects(1, &scissor);

    for (int i = 0; i < 2; i++)
    {

        ID3D12RootSignature* sign = (ID3D12RootSignature*)GetAndValidateItem(triangles[0].rootSignature, D12ROOTSIGNATURE);

        gCommandBuffer->SetGraphicsRootSignature(sign);

        gCommandBuffer->SetDescriptorHeaps(triangles[i].heapsCount, triangles[i].descriptorHeap);

        ID3D12PipelineState* pipelineState = (ID3D12PipelineState*)GetAndValidateItem(triangles[i].pipelineState, D12PIPELINESTATE);

        gCommandBuffer->SetPipelineState(pipelineState);
        gCommandBuffer->IASetPrimitiveTopology(triangles[i].topology);




        for (int j = 0; j < triangles[i].descriptorTableCount; j++)
        {
            int heapindex = triangles[i].descriptorHeapSelection[j];
            int step = (heapindex == 0 ? mainSRVDescriptorHeap.descriptorHeapHandleSize : mainSamplerDescriptorHeap.descriptorHeapHandleSize);
            CD3DX12_GPU_DESCRIPTOR_HANDLE handle = CD3DX12_GPU_DESCRIPTOR_HANDLE(triangles[i].descriptorHeap[heapindex]->GetGPUDescriptorHandleForHeapStart(), triangles[i].descriptorHeapPointer[j] + (currentFrame * triangles[i].resourceCount[j]), step);
            gCommandBuffer->SetGraphicsRootDescriptorTable(j, handle);
        }


        if (triangles[i].vertexBuffer != ~0ui64)
        {
            D3D12_VERTEX_BUFFER_VIEW vertexView{};

            ID3D12Resource* vertexBuffer = (ID3D12Resource*)GetAndValidateItem(triangles[i].vertexBuffer, D12RESOURCEVIEW);

            vertexView.BufferLocation = vertexBuffer->GetGPUVirtualAddress() + triangles[i].vertexBufferOffset;
            vertexView.SizeInBytes = triangles[i].vertexBufferSize;
            vertexView.StrideInBytes = triangles[i].vertexSize;

            gCommandBuffer->IASetVertexBuffers(0, 1, &vertexView);
        }

        if (triangles[i].indexBuffer != ~0ui64)
        {
        
            D3D12_INDEX_BUFFER_VIEW indexView{};

            ID3D12Resource* indexBuffer = (ID3D12Resource*)GetAndValidateItem(triangles[i].indexBuffer, D12RESOURCEVIEW);

            indexView.BufferLocation = indexBuffer->GetGPUVirtualAddress() + triangles[i].indexBufferOffset;
            indexView.SizeInBytes = triangles[i].indexBufferSize;
            indexView.Format = (triangles[i].indexSize == 2) ? DXGI_FORMAT_R16_UINT : DXGI_FORMAT_R32_UINT;

            gCommandBuffer->IASetIndexBuffer(&indexView);

            gCommandBuffer->DrawIndexedInstanced(triangles[i].indexCount, triangles[i].instanceCount, 0, 0, 0);

        }
        else 
        {
            gCommandBuffer->DrawInstanced(triangles[i].vertexCount, triangles[i].instanceCount, 0, 0);
        }

        

        //graphicCommandBuffer->SetGraphicsRoot32BitConstants(0, 32, &cam, 0);
        
    }

    gCommandBuffer->EndRenderPass();

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

            gCommandBuffer->Barrier(1, &barrierGroup);

    }

    if (FAILED(gCommandBuffer->Close()))
    {
        printf("Cannot finish recording command buffer\n");
        return -1;
    }

    ID3D12CommandList* const commandLists[] = {
        gCommandBuffer
    };

    lQueueHandle->ExecuteCommandLists(_countof(commandLists), commandLists);

    UINT syncInterval = g_VSync ? 1 : 0;


    UINT presentFlags = g_TearingSupported && !g_VSync ? DXGI_PRESENT_ALLOW_TEARING : 0;


    IDXGISwapChain4* lSwapChain = (IDXGISwapChain4*)GetAndValidateItem(swapChain, DXGISWAPCHAIN);

    if (FAILED(lSwapChain->Present(syncInterval, presentFlags)))
    {
        printf("Cannot present image\n");
        return -1;
    }

    g_FrameFenceValues[currentFrame] = Signal(lQueueHandle, g_Fence, g_FenceValue);

    currentFrame = lSwapChain->GetCurrentBackBufferIndex();

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
 
    dmb->bufferHandle = AllocTypeForEntry(buffer, D12RESOURCEVIEW);

}

void CreateDeviceBuffer(DriverMemoryBuffer* dmb, UINT size, D3D12_RESOURCE_FLAGS flags)
{
    dmb->sizeOfAlloc = size;
    dmb->currentPointer = 0;

    ID3D12Resource* resource = CreateDeviceLocalBuffer(deviceHandle, size, flags);

    dmb->bufferHandle = AllocTypeForEntry(resource, D12RESOURCEVIEW);

    dmb->currentState = dmb->initialState = D3D12_RESOURCE_STATE_GENERIC_READ;
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


ID3D12Resource* CreateCommittedImageResource(ID3D12Device2* device, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension)
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



ID3D12Resource* CreatePlacedImageResource(ID3D12Device2* device, TextureMemoryPool* pool, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension)
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

    D3D12_RESOURCE_ALLOCATION_INFO allocInfo =
        device->GetResourceAllocationInfo(0, 1, &imageDesc);


    UINT64 location = (pool->currentPointer + allocInfo.Alignment-1) & ~(allocInfo.Alignment-1);

    pool->currentPointer += (allocInfo.SizeInBytes + (location - pool->currentPointer));

    ID3D12Heap* heap = (ID3D12Heap*)GetAndValidateItem(pool->heap, D12MEMHEAP);

    ID3D12Resource* imageHandle = nullptr;
    device->CreatePlacedResource(
        heap,
        location,
        &imageDesc,
        D3D12_RESOURCE_STATE_COMMON,
        nullptr,
        IID_PPV_ARGS(&imageHandle)
    );

    return imageHandle;
}

EntryHandle CreatePlacedImageResource(TextureMemoryPool* pool, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension)
{
    ID3D12Resource* resource = CreatePlacedImageResource(deviceHandle, pool, width, height, depth, mips, flags, format, dimension);
    return AllocTypeForEntry(resource, D12RESOURCEVIEW);
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
    ID3D12GraphicsCommandList7* transCommandBuffer = (ID3D12GraphicsCommandList7*)GetAndValidateItem(transferCommandBuffer, D12COMMANDBUFFER7);

	void* mappedData = nullptr;

	Allocation* alloc = &allocationHandle[allocationIndex];

    size_t stride = (alloc->requestedsize + alloc->alignment - 1) & ~(alloc->alignment - 1);

    size_t allocLoc = AllocFromDriverMemoryBuffer(&stagingBuffers[currentFrame], stride*copies, alloc->alignment);

    ID3D12Resource* stagingBuffer = (ID3D12Resource*)GetAndValidateItem(stagingBuffers[currentFrame].bufferHandle, D12RESOURCEVIEW);

    ID3D12Resource* dstBuffer = (ID3D12Resource*)GetAndValidateItem(alloc->bufferHandle, D12RESOURCEVIEW);

	stagingBuffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData)+allocLoc;

	for (int i = 0; i < copies; i++)
	{
		memcpy((void*)(cdata), data, size);
		cdata += stride;
	}
	stagingBuffer->Unmap(0, nullptr);

	transCommandBuffer->CopyBufferRegion(dstBuffer, alloc->offset + offset, stagingBuffer, allocLoc, stride * copies);

	transferCommandsUploaded++;
}

void WriteToImageDeviceLocalMemory(ID3D12Resource* imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers)
{
    void* mappedData = nullptr;

    size_t stride = ((width * componentCount) + (255)) & ~255;

    size_t allocLoc = AllocFromDriverMemoryBuffer(&stagingBuffers[currentFrame], stride*height, 255);

    ID3D12Resource* stagingBuffer = (ID3D12Resource*)GetAndValidateItem(stagingBuffers[currentFrame].bufferHandle, D12RESOURCEVIEW);

    stagingBuffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData)+allocLoc;

    for (int i = 0; i < height; i++)
    {
        memcpy((void*)(cdata), data + (i * width * componentCount), width * componentCount);
        cdata += stride;
    }


    stagingBuffer->Unmap(0, nullptr);

    ID3D12GraphicsCommandList7* transCommandBuffer = (ID3D12GraphicsCommandList7*)GetAndValidateItem(transferCommandBuffer, D12COMMANDBUFFER7);
    
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

    transCommandBuffer->Barrier(1, &barrierGroup);

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

    transCommandBuffer->CopyTextureRegion(&dest, 0, 0, 0, &src, NULL);

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

    transCommandBuffer->Barrier(1, &barrierGroup);

    transferCommandsUploaded++;
}

void CreateImageSampler(ID3D12Device2* device, DescriptorHeap* samplerDescriptorHeap)
{

    ID3D12DescriptorHeap* sampDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(samplerDescriptorHeap->descriptorHeap, D12DESCRIPTORHEAP);

    CD3DX12_CPU_DESCRIPTOR_HANDLE samplerHandle(sampDescriptor->GetCPUDescriptorHandleForHeapStart(), samplerDescriptorHeap->descriptorHeapHandlePointer, samplerDescriptorHeap->descriptorHeapHandleSize);


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

    ID3D12Resource* buffer = (ID3D12Resource*)GetAndValidateItem(alloc->bufferHandle, D12RESOURCEVIEW);


    buffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData);

    size_t stride = (alloc->requestedsize + alloc->alignment - 1) & ~(alloc->alignment - 1);

    for (int i = 0; i < copies; i++)
    {
        memcpy((void*)(cdata+alloc->offset+offset), data, size);
        offset += stride;
    }


    buffer->Unmap(0, nullptr);

}

void CreateImageSRVDescriptorHandle(ID3D12Device2* device, ID3D12Resource* bufferHandle, UINT mipsLevels, DXGI_FORMAT format, DescriptorHeap* heap, D3D12_SRV_DIMENSION dimension)
{
    ID3D12DescriptorHeap* srvDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(heap->descriptorHeap, D12DESCRIPTORHEAP);

    CD3DX12_CPU_DESCRIPTOR_HANDLE srvHandle(srvDescriptor->GetCPUDescriptorHandleForHeapStart(), heap->descriptorHeapHandlePointer, heap->descriptorHeapHandleSize);

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


    ID3D12DescriptorHeap* srvDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(heap->descriptorHeap, D12DESCRIPTORHEAP);

    CD3DX12_CPU_DESCRIPTOR_HANDLE srvHandle(srvDescriptor->GetCPUDescriptorHandleForHeapStart(), heap->descriptorHeapHandlePointer, heap->descriptorHeapHandleSize);

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


    ID3D12DescriptorHeap* uavDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(heap->descriptorHeap, D12DESCRIPTORHEAP);

    CD3DX12_CPU_DESCRIPTOR_HANDLE uavHandle(uavDescriptor->GetCPUDescriptorHandleForHeapStart(), heap->descriptorHeapHandlePointer, heap->descriptorHeapHandleSize);


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
    ID3D12DescriptorHeap* cbvDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(heap->descriptorHeap, D12DESCRIPTORHEAP);

    CD3DX12_CPU_DESCRIPTOR_HANDLE cbvHandle(cbvDescriptor->GetCPUDescriptorHandleForHeapStart(), heap->descriptorHeapHandlePointer, heap->descriptorHeapHandleSize);


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

EntryHandle CreateRootSignature(CD3DX12_ROOT_PARAMETER* rootParameters, UINT parameterCount, D3D12_ROOT_SIGNATURE_FLAGS flags)
{
    ID3D12RootSignature* rootSignature = CreateRootSignature(deviceHandle, rootParameters, parameterCount, flags);

    return AllocTypeForEntry(rootSignature, D12ROOTSIGNATURE);
}

EntryHandle CreateRootSignatureFromShaderGraph(ShaderGraph* graph)
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
        case ShaderResourceType::IMAGE2D:

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
        case ShaderResourceType::IMAGE2D:
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
   
    EntryHandle rootSignature = CreateRootSignature(rootParameters, numRootParameters, D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT);

    return rootSignature;
}

void CreateDescriptorHeapManager(DescriptorHeap* heap, UINT maxDescriptorHandles, D3D12_DESCRIPTOR_HEAP_TYPE type, D3D12_DESCRIPTOR_HEAP_FLAGS flags)
{
    heap->descriptorHeap = CreateDescriptorHeap(type, maxDescriptorHandles, flags, &heap->descriptorHeapHandleSize);
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

                ID3D12Resource* constant = (ID3D12Resource*)GetAndValidateItem(alloc->bufferHandle, D12RESOURCEVIEW);

                CreateCBVDescriptorHandle(deviceHandle, constant, i * alloc->stridesize + alloc->offset, alloc->stridesize , heap);

                ptr += sizeof(DescriptorTypeConstantBuffer);
                break;
            }
            case UNIFORMBUFFER:
            {
                DescriptorTypeUniformBuffer* ubType = (DescriptorTypeUniformBuffer*)header;

                Allocation* alloc = &allocationHandle[ubType->allocationIndex];

                ID3D12Resource* ubv = (ID3D12Resource*)GetAndValidateItem(alloc->bufferHandle, D12RESOURCEVIEW);

                CreateSRVDescriptorHandle(deviceHandle,
                    ubv, i * alloc->stridesize + alloc->offset,
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

                
                ID3D12Resource* image = (ID3D12Resource*)GetAndValidateItem(ubType->image, D12RESOURCEVIEW);

                CreateImageSRVDescriptorHandle(deviceHandle,
                    image,
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

    int descriptorTableCount = numDescriptorSet;

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
            case ShaderResourceType::IMAGE2D:
            {
                ShaderResourceImage* image = (ShaderResourceImage*)header;
                DescriptorTypeImageSRV* srv = (DescriptorTypeImageSRV*)dx12Ptr;
                srv->type = IMAGESRV;
                srv->format = DXGI_FORMAT_B8G8R8A8_UNORM_SRGB;
                srv->image = image->textureHandle;

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
        descriptorTableCount++;
    }

    obj->descriptorTableCount = descriptorTableCount;
}

ID3D12Heap* CreateDX12Heap(SIZE_T size, SIZE_T alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType)
{

    D3D12_HEAP_DESC heapDesc = {};
    heapDesc.SizeInBytes = size;
    heapDesc.Alignment = alignment; //D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT;
    heapDesc.Flags = heapFlags;//D3D12_HEAP_FLAG_ALLOW_ONLY_BUFFERS; // or TEXTURES
    heapDesc.Properties.Type = heapType;
    heapDesc.Properties.CPUPageProperty = D3D12_CPU_PAGE_PROPERTY_UNKNOWN;
    heapDesc.Properties.MemoryPoolPreference = D3D12_MEMORY_POOL_UNKNOWN;
    heapDesc.Properties.CreationNodeMask = 1;
    heapDesc.Properties.VisibleNodeMask = 1;

    ID3D12Heap* heap;

    deviceHandle->CreateHeap(&heapDesc, IID_PPV_ARGS(&heap));

    return heap;
}

void CreateTextureMemoryPool(TextureMemoryPool* pool, size_t sizeOfPool, size_t alignment)
{

    alignment = (alignment + D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT - 1) & ~(D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT - 1);

    ID3D12Heap* heap = CreateDX12Heap(sizeOfPool, alignment, D3D12_HEAP_FLAG_ALLOW_ONLY_NON_RT_DS_TEXTURES, D3D12_HEAP_TYPE_DEFAULT);

    pool->heap = AllocTypeForEntry(heap, D12MEMHEAP);
    pool->sizeOfHeap = sizeOfPool;
    pool->currentPointer = 0ui64;
    pool->alignment = alignment;
}


void CreateDSVRSVMemoryPool(TextureMemoryPool* pool, size_t sizeOfPool, size_t alignment, bool msaa)
{
    alignment = (alignment + D3D12_DEFAULT_MSAA_RESOURCE_PLACEMENT_ALIGNMENT - 1) & ~(D3D12_DEFAULT_MSAA_RESOURCE_PLACEMENT_ALIGNMENT - 1);

    ID3D12Heap* heap = CreateDX12Heap(sizeOfPool, alignment, D3D12_HEAP_FLAG_ALLOW_ONLY_RT_DS_TEXTURES, D3D12_HEAP_TYPE_DEFAULT);

    pool->heap = AllocTypeForEntry(heap, D12MEMHEAP);
    pool->sizeOfHeap = sizeOfPool;
    pool->currentPointer = 0ui64;
    pool->alignment = alignment;
}

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
