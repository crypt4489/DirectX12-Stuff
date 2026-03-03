#define WIN32_LEAN_AND_MEAN

#include <Windows.h>
#include "DX12Device.h"
#include "Types.h"
#include "Files.h"

using namespace DirectX;

HINSTANCE hInst;
HWND hwnd;

bool shouldClose = false; 
LRESULT CALLBACK winproc(HWND hwnd, UINT wm, WPARAM wp, LPARAM lp);
int CreateDX12Window(int requestedDimensionX, int requestDimensionY);
int PollDX12WindowEvents();

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




DXGI_FORMAT ConvertImageFormatToDXGIFormat(ImageFormat format);
D3D12_CULL_MODE ConvertCullModeToD3D12CullMode(CullMode mode);
D3D12_DEPTH_STENCILOP_DESC ConvertFaceStencilDataToD3D12StencilDesc(const FaceStencilData& data);
D3D12_COMPARISON_FUNC ConvertRasterizerTestToD3D12CompareFunc(RasterizerTest testApp);
D3D12_STENCIL_OP ConvertStencilToD3D12StencilOp(StencilOp op);
const char* ConvertVertexUsageToSemanticName(VertexUsage usage, UINT* sematicIndex);
DXGI_FORMAT ConvertComponentFormatToDXGIFormat(ComponentFormatType format);
D3D12_INPUT_CLASSIFICATION ConvertVertexRateToD3D12InputClass(VertexBufferRate rate);
void ConvertVertexLayoutToD3D12InputDesc(
    VertexInputDescription* inputVertexDesc,
    D3D12_INPUT_ELEMENT_DESC* desc,
    D3D12_INPUT_CLASSIFICATION inputRate,
    UINT instanceUseRate, UINT vertexbufferid
);
D3D12_PRIMITIVE_TOPOLOGY_TYPE ConvertPrimitiveTypeToD3D12TopologyType(PrimitiveType type);
D3D12_PRIMITIVE_TOPOLOGY ConvertPrimitiveTypeToD3D12Topology(PrimitiveType type);


const char* ConvertVertexUsageToSemanticName(VertexUsage usage, UINT* sematicIndex)
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

DXGI_FORMAT ConvertComponentFormatToDXGIFormat(ComponentFormatType format)
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


D3D12_INPUT_CLASSIFICATION ConvertVertexRateToD3D12InputClass(VertexBufferRate rate)
{
    D3D12_INPUT_CLASSIFICATION classification = D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA;

    switch (rate)
    {
    case VertexBufferRate::PERVERTEX:
        break;
    case VertexBufferRate::PERINSTANCE:
        classification = D3D12_INPUT_CLASSIFICATION_PER_INSTANCE_DATA;
        break;
    default:
        break;
    }

    return classification;
}


void ConvertVertexLayoutToD3D12InputDesc(VertexInputDescription* inputVertexDesc, D3D12_INPUT_ELEMENT_DESC* desc, D3D12_INPUT_CLASSIFICATION inputRate, UINT instanceUseRate, UINT vertexbufferid)
{
    DXGI_FORMAT currFormat = ConvertComponentFormatToDXGIFormat(inputVertexDesc->format);

    desc->Format = currFormat;
    desc->InputSlotClass = inputRate;
    desc->SemanticName = ConvertVertexUsageToSemanticName(inputVertexDesc->vertexusage, &desc->SemanticIndex);
    desc->InputSlot = vertexbufferid;
    desc->AlignedByteOffset = inputVertexDesc->byteoffset;
    desc->InstanceDataStepRate = instanceUseRate;
}

D3D12_CULL_MODE ConvertCullModeToD3D12CullMode(CullMode mode)
{
    D3D12_CULL_MODE cullMode = D3D12_CULL_MODE_NONE;

    switch (mode)
    {
    case CullMode::CULL_NONE:
        cullMode = D3D12_CULL_MODE_NONE;
        break;

    case CullMode::CULL_BACK:
        cullMode = D3D12_CULL_MODE_BACK;
        break;

    case CullMode::CULL_FRONT:
        cullMode = D3D12_CULL_MODE_FRONT;
        break;

    default:
        cullMode = D3D12_CULL_MODE_NONE;
        break;
    }

    return cullMode;
}

DXGI_FORMAT ConvertImageFormatToDXGIFormat(ImageFormat format)
{
    DXGI_FORMAT dxgiFormat = DXGI_FORMAT_UNKNOWN;

    switch (format)
    {
    case X8L8U8V8:
        // No direct DXGI equivalent
        dxgiFormat = DXGI_FORMAT_UNKNOWN;
        break;

    case DXT1:
        dxgiFormat = DXGI_FORMAT_BC1_UNORM;
        break;

    case DXT3:
        dxgiFormat = DXGI_FORMAT_BC2_UNORM;
        break;

    case R8G8B8A8:
        dxgiFormat = DXGI_FORMAT_R8G8B8A8_UNORM;
        break;

    case B8G8R8A8:
        dxgiFormat = DXGI_FORMAT_B8G8R8A8_UNORM;
        break;

    case D24UNORMS8STENCIL:
        dxgiFormat = DXGI_FORMAT_D24_UNORM_S8_UINT;
        break;

    case D32FLOATS8STENCIL:
        dxgiFormat = DXGI_FORMAT_D32_FLOAT_S8X24_UINT;
        break;

    case D32FLOAT:
        dxgiFormat = DXGI_FORMAT_D32_FLOAT;
        break;

    case R8G8B8A8_UNORM:
        dxgiFormat = DXGI_FORMAT_R8G8B8A8_UNORM;
        break;

    case R8G8B8:
        // No 24-bit RGB format in DXGI
        dxgiFormat = DXGI_FORMAT_UNKNOWN;
        break;

    case B8G8R8A8_UNORM:
        dxgiFormat = DXGI_FORMAT_B8G8R8A8_UNORM;
        break;

    case IMAGE_UNKNOWN:
    default:
        dxgiFormat = DXGI_FORMAT_UNKNOWN;
        break;
    }

    return dxgiFormat;
}

D3D12_COMPARISON_FUNC ConvertRasterizerTestToD3D12CompareFunc(RasterizerTest testApp)
{
    D3D12_COMPARISON_FUNC ret = D3D12_COMPARISON_FUNC_ALWAYS;

    switch (testApp)
    {
    case RasterizerTest::NEVER:
        ret = D3D12_COMPARISON_FUNC_NEVER;
        break;

    case RasterizerTest::LESS:
        ret = D3D12_COMPARISON_FUNC_LESS;
        break;

    case RasterizerTest::EQUAL:
        ret = D3D12_COMPARISON_FUNC_EQUAL;
        break;

    case RasterizerTest::LESSEQUAL:
        ret = D3D12_COMPARISON_FUNC_LESS_EQUAL;
        break;

    case RasterizerTest::GREATER:
        ret = D3D12_COMPARISON_FUNC_GREATER;
        break;

    case RasterizerTest::NOTEQUAL:
        ret = D3D12_COMPARISON_FUNC_NOT_EQUAL;
        break;

    case RasterizerTest::GREATEREQUAL:
        ret = D3D12_COMPARISON_FUNC_GREATER_EQUAL;
        break;

    case RasterizerTest::ALLPASS:
        ret = D3D12_COMPARISON_FUNC_ALWAYS;
        break;

    default:
        break;
    }

    return ret;
}

D3D12_STENCIL_OP ConvertStencilToD3D12StencilOp(StencilOp op)
{
    D3D12_STENCIL_OP result = D3D12_STENCIL_OP_KEEP;

    switch (op)
    {
    case StencilOp::REPLACE:
        result = D3D12_STENCIL_OP_REPLACE;
        break;

    case StencilOp::KEEP:
        result = D3D12_STENCIL_OP_KEEP;
        break;

    case StencilOp::ZERO:
        result = D3D12_STENCIL_OP_ZERO;
        break;

    default:
        result = D3D12_STENCIL_OP_KEEP;
        break;
    }

    return result;
}

D3D12_DEPTH_STENCILOP_DESC ConvertFaceStencilDataToD3D12StencilDesc(const FaceStencilData& data)
{
    D3D12_DEPTH_STENCILOP_DESC desc = {};

    desc.StencilFailOp = ConvertStencilToD3D12StencilOp(data.failOp);
    desc.StencilPassOp = ConvertStencilToD3D12StencilOp(data.passOp);
    desc.StencilDepthFailOp = ConvertStencilToD3D12StencilOp(data.depthFailOp);

    desc.StencilFunc = ConvertRasterizerTestToD3D12CompareFunc(data.stencilCompare);

    return desc;
}

D3D12_PRIMITIVE_TOPOLOGY ConvertPrimitiveTypeToD3D12Topology(PrimitiveType type)
{
    D3D12_PRIMITIVE_TOPOLOGY topology = D3D_PRIMITIVE_TOPOLOGY_UNDEFINED;

    switch (type)
    {
    case TRIANGLES:
        topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
        break;

    case TRISTRIPS:
        topology = D3D_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP;
        break;

    case POINTSLIST:
        topology = D3D_PRIMITIVE_TOPOLOGY_POINTLIST;
        break;

    case LINELIST:
        topology = D3D_PRIMITIVE_TOPOLOGY_LINELIST;
        break;

    case LINESTRIPS:
        topology = D3D_PRIMITIVE_TOPOLOGY_LINESTRIP;
        break;

    default:
        topology = D3D_PRIMITIVE_TOPOLOGY_UNDEFINED;
        break;
    }

    return topology;
}

D3D12_PRIMITIVE_TOPOLOGY_TYPE ConvertPrimitiveTypeToD3D12TopologyType(PrimitiveType type)
{
    D3D12_PRIMITIVE_TOPOLOGY_TYPE topologyType =
        D3D12_PRIMITIVE_TOPOLOGY_TYPE_UNDEFINED;

    switch (type)
    {
    case TRIANGLES:
    case TRISTRIPS:
    case TRIFAN:
        topologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
        break;

    case LINELIST:
    case LINESTRIPS:
        topologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_LINE;
        break;

    case POINTSLIST:
        topologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_POINT;
        break;

    default:
        topologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_UNDEFINED;
        break;
    }

    return topologyType;
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

void* AllocFromPermanent(size_t size, size_t alignment)
{
    size_t current = storageGlobalAllocator;

    if (current + size >= STORAGE_MEM_SIZE)
        current = 0;

    current = (current + alignment - 1) & ~(alignment - 1);

    storageGlobalAllocator += (size)+(current - storageGlobalAllocator);

    return (void*)&globalMemoryPool[current];
}





DX12Device deviceInstance;

EntryHandle queueHandle;

EntryHandle swapChain;

EntryHandle swapChainImages[MAX_FRAMES_IN_FLIGHT];

EntryHandle swapChainDepthImages[MAX_FRAMES_IN_FLIGHT];

EntryHandle graphicCommandBuffers[MAX_FRAMES_IN_FLIGHT];

EntryHandle graphicCommandPools[MAX_FRAMES_IN_FLIGHT];

EntryHandle transferCommandPool;
EntryHandle transferCommandBuffer;
int transferCommandsUploaded = 0;

EntryHandle globalRTVDescriptorHeap;
EntryHandle globalDSVDescriptorHeap;


UINT globalRTVDescriptorSize;
UINT globalDSVDescriptorSize;
UINT currentFrame = 0;
UINT currentImageIndex = 0;

EntryHandle globalRendererFence, globalTransferFence;

uint64_t g_FrameFenceValues[MAX_FRAMES_IN_FLIGHT] = {};

bool g_VSync = true;

bool g_TearingSupported = false;



ShaderHandles shaderHandles[2]{};

EntryHandle bgraImageMemoryPool;

struct TextureDetails
{
    DXGI_FORMAT type;
    uint32_t dataSize;
    uint32_t width, height, miplevels;
    char* data;
};



EntryHandle bgraPool;

EntryHandle rsvdsvPool;

void ParseBMP(TextureDetails* details, const char* name);

int Render();

void ReleaseD3D12Resources();

void WriteToHostMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies);
void WriteToDeviceLocalMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies);
void WriteToImageDeviceLocalMemory(EntryHandle imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers);


EntryHandle CreateRootSignatureFromShaderGraph(ShaderGraph* graph);
struct Camera
{
    XMMATRIX proj;
    XMMATRIX view;
};

XMMATRIX world[2];

Camera cam;


EntryHandle CreatePipelineStateObject(EntryHandle rootSignature, ShaderHandles* handles, int shaderCount, GenericPipelineStateInfo* stateInfo);

PipelineObject triangles[2];


EntryHandle hostBuffer{};
EntryHandle deviceLocalBuffer{};
EntryHandle stagingBuffers[MAX_FRAMES_IN_FLIGHT]{};




EntryHandle mainSRVDescriptorHeap;
EntryHandle mainSamplerDescriptorHeap;
EntryHandle stagingSRVDescriptorHeap;
EntryHandle stagingSamplerDescriptorHeap;


struct ImageHandle
{
    EntryHandle memHeap;
    EntryHandle imageViewHandle;
};

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

    size_t location = deviceInstance.AllocFromDriverMemoryBuffer(hostBuffer, allocSize, alignment);

    int index = allocationHandleIndex++;

    allocationHandle[index].bufferHandle = hostBuffer;
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

    size_t location = deviceInstance.AllocFromDriverMemoryBuffer(deviceLocalBuffer, allocSize, alignment);

    int index = allocationHandleIndex++;

    allocationHandle[index].bufferHandle = deviceLocalBuffer;
    allocationHandle[index].offset = location;
    allocationHandle[index].totalDeviceAlloc = allocSize;
    allocationHandle[index].requestedsize = size;
    allocationHandle[index].alignment = alignment;
    allocationHandle[index].copies = copies;
    allocationHandle[index].stridesize = (size + alignment - 1) & ~(alignment - 1);

    return index;
}

D3D12ShaderType ConvertShaderStageToD3D12ShaderStage(ShaderStageType type)
{
    D3D12ShaderType sType = VERTEX;
    switch (type)
    {
    case VERTEXSHADERSTAGE:
        sType = VERTEX;
        break;
    case FRAGMENTSHADERSTAGE:
        sType = PIXEL;
        break;
    case COMPUTESHADERSTAGE:
        //sType = ;
        break;
    default:
        break;
    }

    return sType;
}

void DoSceneStuff()
{
    ID3D12CommandAllocator* transCommandPool = (ID3D12CommandAllocator*)deviceInstance.GetAndValidateItem(transferCommandPool, D12COMMANDPOOL);
    
    transCommandPool->Reset();

    ID3D12GraphicsCommandList7* transCommandBuffer = (ID3D12GraphicsCommandList7*)deviceInstance.GetAndValidateItem(transferCommandBuffer, D12COMMANDBUFFER7);

    transCommandBuffer->Reset(transCommandPool, NULL);

    bgraPool = deviceInstance.CreateTextureMemoryPool(256 * MiB, D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT);

    ShaderDetails* sdetails[2];

    char* detailData = (char*)AllocFromTemp(1 * KiB, 4);

    ShaderGraph* mainLayout = CreateShaderGraph("DirectLayout.xml", detailData, sdetails);

    for (int i = 0; i < mainLayout->shaderMapCount; i++)
    {
        ShaderMap* map = (ShaderMap*)mainLayout->GetMap(i);
        shaderHandles[i] = { ConvertShaderStageToD3D12ShaderStage(map->type), deviceInstance.CreateShaderBlob(sdetails[i]->GetString()) };
    }


    triangles[0].rootSignature = CreateRootSignatureFromShaderGraph(mainLayout);//CreateGenericRootSignature();


    GenericPipelineStateInfo pipeInfo{};

    CreatePipelineDescription("GenericPipeline.xml", &pipeInfo);

    pipeInfo.colorFormat = ImageFormat::R8G8B8A8;
    pipeInfo.depthFormat = ImageFormat::D32FLOAT;

    triangles[0].pipelineState = CreatePipelineStateObject(triangles[0].rootSignature, shaderHandles, 2, &pipeInfo);

    TextureDetails details{};

    ParseBMP(&details, "face1.bmp");

    int cameraData = AllocFromHostBuffer(sizeof(Camera), sizeof(Camera), MAX_FRAMES_IN_FLIGHT);
    int world1Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 256, MAX_FRAMES_IN_FLIGHT);
    int world2Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 256, MAX_FRAMES_IN_FLIGHT);
    int vertexOffset = AllocFromDeviceBuffer(sizeof(BoxVerts), 16, 1);
    int indexOffset = AllocFromDeviceBuffer(sizeof(BoxIndices), 16, 1);

    
    DriverMemoryBuffer* buffer = (DriverMemoryBuffer*)deviceInstance.GetAndValidateItem(deviceLocalBuffer, D12BUFFERMEMORYPOOL);

    deviceInstance.TransitionBufferBarrier(transferCommandBuffer, buffer->bufferHandle, D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS, D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST);

    WriteToDeviceLocalMemory(vertexOffset, BoxVerts, sizeof(BoxVerts), 0, 1);

    WriteToDeviceLocalMemory(indexOffset, BoxIndices, sizeof(BoxIndices), 0, 1);

    WriteToHostMemory(cameraData, &cam, sizeof(Camera), 0, MAX_FRAMES_IN_FLIGHT);

    WriteToDeviceLocalMemory(world1Data, world, 64, 0, MAX_FRAMES_IN_FLIGHT);

    WriteToDeviceLocalMemory(world2Data, &world[1], 64, 0, MAX_FRAMES_IN_FLIGHT);

    deviceInstance.TransitionBufferBarrier(transferCommandBuffer, buffer->bufferHandle, D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST, D3D12_BARRIER_SYNC_DRAW | D3D12_BARRIER_SYNC_INDEX_INPUT, D3D12_BARRIER_ACCESS_VERTEX_BUFFER | D3D12_BARRIER_ACCESS_CONSTANT_BUFFER | D3D12_BARRIER_ACCESS_INDEX_BUFFER);

    bgraImageMemoryPool = deviceInstance.CreateImageResourceFromPool(bgraPool, details.width, details.height, 1, details.miplevels, D3D12_RESOURCE_FLAG_NONE, details.type, D3D12_RESOURCE_DIMENSION_TEXTURE2D);

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

    triangles[1].descriptorTableCount++;

    triangles[1].descriptorHeapPointer[triangles[1].descriptorTableCount - 1] =
        triangles[0].descriptorHeapPointer[triangles[0].descriptorTableCount - 1];
    triangles[1].resourceCount[triangles[1].descriptorTableCount - 1] =
        triangles[0].resourceCount[triangles[0].descriptorTableCount - 1];
    triangles[1].descriptorHeapSelection[triangles[1].descriptorTableCount - 1] =
        triangles[0].descriptorHeapSelection[triangles[0].descriptorTableCount - 1];
    
    triangles[0].topology = ConvertPrimitiveTypeToD3D12Topology(pipeInfo.primType);
    triangles[0].heapsCount = 2;
    triangles[0].instanceCount = 1;
    triangles[0].vertexCount = 24;
    triangles[0].indexCount = 36;
    triangles[0].descriptorHeap[0] = mainSRVDescriptorHeap;
    triangles[0].descriptorHeap[1] = mainSamplerDescriptorHeap;


    triangles[1].topology = ConvertPrimitiveTypeToD3D12Topology(pipeInfo.primType);
    triangles[1].heapsCount = 2;
    triangles[1].instanceCount = 1;
    triangles[1].vertexCount = 24;
    triangles[1].indexCount = 36;
    triangles[1].descriptorHeap[0] = mainSRVDescriptorHeap;
    triangles[1].descriptorHeap[1] = mainSamplerDescriptorHeap;

    triangles[1].pipelineState = triangles[0].pipelineState;
    triangles[1].rootSignature = triangles[0].rootSignature;

   

    triangles[0].indexBuffer =  buffer->bufferHandle;
    triangles[1].indexBuffer =  buffer->bufferHandle;
    triangles[0].vertexBuffer = buffer->bufferHandle;
    triangles[1].vertexBuffer = buffer->bufferHandle;
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

    bool debug = false;

#if _DEBUG
    deviceInstance.EnableRuntimeValidation();
    debug = true;
#endif

    deviceInstance.CreateDevice(debug);
  
    queueHandle = deviceInstance.CreateCommandQueue(D3D12_COMMAND_LIST_TYPE_DIRECT);

    swapChain = deviceInstance.CreateSwapChain(
        hwnd,
        queueHandle,
        800,
        600,
        MAX_FRAMES_IN_FLIGHT,
        ConvertImageFormatToDXGIFormat(ImageFormat::R8G8B8A8),
        0
    );

    globalRTVDescriptorHeap = deviceInstance.CreateDescriptorHeap(
        D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
        MAX_FRAMES_IN_FLIGHT,
        D3D12_DESCRIPTOR_HEAP_FLAG_NONE, &globalRTVDescriptorSize
    );

 
    globalDSVDescriptorHeap = deviceInstance.CreateDescriptorHeap(
        D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
        MAX_FRAMES_IN_FLIGHT,
        D3D12_DESCRIPTOR_HEAP_FLAG_NONE, &globalDSVDescriptorSize
    );

    mainSRVDescriptorHeap = deviceInstance.CreateDescriptorHeapManager(MAX_FRAMES_IN_FLIGHT * 100, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE);

    mainSamplerDescriptorHeap = deviceInstance.CreateDescriptorHeapManager(MAX_FRAMES_IN_FLIGHT * 100, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER, D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE);

    stagingSRVDescriptorHeap = deviceInstance.CreateDescriptorHeapManager(MAX_FRAMES_IN_FLIGHT * 50, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, D3D12_DESCRIPTOR_HEAP_FLAG_NONE);

    stagingSamplerDescriptorHeap = deviceInstance.CreateDescriptorHeapManager(MAX_FRAMES_IN_FLIGHT * 50, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER, D3D12_DESCRIPTOR_HEAP_FLAG_NONE);

    deviceInstance.CreateRenderTargetView(swapChain, globalRTVDescriptorHeap, swapChainImages, MAX_FRAMES_IN_FLIGHT);

    rsvdsvPool = deviceInstance.CreateDSVRSVMemoryPool(24 * MiB, (1<<22), false);

    deviceInstance.CreateDepthStencilView(
        globalDSVDescriptorHeap,
        rsvdsvPool,
        swapChainDepthImages, 
        MAX_FRAMES_IN_FLIGHT, 
        800, 600, 
        DXGI_FORMAT_D32_FLOAT, 
        1
    );

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        graphicCommandPools[i] =
            deviceInstance.CreateCommandAllocator( D3D12_COMMAND_LIST_TYPE_DIRECT);
    }

    globalRendererFence = deviceInstance.CreateFenceObject();

    globalTransferFence = deviceInstance.CreateFenceObject();

    transferCommandPool = deviceInstance.CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT);

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        graphicCommandBuffers[i] = deviceInstance.CreateCommandList(
            graphicCommandPools[i],
            D3D12_COMMAND_LIST_TYPE_DIRECT
        );
    }
    

    transferCommandBuffer = deviceInstance.CreateCommandList(
        transferCommandPool,
        D3D12_COMMAND_LIST_TYPE_DIRECT
    );


    world[0] = XMMatrixIdentity();
    world[1] = XMMatrixTranslation(2.0, 1.0, -2.0);
   
    hostBuffer = deviceInstance.CreateHostBuffer(8 * KiB, DXGI_FORMAT_UNKNOWN, D3D12_RESOURCE_FLAG_NONE);

    deviceLocalBuffer = deviceInstance.CreateDeviceBuffer(8 * KiB, DXGI_FORMAT_UNKNOWN, D3D12_RESOURCE_FLAG_NONE);

    for (UINT i = 0; i < MAX_FRAMES_IN_FLIGHT; i++)
    {
        stagingBuffers[i] = deviceInstance.CreateHostBuffer(64*MiB, DXGI_FORMAT_UNKNOWN, D3D12_RESOURCE_FLAG_NONE);
    }
    
    DoSceneStuff();   
    g_IsInitialized = true;

    while (!shouldClose)
    {
        PollDX12WindowEvents();
        int ret = Render();
        if (ret < 0) break;
    }

    deviceInstance.Flush( globalRendererFence, queueHandle);

    ReleaseD3D12Resources();

    CloseWindow(hwnd);

    return 0;
}



void ReleaseD3D12Resources()
{
    deviceInstance.ReleaseAllDriverCOMHandles();

  
    if (deviceInstance.deviceHandle)
    {
        deviceInstance.deviceHandle->Release();
        deviceInstance.deviceHandle = nullptr;
    }

#ifdef _DEBUG
    IDXGIDebug1* dxgiDebug = nullptr;
    DXGIGetDebugInterface1(0, IID_PPV_ARGS(&dxgiDebug));

    dxgiDebug->ReportLiveObjects(
        DXGI_DEBUG_ALL,
        DXGI_DEBUG_RLO_ALL
    );

    dxgiDebug->Release();
#endif
}





EntryHandle CreatePipelineStateObject(EntryHandle rootSignature, ShaderHandles* handles, int shaderCount, GenericPipelineStateInfo* stateInfo)
{

    DX12PipelineStateObjectCreate createInfo{};

    for (int i = 0; i < shaderCount; i++)
    {

        ID3DBlob* blob = (ID3DBlob*)deviceInstance.GetAndValidateItem(handles[i].shader, D12SHADERBLOB);

        SIZE_T bcLen = blob->GetBufferSize();
        void* shaderData = blob->GetBufferPointer();
        switch (handles[i].type)
        {
        case VERTEX:
            createInfo.desc.VS.BytecodeLength = bcLen;
            createInfo.desc.VS.pShaderBytecode = shaderData;
            break;
        case PIXEL:
            createInfo.desc.PS.BytecodeLength = bcLen;
            createInfo.desc.PS.pShaderBytecode = shaderData;
            break;
        }
    }

    int inputCount = stateInfo->vertexBufferDescCount;

    UINT totalAttributeCount = 0;

    for (int i = 0; i < inputCount; i++)
    {
        VertexBufferDescription* buffers = &stateInfo->vertexBufferDesc[i];

        int attributeCount = buffers->descCount;

        totalAttributeCount += attributeCount;
    }

    D3D12_INPUT_ELEMENT_DESC* attributeDesc = (D3D12_INPUT_ELEMENT_DESC*)AllocFromTemp(sizeof(D3D12_INPUT_ELEMENT_DESC) * totalAttributeCount, 4);

    int attributeCounter = 0;

    for (int i = 0; i < inputCount; i++)
    {

        VertexBufferDescription* buffers = &stateInfo->vertexBufferDesc[i];

        int attributeCount = buffers->descCount;

        for (int j = 0; j < attributeCount; j++)
        {
            ConvertVertexLayoutToD3D12InputDesc(&buffers->descriptions[j], &attributeDesc[attributeCounter + j], ConvertVertexRateToD3D12InputClass(buffers->rate), 0, i);
        }

        attributeCounter += attributeCount;
    }

    createInfo.SetInputLayout(attributeDesc, totalAttributeCount, ConvertPrimitiveTypeToD3D12TopologyType(stateInfo->primType));

    createInfo.SetNumRenderTargets(1);

    createInfo.SetSlotRenderTarget(0, ConvertImageFormatToDXGIFormat(stateInfo->colorFormat));

    createInfo.SetDepthFormat(ConvertImageFormatToDXGIFormat(stateInfo->depthFormat));

    createInfo.SetRasterizerState(ConvertCullModeToD3D12CullMode(stateInfo->cullMode), (stateInfo->windingOrder == TriangleWinding::CCW ? TRUE : FALSE));

    createInfo.SetBlendState();

    createInfo.SetSampleDesc(UINT_MAX, 1, 0);
  
    if (stateInfo->frontFace.compareMask != stateInfo->backFace.compareMask || stateInfo->frontFace.writeMask != stateInfo->backFace.writeMask)
    {
        //handle unaligned 
    }

    D3D12_DEPTH_STENCILOP_DESC BackFace = ConvertFaceStencilDataToD3D12StencilDesc(stateInfo->backFace);
    D3D12_DEPTH_STENCILOP_DESC FrontFace = ConvertFaceStencilDataToD3D12StencilDesc(stateInfo->frontFace);

    createInfo.SetDepthStencilState(stateInfo->depthEnable, stateInfo->StencilEnable, ConvertRasterizerTestToD3D12CompareFunc(stateInfo->depthTest),
        (stateInfo->depthWrite ? D3D12_DEPTH_WRITE_MASK_ALL : D3D12_DEPTH_WRITE_MASK_ZERO), &FrontFace, &BackFace, stateInfo->frontFace.compareMask, stateInfo->frontFace.writeMask);

   
    return deviceInstance.CreatePipelineStateObject(rootSignature, &createInfo);
}



void ResetTransferPool()
{
    ID3D12GraphicsCommandList7* transCommandBuffer = (ID3D12GraphicsCommandList7*)deviceInstance.GetAndValidateItem(transferCommandBuffer, D12COMMANDBUFFER7);

    ID3D12CommandAllocator* transCommandPool = (ID3D12CommandAllocator*)deviceInstance.GetAndValidateItem(transferCommandPool, D12COMMANDPOOL);

    transCommandPool->Reset();

    transCommandBuffer->Reset(transCommandPool, NULL);

    transferCommandsUploaded = 0;
}

int Render()
{

    auto commandAllocator = graphicCommandPools[currentFrame];

    auto graphicCommandBuffer = graphicCommandBuffers[currentFrame];

    auto backBuffer = swapChainImages[currentImageIndex];

    auto depthBuffer = swapChainDepthImages[currentImageIndex];

    auto backBufferResource = (ID3D12Resource*)deviceInstance.GetAndValidateItem(backBuffer, D12RESOURCEHANDLE);

    ID3D12GraphicsCommandList7* gCommandBuffer = (ID3D12GraphicsCommandList7*)deviceInstance.GetAndValidateItem(graphicCommandBuffer, D12COMMANDBUFFER7);

    ID3D12CommandAllocator* commandPool = (ID3D12CommandAllocator*)deviceInstance.GetAndValidateItem(commandAllocator, D12COMMANDPOOL);

    ID3D12CommandList* commandLists[2];

    UINT cmdListIndex = 0;

    if (transferCommandsUploaded)
    {

        ID3D12GraphicsCommandList7* transCommandBuffer = (ID3D12GraphicsCommandList7*)deviceInstance.GetAndValidateItem(transferCommandBuffer, D12COMMANDBUFFER7);

        transCommandBuffer->Close();

        commandLists[cmdListIndex++] = transCommandBuffer;

        transferCommandsUploaded = 0;
    }

    commandPool->Reset();

    gCommandBuffer->Reset(commandPool, nullptr);


    ID3D12DescriptorHeap* dsvDescriptor = (ID3D12DescriptorHeap*)deviceInstance.GetAndValidateItem(globalDSVDescriptorHeap, D12DESCRIPTORHEAP);
    ID3D12DescriptorHeap* rsvDescriptor = (ID3D12DescriptorHeap*)deviceInstance.GetAndValidateItem(globalRTVDescriptorHeap, D12DESCRIPTORHEAP);

    DX12CPUDescriptorHandle dsvHandle(dsvDescriptor->GetCPUDescriptorHandleForHeapStart(), currentFrame, globalDSVDescriptorSize);

    DX12CPUDescriptorHandle rtvHandle(rsvDescriptor->GetCPUDescriptorHandleForHeapStart(), currentFrame, globalRTVDescriptorSize);

    
    {
        deviceInstance.TransitionImageResource(gCommandBuffer, backBufferResource,
            D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS,
            D3D12_BARRIER_SYNC_RENDER_TARGET, D3D12_BARRIER_ACCESS_RENDER_TARGET,
            D3D12_BARRIER_LAYOUT_PRESENT, D3D12_BARRIER_LAYOUT_RENDER_TARGET,
            0, 1, 0, 1
        );
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

        ID3D12RootSignature* sign = (ID3D12RootSignature*)deviceInstance.GetAndValidateItem(triangles[i].rootSignature, D12ROOTSIGNATURE);

        gCommandBuffer->SetGraphicsRootSignature(sign);


        ID3D12DescriptorHeap** heaps = (ID3D12DescriptorHeap**)AllocFromTemp(sizeof(ID3D12DescriptorHeap*) * triangles[i].heapsCount, 4);
        
        UINT* heapSizes = (UINT*)AllocFromTemp(sizeof(UINT) * triangles[i].heapsCount, 4);

        for (int j = 0; j < triangles[i].heapsCount; j++)
        {
            heaps[j] = (ID3D12DescriptorHeap*)deviceInstance.GetDescriptorHeapFromManager(triangles[i].descriptorHeap[j]);
            heapSizes[j] = deviceInstance.GetDescriptorHeapSizeFromManager(triangles[i].descriptorHeap[j]);
        }

        gCommandBuffer->SetDescriptorHeaps(triangles[i].heapsCount, heaps);

        ID3D12PipelineState* pipelineState = (ID3D12PipelineState*)deviceInstance.GetAndValidateItem(triangles[i].pipelineState, D12PIPELINESTATE);

        gCommandBuffer->SetPipelineState(pipelineState);
        gCommandBuffer->IASetPrimitiveTopology(triangles[i].topology);

        for (int j = 0; j < triangles[i].descriptorTableCount; j++)
        {
            int heapindex = triangles[i].descriptorHeapSelection[j];
            DX12GPUDescriptorHandle handle = DX12GPUDescriptorHandle(heaps[heapindex]->GetGPUDescriptorHandleForHeapStart(), triangles[i].descriptorHeapPointer[j] + (currentFrame * triangles[i].resourceCount[j]), heapSizes[heapindex]);
            gCommandBuffer->SetGraphicsRootDescriptorTable(j, handle);
        }


        if (triangles[i].vertexBuffer != ~0ui64)
        {
            D3D12_VERTEX_BUFFER_VIEW vertexView{};

            ID3D12Resource* vertexBuffer = (ID3D12Resource*)deviceInstance.GetAndValidateItem(triangles[i].vertexBuffer, D12RESOURCEHANDLE);

            vertexView.BufferLocation = vertexBuffer->GetGPUVirtualAddress() + triangles[i].vertexBufferOffset;
            vertexView.SizeInBytes = (UINT)triangles[i].vertexBufferSize;
            vertexView.StrideInBytes = triangles[i].vertexSize;

            gCommandBuffer->IASetVertexBuffers(0, 1, &vertexView);
        }

        if (triangles[i].indexBuffer != ~0ui64)
        {
        
            D3D12_INDEX_BUFFER_VIEW indexView{};

            ID3D12Resource* indexBuffer = (ID3D12Resource*)deviceInstance.GetAndValidateItem(triangles[i].indexBuffer, D12RESOURCEHANDLE);

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
            deviceInstance.TransitionImageResource(gCommandBuffer, backBufferResource,
            
                D3D12_BARRIER_SYNC_RENDER_TARGET, D3D12_BARRIER_ACCESS_RENDER_TARGET,
                D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS,
                 D3D12_BARRIER_LAYOUT_RENDER_TARGET, D3D12_BARRIER_LAYOUT_PRESENT,
                0, 1, 0, 1
            );
    }

    if (FAILED(gCommandBuffer->Close()))
    {
        printf("Cannot finish recording command buffer\n");
        return -1;
    }


    commandLists[cmdListIndex++] = gCommandBuffer;

    deviceInstance.ExecuteCommandListsOnQueue(queueHandle, commandLists, cmdListIndex);


    UINT syncInterval = g_VSync ? 1 : 0;


    UINT presentFlags = g_TearingSupported && !g_VSync ? DXGI_PRESENT_ALLOW_TEARING : 0;


    IDXGISwapChain4* lSwapChain = (IDXGISwapChain4*)deviceInstance.GetAndValidateItem(swapChain, DXGISWAPCHAIN);

    if (FAILED(lSwapChain->Present(syncInterval, presentFlags)))
    {
        printf("Cannot present image\n");
        return -1;
    }

    g_FrameFenceValues[currentFrame] = deviceInstance.Signal(queueHandle, globalRendererFence);

    currentImageIndex = lSwapChain->GetCurrentBackBufferIndex();

    currentFrame = (currentFrame + 1) % MAX_FRAMES_IN_FLIGHT;

    deviceInstance.WaitForFenceValue(globalRendererFence, g_FrameFenceValues[currentFrame], UINT32_MAX);

    return 0;
}

void WriteToDeviceLocalMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies)
{
    
	Allocation* alloc = &allocationHandle[allocationIndex];

    size_t stride = (alloc->requestedsize + alloc->alignment - 1) & ~(alloc->alignment - 1);

    deviceInstance.WriteToDeviceLocalMemory(alloc->bufferHandle, stagingBuffers[currentFrame], transferCommandBuffer, data, size, alloc->offset + offset, stride, copies);

	transferCommandsUploaded++;
}

void WriteToImageDeviceLocalMemory(EntryHandle imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers)
{
    deviceInstance.WriteToImageDeviceLocalMemory(imageHandle, transferCommandBuffer, stagingBuffers[currentFrame],
        data, width, height, componentCount, totalImageSize, format, mipLevels, layers
    );

    transferCommandsUploaded++;
}

void WriteToHostMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies)
{
    void* mappedData = nullptr;

    Allocation* alloc = &allocationHandle[allocationIndex];

    size_t stride = (alloc->requestedsize + alloc->alignment - 1) & ~(alloc->alignment - 1);

    deviceInstance.WriteToHostMemory(alloc->bufferHandle, data, size, alloc->offset + offset, stride, copies);
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
    
    DX12DescriptorTableRanges ranges{};

    ranges.numOfRanges = numOfRanges;
    ranges.ranges = (D3D12_DESCRIPTOR_RANGE*)AllocFromTemp(sizeof(D3D12_DESCRIPTOR_RANGE) * numOfRanges, 4);

    ShaderStageType* visibility = (ShaderStageType*)AllocFromTemp(sizeof(ShaderStageType) * numRootParameters, 4);

    UINT samplerIndex = 0, srvIndex = 0, cbvIndex = 0, samplerRangeParameterIndex = numOfRanges-samplerCount;


    for (int i = 0; i < graph->resourceCount; i++)
    {
        ShaderResource* resource = (ShaderResource*)graph->GetResource(i);

        switch (resource->type)
        {
        case ShaderResourceType::CONSTANT_BUFFER:
        {
            ranges.AppendCBVRange(1, cbvIndex++, 0);
            visibility[resource->set] |= resource->stages;
            rangeCount[resource->set] += 1;
            break;
        }
        case ShaderResourceType::IMAGE2D:
        {
            ranges.AppendSRVRange(1, srvIndex++, 0);
            rangeCount[resource->set] += 1;
            visibility[resource->set] |= resource->stages;
            break;
        }
        case ShaderResourceType::SAMPLERSTATE:
        {
            ranges.CreateSamplerRange(samplerRangeParameterIndex++, 1, samplerIndex++, 0);
            visibility[numRootParameters - 1] |= resource->stages;
            rangeCount[numRootParameters - 1] += 1;
            break;
        }
        case ShaderResourceType::UNIFORM_BUFFER:
        {
            ranges.AppendSRVRange(1, srvIndex++, 0);
            rangeCount[resource->set] += 1;
            visibility[resource->set] |= resource->stages;
            break;
        }
        }
    }

    DX12RootSignatureCreate createInfo{};

    createInfo.numOfRootParameters = numRootParameters;

    createInfo.rootParameters = (D3D12_ROOT_PARAMETER*)AllocFromTemp(sizeof(D3D12_ROOT_PARAMETER) * numRootParameters, 4);

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

        createInfo.CreateDescriptorTable(rootParamIndex, &ranges, rangeIterIndex, rangeCount[i], visible);

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

        createInfo.CreateDescriptorTable(numRootParameters - 1, &ranges, rangeIterIndex, samplerCount, visible);
    }
   
    EntryHandle rootSignature = deviceInstance.CreateRootSignature(&createInfo, D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT);

    return rootSignature;
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

    int descriptorTableCount = numDescriptorSet;

    DescriptorHeapManager* mainHeap = (DescriptorHeapManager*)deviceInstance.GetAndValidateItem(mainSRVDescriptorHeap, D12DESCRIPTORMANAGER);

    DescriptorHeapManager* samplerHeap = (DescriptorHeapManager*)deviceInstance.GetAndValidateItem(mainSamplerDescriptorHeap, D12DESCRIPTORMANAGER);

    DescriptorHeapManager* stageMainHeap = (DescriptorHeapManager*)deviceInstance.GetAndValidateItem(stagingSRVDescriptorHeap, D12DESCRIPTORMANAGER);

    DescriptorHeapManager* stageSamplerHeap = (DescriptorHeapManager*)deviceInstance.GetAndValidateItem(stagingSamplerDescriptorHeap, D12DESCRIPTORMANAGER);

    int samplerIndex = stageSamplerHeap->descriptorHeapHandlePointer;

    int maxSamplerFrameCount = 0;

    for (int i = 0; i < numDescriptorSet; i++)
    {
        int descriptorid = descriptorsets[i];

        obj->descriptorHeapSelection[i] = 0;

        if (srvDescriptorTablesStart[descriptorid] != -1)
        {
            obj->descriptorHeapPointer[i] = srvDescriptorTablesStart[descriptorid];
            obj->resourceCount[i] = srvDescriptorTablesCounts[descriptorid];
            continue;
        }

        uintptr_t head = descriptorSets[descriptorid];
        ShaderResourceSet* set = (ShaderResourceSet*)head;
        uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

        int count = set->bindingCount;

        int numberOfTables = set->setCount;

        maxSamplerFrameCount = max(maxSamplerFrameCount, numberOfTables);

        int mainHeapIndex = stageMainHeap->descriptorHeapHandlePointer;

        for (int i = 0; i < count; i++)
        {
            ShaderResourceHeader* header = (ShaderResourceHeader*)offsets[i];

            switch (header->type)
            {
            case ShaderResourceType::IMAGE2D:
            {
                ShaderResourceImage* image = (ShaderResourceImage*)header;
                deviceInstance.CreateImageSRVDescriptorHandle(image->textureHandle,
                    1, DXGI_FORMAT_B8G8R8A8_UNORM_SRGB, stageMainHeap, mainHeapIndex++, D3D12_SRV_DIMENSION_TEXTURE2D
                );
                break;
            }
            case ShaderResourceType::SAMPLERSTATE:
            {
                ShaderResourceSampler* image = (ShaderResourceSampler*)header;
                deviceInstance.CreateImageSampler(stageSamplerHeap, samplerIndex++);
                samplerCount++;
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

                Allocation* alloc = &allocationHandle[buffer->allocationIndex];

                deviceInstance.CreateCBVDescriptorHandle(alloc->bufferHandle, i * alloc->stridesize + alloc->offset, alloc->stridesize, stageMainHeap, mainHeapIndex++);

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
                Allocation* alloc = &allocationHandle[buffer->allocation];

                deviceInstance.CreateSRVDescriptorHandle(alloc->bufferHandle,
                    i * alloc->stridesize + alloc->offset,
                    buffer->arrayCount, alloc->requestedsize,
                    DXGI_FORMAT_UNKNOWN,
                    stageMainHeap,
                    mainHeapIndex++,
                    D3D12_SRV_DIMENSION_BUFFER
                );

                break;
            }


            case ShaderResourceType::BUFFER_VIEW:
            {
                ShaderResourceBufferView* bufferView = (ShaderResourceBufferView*)header;

                break;
            }
            }
        }


        int mainTableCount = mainHeapIndex - stageMainHeap->descriptorHeapHandlePointer;

        for (int j = 0; j < numberOfTables; j++)
        {
            deviceInstance.CopyDescriptors(mainTableCount, 
                stageMainHeap->descriptorHeapHandlePointer, mainHeap->descriptorHeapHandlePointer + (mainTableCount * j),
                stageMainHeap->descriptorHeap, mainHeap->descriptorHeap,
                stageMainHeap->descriptorHeapHandleSize, mainHeap->descriptorHeapHandleSize,
                D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV
            );
        }

        obj->descriptorHeapPointer[i] = srvDescriptorTablesStart[descriptorid] = mainHeap->descriptorHeapHandlePointer;
        obj->resourceCount[i] = srvDescriptorTablesCounts[descriptorid] = mainTableCount;

        mainHeap->descriptorHeapHandlePointer += (mainTableCount * numberOfTables);
    }

    if (samplerCount)
    {
        for (int j = 0; j < maxSamplerFrameCount; j++)
        {
            deviceInstance.CopyDescriptors(samplerCount, stageSamplerHeap->descriptorHeapHandlePointer, samplerHeap->descriptorHeapHandlePointer + (samplerCount * j),
                stageSamplerHeap->descriptorHeap, samplerHeap->descriptorHeap,
                stageSamplerHeap->descriptorHeapHandleSize, samplerHeap->descriptorHeapHandleSize,
                D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER
            );
        }

        obj->descriptorHeapPointer[numDescriptorSet] = samplerHeap->descriptorHeapHandlePointer;
        obj->resourceCount[numDescriptorSet] =  samplerCount;
        obj->descriptorHeapSelection[numDescriptorSet] = 1;
        
        samplerHeap->descriptorHeapHandlePointer += (samplerCount * maxSamplerFrameCount);

        descriptorTableCount++;
    }

    obj->descriptorTableCount = descriptorTableCount;
}




ShaderGraph* CreateShaderGraph(
    const std::string& filename,
    char* detailsData,
    ShaderDetails** details
)
{

    uintptr_t TreeNodes[50]{};
    int SetNodes[15]{};
    int ShaderRefs[5]{};

    OSFileHandle fileHandle;

    auto ret = OSOpenFile(filename.c_str(), READ, &fileHandle);

    if (ret)
    {
        throw std::runtime_error("Shader Init file is unable to be opened");
    }

    char* data = (char*)AllocFromTemp(fileHandle.fileLength, 64);

    int shaderGraphSize = fileHandle.fileLength;

    OSReadFile(&fileHandle, fileHandle.fileLength, data);

    OSCloseFile(&fileHandle);

    int shaderCount = 0;
    int shaderResourceCount = 0;
    int setCount = 0;

    int lastShader = 0;
    int shaderDetailDataStride = 0;

    int tagCount = 0;
    int curr = 0;

    int stride = SkipLine(data, shaderGraphSize, curr);
    curr += stride;

    while (curr + stride < shaderGraphSize)
    {

        unsigned long hashl = 0;
        bool opening = true;
        stride = ProcessTag(data,  shaderGraphSize, curr, &hashl, &opening);
        curr += stride;

        stride = SkipLine(data, shaderGraphSize, curr);

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
                

                stride = HandleShader(data, shaderGraphSize, curr, &TreeNodes[tagCount], detailsData, &shaderDetailDataStride);

                ShaderRefs[shaderCount] = tagCount;

                details[shaderCount] = (ShaderDetails*)detailsData;

                detailsData += shaderDetailDataStride;

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
                stride = HandleShaderResourceItem(data,  shaderGraphSize, curr, &TreeNodes[tagCount]);
                shaderResourceCount++;
            }
            break;
        case hash("ComputeLayout"):
            //std::cout << "ComputeLayout" << std::endl;
            if (opening) {
                stride = HandleComputeLayout(data,  shaderGraphSize, curr, &TreeNodes[tagCount]);
            }

            break;
        }

        curr += stride;


    }



    ShaderGraph* graph = (ShaderGraph*)(AllocFromTemp(sizeof(ShaderGraph) + (setCount * sizeof(ShaderSetLayout)) + (shaderResourceCount * sizeof(ShaderResource)) + (shaderCount * sizeof(ShaderMap)), 4));

    memset(graph, 0, sizeof(ShaderGraph) + (setCount * sizeof(ShaderSetLayout)) + (shaderResourceCount * sizeof(ShaderResource)));

    graph->shaderMapCount = shaderCount;
    graph->resourceSetCount = setCount;

    int shaderIndex = 0;

    for (int j = 0; j < shaderCount; j++)
    {
        ShaderMap* map = (ShaderMap*)graph->GetMap(j);

        ShaderDetailsXMLTag* tag = (ShaderDetailsXMLTag*)TreeNodes[ShaderRefs[j]];

        ShaderStageType type = tag->type;

        /*if (type == ShaderStageTypeBits::COMPUTESHADERSTAGE)
        {
            ShaderComputeLayoutXMLTag* ctag = (ShaderComputeLayoutXMLTag*)TreeNodes[ShaderRefs[j] + 1];
            ShaderDetails* deats = details[j];
            ShaderComputeLayout* layout = (ShaderComputeLayout*)deats->GetShaderData();
            layout->x = ctag->comps.x;
            layout->y = ctag->comps.y;
            layout->z = ctag->comps.z;
        } */

        map->type = type;


    }

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


void CreatePipelineDescription(const std::string& filename, GenericPipelineStateInfo* stateInfo)
{
    memset(stateInfo, 0, sizeof(GenericPipelineStateInfo));


    OSFileHandle fileHandle;

    auto ret = OSOpenFile(filename.c_str(), READ, &fileHandle);

    if (ret)
    {
        throw std::runtime_error("Shader Init file is unable to be opened");
    }

    char* data = (char*)AllocFromTemp(fileHandle.fileLength, 64);

    OSReadFile(&fileHandle, fileHandle.fileLength, data);

    char* dataStart = data;
    int dataSize = fileHandle.fileLength;

    int tagCount = 0;
    int curr = 0;

    int stride = SkipLine(dataStart, dataSize, curr);
    curr += stride;

    int currentVertexInput = 0;
    int currentVertexInputDescription = 0;

    OSCloseFile(&fileHandle);

    while (curr + stride < dataSize)
    {

        unsigned long hashl = 0;
        bool opening = true;
        stride = ProcessTag(dataStart, dataSize, curr, &hashl, &opening);
        curr += stride;

        stride = SkipLine(dataStart, dataSize, curr);

        if (opening)
        {
            tagCount++;
        }



        switch (hashl)
        {
        case hash("PipelineDescription"):
            if (opening)
            {
                stride = HandlePipelineDescription(dataStart, dataSize, curr, stateInfo);
            }
            break;
        case hash("DepthTest"):
            if (opening)
            {
                stride = HandleDepthTest(dataStart, dataSize, curr, stateInfo);
            }
            break;
        case hash("PrimitiveType"):
            if (opening)
            {
                stride = HandlePrimitiveType(dataStart, dataSize, curr, stateInfo);
            }
            break;

        case hash("CullMode"):
            if (opening)
            {
                stride = HandleCullMode(dataStart, dataSize, curr, stateInfo);
            }
            break;
        case hash("VertexInput"):
        {
            if (opening)
            {
                stateInfo->vertexBufferDescCount++;
                stride = HandleVertexInput(dataStart, dataSize, curr, stateInfo, currentVertexInput);
            }
            else
            {
                currentVertexInput++;
            }
            break;
        }
        case hash("VertexComponent"):
        {
            if (opening)
            {
                stateInfo->vertexBufferDesc[currentVertexInput].descCount++;
                stride = HandleVertexComponentInput(dataStart, dataSize, curr, stateInfo, currentVertexInput, currentVertexInputDescription);
            }
            else
            {
                currentVertexInputDescription++;
            }
            break;
        }
        case hash("FrontStencilTest"):
        {
            if (opening)
            {
                stateInfo->StencilEnable = true;
                stride = HandleStencilTest(dataStart, dataSize, curr, &stateInfo->frontFace);
            }
            break;
        }
        case hash("BackStencilTest"):
        {
            if (opening)
            {
                stateInfo->StencilEnable = true;
                stride = HandleStencilTest(dataStart, dataSize, curr, &stateInfo->backFace);
            }
            break;
        }
        }

        curr += stride;


    }
}



