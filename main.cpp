#define WIN32_LEAN_AND_MEAN

#include <bit>

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

#define TEMP_MEM_SIZE 64*MiB
#define STORAGE_MEM_SIZE 128*MiB

static char globalMemoryPool[STORAGE_MEM_SIZE];
static char tempGlobalMemoryPool[TEMP_MEM_SIZE];

static RingAllocator tempGlobalMemoryAllocator(tempGlobalMemoryPool, TEMP_MEM_SIZE);
static SlabAllocator globalMemoryAllocater(globalMemoryPool, STORAGE_MEM_SIZE);




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
D3D12_RENDER_PASS_ENDING_ACCESS_TYPE ConvertAttachmentStoreUsageToEndingAccess(AttachmentStoreUsage usage);
D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE ConvertAttachmentLoadUsageToBeginningAccess(AttachmentLoadUsage usage);
D3D12_BARRIER_LAYOUT ConvertToBarrierLayout(ImageLayout layout);

D3D12_BARRIER_LAYOUT ConvertToBarrierLayout(ImageLayout layout)
{
    D3D12_BARRIER_LAYOUT returnBarrier = D3D12_BARRIER_LAYOUT_UNDEFINED;

    switch (layout)
    {
    case ImageLayout::GENERAL_LAYOUT:
        returnBarrier = D3D12_BARRIER_LAYOUT_COMMON;
        break;
    case ImageLayout::UNDEFINED:
        returnBarrier = D3D12_BARRIER_LAYOUT_UNDEFINED;
        break;

    case ImageLayout::WRITEABLE:
        returnBarrier = D3D12_BARRIER_LAYOUT_UNORDERED_ACCESS;
        break;

    case ImageLayout::SHADERREADABLE:
        returnBarrier = D3D12_BARRIER_LAYOUT_SHADER_RESOURCE;
        break;

    case ImageLayout::COLORATTACHMENT:
        returnBarrier = D3D12_BARRIER_LAYOUT_RENDER_TARGET;
        break;

    case ImageLayout::DEPTHSTENCILATTACHMENT:
    case ImageLayout::STENCILATTACHMENT:
    case ImageLayout::DEPTHATTACHMENT:
        returnBarrier = D3D12_BARRIER_LAYOUT_DEPTH_STENCIL_WRITE;
        break;

    case ImageLayout::PRESENT:
        returnBarrier = D3D12_BARRIER_LAYOUT_PRESENT;
        break;

    default:
        returnBarrier = D3D12_BARRIER_LAYOUT_UNDEFINED;
        break;
    }

    return returnBarrier;
}

void 
CreateDepthDesc(
    D3D12_RENDER_PASS_DEPTH_STENCIL_DESC* desc,
    D3D12_CPU_DESCRIPTOR_HANDLE dsv,
    const AttachmentClear& clear, 
    ImageFormat depthStencilFormat)
{

    desc->cpuDescriptor = dsv;

    switch (clear.type)
    {
    case CLEARDEPTH:
    {
        desc->DepthBeginningAccess.Type = D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_CLEAR;

        desc->DepthBeginningAccess.Clear.ClearValue.Format = ConvertImageFormatToDXGIFormat(depthStencilFormat);
        desc->DepthBeginningAccess.Clear.ClearValue.DepthStencil.Depth = clear.val.ddata;
    } break;

    case NOCLEAR:
    default:
    {
        desc->DepthBeginningAccess.Type =
            D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_DISCARD;
    } break;
    }
}

void
CreateRenderTargetDesc(
    D3D12_RENDER_PASS_RENDER_TARGET_DESC* desc,
    D3D12_CPU_DESCRIPTOR_HANDLE rtv,
    const AttachmentClear& clear,
    ImageFormat rtvFormat)
{
    desc->cpuDescriptor = rtv;

    switch (clear.type)
    {
    case CLEARCOLOR:
    {
        desc->BeginningAccess.Type = D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_CLEAR;

        desc->BeginningAccess.Clear.ClearValue.Format = ConvertImageFormatToDXGIFormat(rtvFormat); // You should set this!
        memcpy(desc->BeginningAccess.Clear.ClearValue.Color,
            clear.val.cdata,
            sizeof(float) * 4);
    } break;

    case NOCLEAR:
    default:
    {
        desc->BeginningAccess.Type = D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_DISCARD;
    } break;
    }

    return;
}

void CreateStencilDesc(
    D3D12_RENDER_PASS_DEPTH_STENCIL_DESC* desc,
    const AttachmentClear& clear,
    ImageFormat format, AttachmentStoreUsage storeOp)
{
    switch (clear.type)
    {
    case CLEARDEPTH:
    {
        desc->StencilBeginningAccess.Type = D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_CLEAR;
        desc->StencilBeginningAccess.Clear.ClearValue.Format = ConvertImageFormatToDXGIFormat(format);
        desc->StencilBeginningAccess.Clear.ClearValue.DepthStencil.Stencil = clear.val.sdata;
                                                                                                
    } break;

    case NOCLEAR:
    default:
    {
        desc->StencilBeginningAccess.Type = D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_DISCARD;
    } break;
    }
    desc->StencilEndingAccess.Type = ConvertAttachmentStoreUsageToEndingAccess(storeOp);
}

D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE
ConvertAttachmentLoadUsageToBeginningAccess(AttachmentLoadUsage usage)
{
    switch (usage)
    {
    case AttachmentLoadUsage::ATTACHNOCARE:
        return D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_DISCARD;

    case AttachmentLoadUsage::ATTACHCLEAR:
        return D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_CLEAR;

    default:
        // Safe fallback
        return D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_DISCARD;
    }
}

D3D12_RENDER_PASS_ENDING_ACCESS_TYPE
ConvertAttachmentStoreUsageToEndingAccess(AttachmentStoreUsage usage)
{
    switch (usage)
    {
    case AttachmentStoreUsage::ATTACHDISCARD:
        return D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_DISCARD;

    case AttachmentStoreUsage::ATTACHSTORE:
        return D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_PRESERVE;

    default:
        // Safe fallback
        return D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_DISCARD;
    }
}


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




void* AllocFromTemp(int size, int alignment)
{
    return tempGlobalMemoryAllocator.Allocate(size, alignment);
}

void* AllocFromPermanent(int size, int alignment)
{
    return globalMemoryAllocater.Allocate(size, alignment);
}





DX12Device deviceInstance;

EntryHandle queueHandle;

EntryHandle swapChain;

EntryHandle swapChainDepthImages[MAX_FRAMES_IN_FLIGHT];

EntryHandle graphicCommandPools[MAX_FRAMES_IN_FLIGHT];

EntryHandle transferCommandPool;
DX12GraphicsCommandRecorder transferCommandRecorder;

int transferCommandsUploaded = 0;


EntryHandle globalRTVDescriptorHeap;
EntryHandle globalDSVDescriptorHeap;

UINT currentFrame = 0;
UINT currentImageIndex = 0;

EntryHandle globalRendererFence, globalTransferFence;

uint64_t g_FrameFenceValues[MAX_FRAMES_IN_FLIGHT] = {};

bool g_VSync = true;

bool g_TearingSupported = false;

EntryHandle storedRenderables[2];

ShaderHandles shaderHandles[2]{};

DX12GraphicsCommandRecorder commandRecorders[MAX_FRAMES_IN_FLIGHT];

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


static AttachmentGraph mainGraphLayout;
static AttachmentGraphInstance mainGraphInstance;

void ParseBMP(TextureDetails* details, const char* name);

int Render();

void ReleaseD3D12Resources();

void WriteToHostMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies);
void WriteToDeviceLocalMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies);
void WriteToImageDeviceLocalMemory(EntryHandle imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers);


EntryHandle CreateRootSignatureFromShaderGraph(ShaderGraph* graph);

int CreateCBVArgsFromResourceSet(int descriptorid, int rootParamId, DX12ConstantBufferPipelineArguments* args);

struct Camera
{
    XMMATRIX proj;
    XMMATRIX view;
};

XMMATRIX world[2];

Camera cam;


EntryHandle CreatePipelineStateObject(EntryHandle rootSignature, ShaderHandles* handles, int shaderCount, GenericPipelineStateInfo* stateInfo);

EntryHandle CreateGraphicsPipelineObject(int* descriptorsets, int descCount, DX12PipelineCreationInfo* info);

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


static int allocationHandleIndex = 0;

RenderAllocation allocationHandle[50];

int CreateTablesFromResourceSet(int* descriptorsets, int numDescriptorSet, int rootParamOffset, DX12DescriptorTableBindings* tableBindings);

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

int AllocFromHostBuffer(size_t size, size_t structureCopies, size_t alignment, ComponentFormatType format, int bufferCopies)
{
    size_t perStructSize = (size + alignment - 1) & ~((size_t)alignment - 1);

    size_t totalAllocSize = perStructSize * structureCopies;

    totalAllocSize *= bufferCopies;

    size_t location = deviceInstance.AllocFromDriverMemoryBuffer(hostBuffer, totalAllocSize, perStructSize);

    int index = allocationHandleIndex++;

    allocationHandle[index].memIndex = hostBuffer;
    allocationHandle[index].offset = location;
    allocationHandle[index].deviceAllocSize = totalAllocSize;
    allocationHandle[index].requestedSize = size;
    allocationHandle[index].alignment = alignment;
    allocationHandle[index].structureCopies = (int)structureCopies;
    allocationHandle[index].formatType = format;
    allocationHandle[index].allocType = (bufferCopies > 1) ? AllocationType::PERDRAW : AllocationType::STATIC;

    allocationHandle[index].viewIndex = deviceInstance.CreateBufferView(location, perStructSize, (UINT)structureCopies, ConvertComponentFormatToDXGIFormat(format), bufferCopies);


    return index;
}

int AllocFromDeviceBuffer(size_t size, size_t structureCopies, size_t alignment, ComponentFormatType format, int bufferCopies)
{
    size_t perStructSize = (size + alignment - 1) & ~((size_t)alignment - 1);

    size_t totalAllocSize = perStructSize * structureCopies;

    totalAllocSize *= bufferCopies;

    size_t location = deviceInstance.AllocFromDriverMemoryBuffer(deviceLocalBuffer, totalAllocSize, perStructSize);

    int index = allocationHandleIndex++;

    allocationHandle[index].memIndex = deviceLocalBuffer;
    allocationHandle[index].offset = location;
    allocationHandle[index].deviceAllocSize = totalAllocSize;
    allocationHandle[index].requestedSize = size;
    allocationHandle[index].alignment = alignment;
    allocationHandle[index].structureCopies = (int)structureCopies;
    allocationHandle[index].formatType = format;
    allocationHandle[index].allocType = (bufferCopies > 1) ? AllocationType::PERDRAW : AllocationType::STATIC;

    allocationHandle[index].viewIndex = deviceInstance.CreateBufferView(location, perStructSize, (UINT)structureCopies, ConvertComponentFormatToDXGIFormat(format), bufferCopies);

    return index;
}

DX12ShaderType ConvertShaderStageToD3D12ShaderStage(ShaderStageType type)
{
    DX12ShaderType sType = VERTEX;
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

#define MAX_PUSH_ARGS 8
#define MAX_ROOT_PARAMS 8

EntryHandle CreateGraphicsPipelineObject(int* descriptorsets, int descCount, DX12PipelineCreationInfo* info)
{
    int rootParamIndex = 0;
    UINT pushConstantsArgsCount = 0;

    DX12ConstantBufferPipelineArguments* pushArgs = (DX12ConstantBufferPipelineArguments*)AllocFromTemp(sizeof(DX12ConstantBufferPipelineArguments) * MAX_PUSH_ARGS, alignof(DX12ConstantBufferPipelineArguments));
    DX12DescriptorTableBindings* tables = (DX12DescriptorTableBindings*)AllocFromTemp(sizeof(DX12DescriptorTableBindings) * MAX_ROOT_PARAMS, alignof(DX12DescriptorTableBindings));

    for (int b = 0; b < descCount; b++)
    {
        pushConstantsArgsCount += CreateCBVArgsFromResourceSet(descriptorsets[b], 0, pushArgs + pushConstantsArgsCount);
    }

    if (pushConstantsArgsCount)
        rootParamIndex++;

    info->cbvArgsCount = pushConstantsArgsCount;

    info->descriptorTableCount = (UINT)CreateTablesFromResourceSet(descriptorsets, descCount, rootParamIndex, tables);

    info->tables = tables;

    return deviceInstance.CreateGraphicsPipelineObject(info, pushArgs);

}

XMVECTOR color = { 1.0f, 0.0f, 0.0f, 1.0f };


std::array<int, 10> renderPassesHandles{};

int CreateAttachmentResources(AttachmentGraphInstance* graphInstance, int renderPassIndex, int imageCount, EntryHandle* backBufferResources, EntryHandle* backBufferViews, uint32_t width, uint32_t height, RenderPassType rpType, AttachmentClear* clears)
{
    static int rpIndex = 0;

    AttachmentRenderPassInstance* currentRenderPass = &graphInstance->passes[renderPassIndex];

    AttachmentRenderPass* currentRenderPassDescription = &graphInstance->graphLayout->holders[renderPassIndex];

    int baseRPData = currentRenderPass->baseRenderPassData = rpIndex;

    if (currentRenderPassDescription->depthStencilCount)
    {
        rpIndex += 1;
    }

    rpIndex += 1;

    int colorCount = currentRenderPassDescription->colorCount;

    int depthCount = currentRenderPassDescription->depthStencilCount;

    int depthOffset = (colorCount * imageCount);

    int depthIndex = 0;

    int colorIndex = 0;

    int attachmentCount = currentRenderPass->attachInstCount;

    AttachmentInstance* attachInsts = currentRenderPass->attachInst;

    currentRenderPass->rpType = rpType;

    EntryHandle* attachmentViews = (EntryHandle*)AllocFromTemp(sizeof(EntryHandle) * attachmentCount * imageCount, alignof(EntryHandle));

    EntryHandle* attachmentResources = (EntryHandle*)AllocFromTemp(sizeof(EntryHandle) * attachmentCount * imageCount, alignof(EntryHandle));

    for (int b = 0; b < attachmentCount; b++)
    {
        AttachmentInstance* attachDesc = &attachInsts[b];

        int resourceIndex = attachDesc->attachmentResource;

        AttachmentResourceInstance* resourceInst = &graphInstance->resources[resourceIndex];

        AttachmentResource* resourceTempl = &graphInstance->graphLayout->resources[resourceIndex];

        int sampHi = resourceInst->sampHi;
        int sampLo = resourceInst->sampLo;

        int sampleCount = max(std::bit_width((unsigned)sampHi) - 1, 1);

        int imageWidth = width;
        int imageHeight = height;

        if (clears)
        {
            attachDesc->clear = clears[resourceIndex];
        }

        if (resourceTempl->viewType == AttachmentViewType::SWAPCHAIN)
        {
            resourceInst->attachmentImage = (EntryHandle**)AllocFromPermanent(sizeof(EntryHandle*) * 1, alignof(EntryHandle));
            resourceInst->attachmentImageView = (EntryHandle**)AllocFromPermanent(sizeof(EntryHandle*) * 1, alignof(EntryHandle));
            resourceInst->attachmentImageView[0] = backBufferViews;
            resourceInst->attachmentImage[0] = backBufferResources;

            for (int g = 0; g < imageCount; g++)
            {
                attachmentResources[(g * colorCount) + colorIndex] = backBufferResources[g];
                attachmentViews[(g * colorCount) + colorIndex] = backBufferViews[g];
            }

            colorIndex++;
        }
        else
        {
            if (!resourceInst->attachmentImage)
            {
                resourceInst->attachmentImage = (EntryHandle**)AllocFromPermanent(sizeof(EntryHandle*) * sampleCount, alignof(EntryHandle));

                for (int c = 0; c < sampleCount; c++)
                {
                    resourceInst->attachmentImage[c] = (EntryHandle*)AllocFromPermanent(sizeof(EntryHandle) * imageCount, alignof(EntryHandle));
                }
            }

            if (!resourceInst->attachmentImageView)
            {

                resourceInst->attachmentImageView = (EntryHandle**)AllocFromPermanent(sizeof(EntryHandle*) * sampleCount, alignof(EntryHandle));

                for (int c = 0; c < sampleCount; c++)
                {
                    resourceInst->attachmentImageView[c] = (EntryHandle*)AllocFromPermanent(sizeof(EntryHandle) * imageCount, alignof(EntryHandle));
                }
            }

            DXGI_FORMAT attachmentFormat = ConvertImageFormatToDXGIFormat(resourceTempl->format);

            resourceInst->imageCount = imageCount;

            switch (resourceInst->usage)
            {

            case AttachmentResourceInstanceUsage::COLOR_ATTACHMENT_USAGE:

                for (int v = 0; v < sampleCount; v++)
                {
                    for (int g = 0; g < imageCount; g++)
                    {
                       attachmentResources[(g*colorCount)+colorIndex] = resourceInst->attachmentImage[v][g] = deviceInstance.CreateImageResourceFromPool(rsvdsvPool, imageWidth, imageHeight,
                            1, 1, D3D12_RESOURCE_FLAG_ALLOW_RENDER_TARGET, attachmentFormat, D3D12_RESOURCE_DIMENSION_TEXTURE2D, D3D12_RESOURCE_STATE_RENDER_TARGET);
                

                       attachmentViews[(g * colorCount) + colorIndex] = resourceInst->attachmentImageView[v][g]
                            =
                            deviceInstance.CreateImageView(attachmentFormat, 0, 1);
                    }

                    sampLo <<= 1;
                }

                colorIndex++;

                break;
            case AttachmentResourceInstanceUsage::DEPTH_STENCIL_ATTACHMENT_USAGE:
                
                for (int v = 0; v < sampleCount; v++)
                {
                    for (int g = 0; g < imageCount; g++)
                    {
                        attachmentResources[depthOffset + (g * depthCount) + depthIndex] = resourceInst->attachmentImage[v][g] = deviceInstance.CreateImageResourceFromPool(rsvdsvPool, imageWidth, imageHeight,
                            1, 1, D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL, attachmentFormat, D3D12_RESOURCE_DIMENSION_TEXTURE2D, D3D12_RESOURCE_STATE_COMMON);

                        attachmentViews[depthOffset + (g * depthCount) + depthIndex] = resourceInst->attachmentImageView[v][g]
                            =
                            deviceInstance.CreateImageView(attachmentFormat, 0, 1);
                    }

                    sampLo <<= 1;
                }
                depthIndex++;
                break;
            case AttachmentResourceInstanceUsage::DEPTH_ATTACHMENT_USAGE:
                for (int v = 0; v < sampleCount; v++)
                {
                    for (int g = 0; g < imageCount; g++)
                    {
                        attachmentResources[depthOffset + (g * depthCount) + depthIndex] = resourceInst->attachmentImage[v][g] = deviceInstance.CreateImageResourceFromPool(rsvdsvPool, imageWidth, imageHeight,
                            1, 1, D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL, attachmentFormat, D3D12_RESOURCE_DIMENSION_TEXTURE2D, D3D12_RESOURCE_STATE_COMMON);

                        attachmentViews[depthOffset + (g * depthCount) + depthIndex] = resourceInst->attachmentImageView[v][g]
                            =
                            deviceInstance.CreateImageView(attachmentFormat, 0, 1);
                    }
                    sampLo <<= 1;

                }
                depthIndex++;
                break;

            case AttachmentResourceInstanceUsage::STENCIL_ATTACHMENT_USAGE:
                for (int v = 0; v < sampleCount; v++)
                {
                    for (int g = 0; g < imageCount; g++)
                    {
                        attachmentResources[depthOffset + (g * depthCount) + depthIndex] = resourceInst->attachmentImage[v][g] = deviceInstance.CreateImageResourceFromPool(rsvdsvPool, imageWidth, imageHeight,
                            1, 1, D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL, attachmentFormat, D3D12_RESOURCE_DIMENSION_TEXTURE2D, D3D12_RESOURCE_STATE_COMMON);

                        attachmentViews[depthOffset + (g * depthCount) + depthIndex] = resourceInst->attachmentImageView[v][g]
                            =
                            deviceInstance.CreateImageView(attachmentFormat, 0, 1);
                    }
                    sampLo <<= 1;

                }
                depthIndex++;
                break;

            case AttachmentResourceInstanceUsage::RESOLVE_ATTACHMENT_USAGE:
                for (int g = 0; g < imageCount; g++)
                {
                    attachmentResources[(g * colorCount) + colorIndex] = resourceInst->attachmentImage[0][g] = deviceInstance.CreateImageResourceFromPool(rsvdsvPool, imageWidth, imageHeight,
                        1, 1, D3D12_RESOURCE_FLAG_ALLOW_DEPTH_STENCIL, attachmentFormat, D3D12_RESOURCE_DIMENSION_TEXTURE2D, D3D12_RESOURCE_STATE_RENDER_TARGET|D3D12_RESOURCE_STATE_RESOLVE_SOURCE);

                    attachmentViews[(g * colorCount) + colorIndex] = resourceInst->attachmentImageView[0][g]
                        =
                        deviceInstance.CreateImageView(attachmentFormat, 0, 1);
                }
                colorIndex++;
                break;

            case AttachmentResourceInstanceUsage::PRESERVE_ATTACHMENT_USAGE:
                //flags = 0; // no direct Vulkan usage flag
                break;

            case AttachmentResourceInstanceUsage::INPUT_ATTACHMENT_USAGE:
                //flags = VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT;
                break;
            }
        }
    }


   
    renderPassesHandles[baseRPData] = deviceInstance.CreateRenderTargetViews(globalRTVDescriptorHeap, &attachmentResources[0], colorCount, imageCount);
    
    renderPassesHandles[baseRPData+1] = deviceInstance.CreateDepthStencilView(globalDSVDescriptorHeap, &attachmentResources[(depthOffset)], &attachmentViews[(depthOffset)], imageCount);
   
    return 0;
}

void CreateFrameGraphInstance(AttachmentGraph* graphLayout, AttachmentGraphInstance* outInstance)
{
    int renderPassCount = graphLayout->passesCount;
    int resourceCount = graphLayout->resourceCount;

    outInstance->passes = (AttachmentRenderPassInstance*)AllocFromPermanent(sizeof(AttachmentRenderPassInstance) * renderPassCount, 8);
    outInstance->resources = (AttachmentResourceInstance*)AllocFromPermanent(sizeof(AttachmentResourceInstance) * resourceCount, 8);
    outInstance->graphLayout = graphLayout;

    for (int i = 0; i < renderPassCount; i++)
    {
        AttachmentRenderPass* renderPassDescription = &graphLayout->holders[i];

        AttachmentRenderPassInstance* passInstance = &outInstance->passes[i];

        passInstance->maxSampleCount = 1;

        int attachmentCount = passInstance->attachInstCount = renderPassDescription->attachmentCount;

        passInstance->attachInst = (AttachmentInstance*)AllocFromPermanent(sizeof(AttachmentInstance) * attachmentCount, 8);

        passInstance->currentSampleCount = 0;

        int sampHi = 1, sampLo = 1;

        for (int c = 0; c < attachmentCount; c++)
        {
            AttachmentDescription* desc = &renderPassDescription->descs[c];

            AttachmentResource* resDesc = &graphLayout->resources[desc->resourceIndex];

            AttachmentResourceInstance* currResource = &outInstance->resources[desc->resourceIndex];

            int sampleCountLo = (resDesc->msaa ? 2 : 1);

            int sampleCountHi = (resDesc->msaa ? (1 << 1) : 1);

            sampHi = max(sampleCountHi, sampHi);

            sampLo = max(sampleCountLo, sampLo);

            currResource->attachmentImage = currResource->attachmentImageView = nullptr;

            passInstance->attachInst[c].attachmentResource = desc->resourceIndex;
            passInstance->attachInst[c].descLayout = desc;
          
            switch (desc->attachType)
            {
            case AttachmentDescriptionType::COLORATTACH:
                currResource->usage = AttachmentResourceInstanceUsage::COLOR_ATTACHMENT_USAGE;
                break;
            case AttachmentDescriptionType::RESOLVEATTACH:
                currResource->usage = AttachmentResourceInstanceUsage::RESOLVE_ATTACHMENT_USAGE;
                break;
            case AttachmentDescriptionType::DEPTHATTACH:
                currResource->usage = AttachmentResourceInstanceUsage::DEPTH_ATTACHMENT_USAGE;
                break;
            case AttachmentDescriptionType::DEPTHSTENCILATTACH:
                currResource->usage = AttachmentResourceInstanceUsage::DEPTH_STENCIL_ATTACHMENT_USAGE;
                break;
            case AttachmentDescriptionType::STENCILATTACH:
                currResource->usage = AttachmentResourceInstanceUsage::STENCIL_ATTACHMENT_USAGE;
                break;
            }

            currResource->sampLo = sampleCountLo;
            currResource->sampHi = sampleCountHi;
        }
    }
}


void DoSceneStuff()
{

    DX12SwapChain* swapChainDX = (DX12SwapChain*)deviceInstance.GetAndValidateItem(swapChain, D12SWAPCHAINHANDLE);

    StringView mainGraphInput = tempGlobalMemoryAllocator.AllocateFromNullStringCopy("MainFrameGraph.xml");

    CreateAttachmentGraphFromFile(mainGraphInput, &mainGraphLayout, &tempGlobalMemoryAllocator);

    CreateFrameGraphInstance(&mainGraphLayout, &mainGraphInstance);

    std::array<AttachmentClear, 2> clears = {
        RPClearType::CLEARCOLOR, {0.0, 0.0, 0.0, 0.0},
        RPClearType::CLEARDEPTH, {1.0, 0},
    };

    CreateAttachmentResources(&mainGraphInstance, 0, swapChainDX->numberOfImages, swapChainDX->backBufferResources, swapChainDX->backbufferViews, 800, 600, RenderPassType::SWAPCHAIN_IMAGE_COUNT, clears.data());

    transferCommandRecorder.ResetCommandPoolandBuffer();

    bgraPool = deviceInstance.CreateTextureMemoryPool(256 * MiB, D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT);

    SlabAllocator tempShaderDetails{ globalMemoryAllocater.Allocate(2 * KiB), 2 * KiB };

    ShaderDetails* sdetails = (ShaderDetails*)tempShaderDetails.Head();
  
    int shaderCount = 0;

    StringView graphNameView = tempGlobalMemoryAllocator.AllocateFromNullStringCopy("DirectLayout.xml");

    ShaderGraph* mainLayout = CreateShaderGraph(graphNameView, &tempGlobalMemoryAllocator, &globalMemoryAllocater, &tempShaderDetails, &shaderCount);

    for (int i = 0; i < mainLayout->shaderMapCount; i++)
    {
        ShaderMap* map = (ShaderMap*)mainLayout->GetMap(i);
        shaderHandles[i] = { ConvertShaderStageToD3D12ShaderStage(map->type), deviceInstance.CreateShaderBlob(sdetails->GetString()) };
        sdetails = sdetails->GetNext();
    }


    EntryHandle genericRoot = CreateRootSignatureFromShaderGraph(mainLayout);//CreateGenericRootSignature();

    GenericPipelineStateInfo pipeInfo{};

    StringView pipelineNameView = tempGlobalMemoryAllocator.AllocateFromNullStringCopy("GenericPipeline.xml");

    CreatePipelineDescription(pipelineNameView, &pipeInfo, &tempGlobalMemoryAllocator);

    pipeInfo.colorFormat = ImageFormat::R8G8B8A8;
    pipeInfo.depthFormat = ImageFormat::D32FLOAT;

    EntryHandle genericPipe  = CreatePipelineStateObject(genericRoot, shaderHandles, 2, &pipeInfo);

    TextureDetails details{};

    ParseBMP(&details, "face1.bmp");

    int cameraData = AllocFromHostBuffer(sizeof(Camera), 1, sizeof(Camera), ComponentFormatType::NO_BUFFER_FORMAT, MAX_FRAMES_IN_FLIGHT);
    int world1Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 1,  256, ComponentFormatType::NO_BUFFER_FORMAT, MAX_FRAMES_IN_FLIGHT);
    int world2Data = AllocFromDeviceBuffer(sizeof(XMMATRIX), 1, 256, ComponentFormatType::NO_BUFFER_FORMAT, MAX_FRAMES_IN_FLIGHT);
    int vertexOffset = AllocFromDeviceBuffer(sizeof(BoxVerts), 1, 16, ComponentFormatType::NO_BUFFER_FORMAT, 1);
    int indexOffset = AllocFromDeviceBuffer(sizeof(BoxIndices), 1, 16, ComponentFormatType::NO_BUFFER_FORMAT, 1);

    
    DX12DriverMemoryBuffer* buffer = (DX12DriverMemoryBuffer*)deviceInstance.GetAndValidateItem(deviceLocalBuffer, D12BUFFERMEMORYPOOL);

    transferCommandRecorder.TransitionBufferBarrier(buffer->bufferHandle, D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS, D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST);

    WriteToDeviceLocalMemory(vertexOffset, BoxVerts, sizeof(BoxVerts), 0, 1);

    WriteToDeviceLocalMemory(indexOffset, BoxIndices, sizeof(BoxIndices), 0, 1);

    WriteToHostMemory(cameraData, &cam, sizeof(Camera), 0, MAX_FRAMES_IN_FLIGHT);

    WriteToDeviceLocalMemory(world1Data, world, 64, 0, MAX_FRAMES_IN_FLIGHT);

    WriteToDeviceLocalMemory(world2Data, &world[1], 64, 0, MAX_FRAMES_IN_FLIGHT);

    transferCommandRecorder.TransitionBufferBarrier(buffer->bufferHandle, D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST, D3D12_BARRIER_SYNC_DRAW | D3D12_BARRIER_SYNC_INDEX_INPUT, D3D12_BARRIER_ACCESS_VERTEX_BUFFER | D3D12_BARRIER_ACCESS_CONSTANT_BUFFER | D3D12_BARRIER_ACCESS_INDEX_BUFFER);

    bgraImageMemoryPool = deviceInstance.CreateSampledImageHandle(bgraPool, details.width, details.height, 1, details.miplevels, details.type, D3D12_RESOURCE_DIMENSION_TEXTURE2D);

    WriteToImageDeviceLocalMemory(bgraImageMemoryPool, details.data, details.width, details.height, 4, details.dataSize, details.type, details.miplevels, 1);


    static EntryHandle mainSamplerIndex = deviceInstance.CreateSampler(7);

    static int camSRVDS = AllocateShaderResourceSet(mainLayout, 0, MAX_FRAMES_IN_FLIGHT);

    BindBufferToShaderResource(camSRVDS, &cameraData, nullptr, 0, 1, 0);
    BindImageResourceToShaderResource(camSRVDS, &bgraImageMemoryPool, 1, 0, 1);
    BindSamplerResourceToShaderResource(camSRVDS, &mainSamplerIndex, 1, 0, 2);

    static int worldOne = AllocateShaderResourceSet(mainLayout, 1, MAX_FRAMES_IN_FLIGHT);
    BindBufferToShaderResource(worldOne, &world1Data, nullptr, 0, 1, 0);
    
    static int worldTwo = AllocateShaderResourceSet(mainLayout, 1, MAX_FRAMES_IN_FLIGHT);
    BindBufferToShaderResource(worldTwo, &world2Data, nullptr, 0, 1, 0);

    int basic1[2] = { camSRVDS, worldOne };
    int basic2[2] = { camSRVDS, worldTwo };

    
   
    UploadConstant(camSRVDS, &color, 0);

    DX12PipelineCreationInfo info{}, info2{};

   
    EntryHandle heaps[2] = { mainSRVDescriptorHeap, mainSamplerDescriptorHeap };

    info.heapsCounts = 2;
    info.descriptorHeapHandles = heaps;

    info.topology = ConvertPrimitiveTypeToD3D12Topology(pipeInfo.primType);
    info.instanceCount = 1;

    info.vertexBufferHandle = buffer->bufferHandle;
    info.vertexBufferOffset = allocationHandle[vertexOffset].offset;
    info.vertexBufferSize = sizeof(BoxVerts);
    info.vertexSize = sizeof(XMVECTOR) * 2;
    info.vertexCount = 24;

    info.indexBuffer = buffer->bufferHandle;
    info.indexBufferOffset = allocationHandle[indexOffset].offset;
    info.indexBufferSize = sizeof(BoxIndices);
    info.indexSize = 2;
    info.indexCount = 36;

    info.rootSignature = genericRoot;
    info.pipelineState = genericPipe;

  
    info2 = info;

  
    EntryHandle tri1 = CreateGraphicsPipelineObject(basic1, 2, &info);
    EntryHandle tri2 = CreateGraphicsPipelineObject(basic2, 2, &info2);

    storedRenderables[0] = tri1;
    storedRenderables[1] = tri2;
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

    swapChain = deviceInstance.CreateSwapChainHandle(
        hwnd,
        queueHandle,
        MAX_FRAMES_IN_FLIGHT,
        800,
        600,
        ConvertImageFormatToDXGIFormat(ImageFormat::R8G8B8A8)
    );

 
    globalRTVDescriptorHeap = deviceInstance.CreateDescriptorHeapManager(MAX_FRAMES_IN_FLIGHT * 3, D3D12_DESCRIPTOR_HEAP_TYPE_RTV, D3D12_DESCRIPTOR_HEAP_FLAG_NONE);

    globalDSVDescriptorHeap = deviceInstance.CreateDescriptorHeapManager(
        MAX_FRAMES_IN_FLIGHT,
        D3D12_DESCRIPTOR_HEAP_TYPE_DSV,
        D3D12_DESCRIPTOR_HEAP_FLAG_NONE
    );

    mainSRVDescriptorHeap = deviceInstance.CreateDescriptorHeapManager(MAX_FRAMES_IN_FLIGHT * 100, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE);

    mainSamplerDescriptorHeap = deviceInstance.CreateDescriptorHeapManager(MAX_FRAMES_IN_FLIGHT * 100, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER, D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE);

    stagingSamplerDescriptorHeap = deviceInstance.CreateDescriptorHeapManager(MAX_FRAMES_IN_FLIGHT * 50, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER, D3D12_DESCRIPTOR_HEAP_FLAG_NONE);

    rsvdsvPool = deviceInstance.CreateDSVRSVMemoryPool(24 * MiB, (1<<22), false);

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
        EntryHandle graphicCommandBuffer = deviceInstance.CreateCommandList(
            graphicCommandPools[i],
            D3D12_COMMAND_LIST_TYPE_DIRECT
        );

        commandRecorders[i] = deviceInstance.CreateRecorder(graphicCommandBuffer, graphicCommandPools[i]);
    }
    
    EntryHandle transferCommandList = deviceInstance.CreateCommandList(
        transferCommandPool,
        D3D12_COMMAND_LIST_TYPE_DIRECT
    );

    transferCommandRecorder = deviceInstance.CreateRecorder(transferCommandList, transferCommandPool);


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
    transferCommandRecorder.ResetCommandPoolandBuffer();

    transferCommandsUploaded = 0;
}

int Render()
{
    deviceInstance.WaitForFenceValue(globalRendererFence, g_FrameFenceValues[currentFrame], UINT32_MAX);

    currentImageIndex = deviceInstance.AcquireNextImageFromSWC(swapChain);

    auto& commandRecorder = commandRecorders[currentFrame];

    ID3D12CommandList* commandLists[2];

    UINT cmdListIndex = 0;

    if (transferCommandsUploaded)
    {

        transferCommandRecorder.CloseCommandBuffer();

        commandLists[cmdListIndex++] = transferCommandRecorder.cmdList;

        transferCommandsUploaded = 0;
    }

    switch (currentFrame)
    {
    case 0:
        color = XMVectorSet(0.0, 1.0, 0.0, 1.0);
        break;
    case 1:
        color = XMVectorSet(1.0, 1.0, 0.0, 1.0);
        break;
    case 2:
        color = XMVectorSet(0.0, 0.0, 1.0, 1.0);
        break;    
    }

    commandRecorder.ResetCommandPoolandBuffer();

    for (int frameGraph = 0; frameGraph < 1; frameGraph++)
    {
        AttachmentGraphInstance* currGraphInstance = &mainGraphInstance;

        int numberOfRenderPasses = currGraphInstance->graphLayout->passesCount;

        for (int i = 0; i < numberOfRenderPasses; i++)
        {
            AttachmentRenderPass* currentRenderPassDesc = &currGraphInstance->graphLayout->holders[i];

            AttachmentRenderPassInstance* currentRenderPassInstance = &currGraphInstance->passes[i];

            int rtvCount = currentRenderPassDesc->colorCount;

            int depthStencilCount = currentRenderPassDesc->depthStencilCount;

            D3D12_RENDER_PASS_RENDER_TARGET_DESC* rtvDescs = (D3D12_RENDER_PASS_RENDER_TARGET_DESC*)AllocFromTemp(sizeof(D3D12_RENDER_PASS_RENDER_TARGET_DESC) * rtvCount, alignof(D3D12_RENDER_PASS_RENDER_TARGET_DESC));

            D3D12_RENDER_PASS_DEPTH_STENCIL_DESC depthDesc;

            int rtvIndex = 0;

            int attachCount = currentRenderPassInstance->attachInstCount;

            int rpIndex = currentRenderPassInstance->baseRenderPassData;

            for (int j = 0; j < attachCount; j++)
            {
                AttachmentInstance* attachInst = &currentRenderPassInstance->attachInst[j];

                AttachmentDescription* desc = attachInst->descLayout;

                if (desc->attachType == AttachmentDescriptionType::DEPTHATTACH)
                {
                    DX12CPUDescriptorHandle dsvHandle = deviceInstance.GetCPUHandleFromDescriptorManager(globalDSVDescriptorHeap, renderPassesHandles[rpIndex + 1] + currentImageIndex);

                    CreateDepthDesc(&depthDesc, dsvHandle, attachInst->clear, currGraphInstance->graphLayout->resources[desc->resourceIndex].format);

                    depthDesc.DepthEndingAccess.Type = ConvertAttachmentStoreUsageToEndingAccess(desc->storeOp);;

                    depthDesc.StencilBeginningAccess.Type = D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_NO_ACCESS;
                    depthDesc.StencilEndingAccess.Type = D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_NO_ACCESS;

                    commandRecorder.TransitionImageResource(currGraphInstance->resources[attachInst->attachmentResource].attachmentImage[0][currentImageIndex],
                        D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS,
                        D3D12_BARRIER_SYNC_DEPTH_STENCIL, D3D12_BARRIER_ACCESS_DEPTH_STENCIL_WRITE,
                        ConvertToBarrierLayout(ImageLayout::GENERAL_LAYOUT), ConvertToBarrierLayout(ImageLayout::DEPTHSTENCILATTACHMENT),
                        0, 1, 0, 1
                    );

                }
                else if (desc->attachType == AttachmentDescriptionType::COLORATTACH)
                {
                    DX12CPUDescriptorHandle rtvHandle = deviceInstance.GetCPUHandleFromDescriptorManager(globalRTVDescriptorHeap, renderPassesHandles[rpIndex] + (currentImageIndex * rtvCount));
                    CreateRenderTargetDesc(&rtvDescs[rtvIndex], rtvHandle, attachInst->clear, currGraphInstance->graphLayout->resources[desc->resourceIndex].format);
                    rtvDescs[rtvIndex].EndingAccess.Type = ConvertAttachmentStoreUsageToEndingAccess(desc->storeOp);


                    commandRecorder.TransitionImageResource(currGraphInstance->resources[attachInst->attachmentResource].attachmentImage[0][currentImageIndex],
                        D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS,
                        D3D12_BARRIER_SYNC_RENDER_TARGET, D3D12_BARRIER_ACCESS_RENDER_TARGET,
                        ConvertToBarrierLayout(ImageLayout::PRESENT), ConvertToBarrierLayout(ImageLayout::COLORATTACHMENT),
                        0, 1, 0, 1
                    );

                    rtvIndex++;

                }
            }


            deviceInstance.BeginRenderPass(rtvDescs, rtvCount, &depthDesc, depthStencilCount, &commandRecorder);

            D3D12_VIEWPORT viewport{};
            viewport.Width = (FLOAT)800;
            viewport.Height = (FLOAT)600;
            viewport.MinDepth = 0.0f;
            viewport.MaxDepth = 1.0f;

            D3D12_RECT scissor{};
            scissor.left = 0;
            scissor.top = 0;
            scissor.right = 800;
            scissor.bottom = 600;

            commandRecorder.SetScissor(1, &scissor);
            commandRecorder.SetViewports(1, &viewport);

            for (int i = 0; i < 2; i++)
            {
                DX12GraphicsPipelineObject* obj = (DX12GraphicsPipelineObject*)deviceInstance.GetAndValidateItem(storedRenderables[i], D12PIPELINEOBJECT);
                obj->DrawObject(&commandRecorders[currentFrame], currentFrame);
            }

            commandRecorder.EndRenderPass();

        }

        for (int i = 0; i < numberOfRenderPasses; i++)
        {
            AttachmentRenderPass* currentRenderPassDesc = &currGraphInstance->graphLayout->holders[i];

            AttachmentRenderPassInstance* currentRenderPassInstance = &currGraphInstance->passes[i];

            int attachCount = currentRenderPassInstance->attachInstCount;

            for (int j = 0; j < attachCount; j++)
            {
                AttachmentInstance* attachInst = &currentRenderPassInstance->attachInst[j];

                AttachmentDescription* desc = attachInst->descLayout;

                if (desc->attachType == AttachmentDescriptionType::DEPTHATTACH)
                {
                    commandRecorder.TransitionImageResource(currGraphInstance->resources[attachInst->attachmentResource].attachmentImage[0][currentImageIndex],
                        D3D12_BARRIER_SYNC_DEPTH_STENCIL, D3D12_BARRIER_ACCESS_DEPTH_STENCIL_WRITE, D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS,
                        ConvertToBarrierLayout(ImageLayout::DEPTHSTENCILATTACHMENT), ConvertToBarrierLayout(ImageLayout::GENERAL_LAYOUT),
                        0, 1, 0, 1
                    );
                }
                else if (desc->attachType == AttachmentDescriptionType::COLORATTACH)
                {
                    commandRecorder.TransitionImageResource(currGraphInstance->resources[attachInst->attachmentResource].attachmentImage[0][currentImageIndex],
                        D3D12_BARRIER_SYNC_RENDER_TARGET, D3D12_BARRIER_ACCESS_RENDER_TARGET,
                        D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS,
                        ConvertToBarrierLayout(ImageLayout::COLORATTACHMENT), ConvertToBarrierLayout(desc->dstLayout),
                        0, 1, 0, 1
                    );
                }
            }
        }
    }
    
    if (commandRecorder.CloseCommandBuffer())
    {
        printf("Cannot finish recording command buffer\n");
        return -1;
    }


    commandLists[cmdListIndex++] = commandRecorder.cmdList;

    deviceInstance.ExecuteCommandListsOnQueue(queueHandle, commandLists, cmdListIndex);
 
    deviceInstance.PresentSwapChainImage(swapChain, 0, 0);

    g_FrameFenceValues[currentFrame] = deviceInstance.Signal(queueHandle, globalRendererFence);

    currentFrame = (currentFrame + 1) % MAX_FRAMES_IN_FLIGHT;

    return 0;
}

void WriteToDeviceLocalMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies)
{
    
	RenderAllocation* alloc = &allocationHandle[allocationIndex];

    size_t stride = ((alloc->requestedSize * alloc->structureCopies) + alloc->alignment - 1) & ~(alloc->alignment - 1);

    deviceInstance.WriteToDeviceLocalMemory(alloc->memIndex, stagingBuffers[currentFrame], &transferCommandRecorder, data, size, alloc->offset + offset, stride, copies);

	transferCommandsUploaded++;
}

void WriteToImageDeviceLocalMemory(EntryHandle imageHandle, char* data, UINT width, UINT height, UINT componentCount, UINT totalImageSize, DXGI_FORMAT format, UINT mipLevels, UINT layers)
{

    deviceInstance.WriteToImageDeviceLocalMemory(imageHandle, &transferCommandRecorder, stagingBuffers[currentFrame],
        data, width, height, componentCount, totalImageSize, format, mipLevels, layers
    );

    transferCommandsUploaded++;
}

void WriteToHostMemory(int allocationIndex, void* data, size_t size, size_t offset, int copies)
{
    void* mappedData = nullptr;

    RenderAllocation* alloc = &allocationHandle[allocationIndex];

    size_t stride = ((alloc->requestedSize * alloc->structureCopies) + alloc->alignment - 1) & ~(alloc->alignment - 1);

    deviceInstance.WriteToHostMemory(alloc->memIndex, data, size, alloc->offset + offset, stride, copies);
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
    UINT numDescriptorsTables = graph->resourceSetCount, numRootParameters = graph->resourceSetCount;

    UINT samplerCount = 0, numOfRanges = 0, constantCount = 0;

    UINT rootParamOffset = 0;

    for (int i = 0; i < graph->resourceSetCount; i++)
    {
        ShaderResourceSetTemplate* layout = (ShaderResourceSetTemplate*)graph->GetSet(i);
        samplerCount += layout->samplerCount;
        constantCount += layout->constantsCount;
        numOfRanges += layout->viewCount;
    }

    if (samplerCount)
    {
        numDescriptorsTables++;
        numRootParameters++;
        numOfRanges++;
    }

    if (constantCount)
    {
        numRootParameters++;
        rootParamOffset++;
    }

    DX12RootSignatureCreate createInfo{};

    createInfo.numOfRootParameters = numRootParameters;

    createInfo.rootParameters = (D3D12_ROOT_PARAMETER*)AllocFromTemp(sizeof(D3D12_ROOT_PARAMETER) * numRootParameters, 4);

    UINT* rangeCount = (UINT*)AllocFromTemp(sizeof(UINT) * numRootParameters, 4);
    memset(rangeCount, 0, sizeof(UINT) * numRootParameters);
    
    DX12DescriptorTableRanges ranges{};

    ranges.numOfRanges = numOfRanges;
    ranges.ranges = (D3D12_DESCRIPTOR_RANGE*)AllocFromTemp(sizeof(D3D12_DESCRIPTOR_RANGE) * numOfRanges, 4);

    ShaderStageType* visibility = (ShaderStageType*)AllocFromTemp(sizeof(ShaderStageType) * numRootParameters, 4);

    UINT samplerIndex = 0, srvIndex = 0, cbvIndex = constantCount, uavIndex = 0, samplerRangeParameterIndex = numOfRanges-samplerCount;

    UINT inlineConstantBlockRootIndex = 0, constantBlockIndex = 0, descriptorTableRootParams = constantCount;

    for (int i = 0; i < graph->resourceCount; i++)
    {
        ShaderResource* resource = (ShaderResource*)graph->GetResource(i);

        int arrayCount = resource->arrayCount;

        switch (resource->type)
        {
		case ShaderResourceType::CONSTANT_BUFFER:
		{
     
			D3D12_SHADER_VISIBILITY visible = D3D12_SHADER_VISIBILITY_ALL;


			switch (resource->stages)
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

			createInfo.CreateConstants(inlineConstantBlockRootIndex++, constantBlockIndex++, 0, resource->size/4, visible);
			break;
		}
        case ShaderResourceType::IMAGE2D:
        {
            ranges.AppendSRVRange(arrayCount, srvIndex, 0);

            srvIndex += arrayCount;

            rangeCount[resource->set] += 1;
            visibility[resource->set] |= resource->stages;
            break;
        }
        case ShaderResourceType::SAMPLERSTATE:
        {
            ranges.CreateSamplerRange(samplerRangeParameterIndex++, arrayCount, samplerIndex, 0);

            samplerIndex += arrayCount;

            visibility[numRootParameters - 1] |= resource->stages;
            rangeCount[numRootParameters - 1] += 1;
            break;
        }
        case ShaderResourceType::BUFFER_VIEW:
        case ShaderResourceType::STORAGE_BUFFER:
        {
            if (resource->action == ShaderResourceAction::SHADERWRITE || resource->action == ShaderResourceAction::SHADERREADWRITE)
            {
                ranges.AppendUAVRange(arrayCount, uavIndex, 0);
                uavIndex += arrayCount;
            }
            else
            {
                ranges.AppendSRVRange(arrayCount, srvIndex, 0);
                srvIndex += arrayCount;
            }

            rangeCount[resource->set] += 1;
            visibility[resource->set] |= resource->stages;
            break;
        }
        case ShaderResourceType::UNIFORM_BUFFER:
        {
            ranges.AppendCBVRange(arrayCount,cbvIndex, 0);

            cbvIndex += arrayCount;

            rangeCount[resource->set] += 1;
            visibility[resource->set] |= resource->stages;
            break;
        }
        }
    }



    UINT rootParamIndex = rootParamOffset, rangeIterIndex = 0;

    for (int i = 0; i < graph->resourceSetCount; i++)
    {
        ShaderResourceSetTemplate* layout = (ShaderResourceSetTemplate*)graph->GetSet(i);

        D3D12_SHADER_VISIBILITY visible = D3D12_SHADER_VISIBILITY_ALL;

        if (!(visibility[i] & (visibility[i] - 1)))
        {
            switch (visibility[i])
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

    int nRet = OSOpenFile(name, (int)strnlen(name, 250), openingFlags, &outHandle);

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

int CreateCBVArgsFromResourceSet(int descriptorid, int rootParamId, DX12ConstantBufferPipelineArguments* args)
{

    DX12ConstantBufferPipelineArguments* headPtr = args;

    uintptr_t head = descriptorSets[descriptorid];

    ShaderResourceSet* set = (ShaderResourceSet*)head;

    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    int bindingCount = set->bindingCount;
    

    for (int g = 0; g < bindingCount; g++)
    {
        ShaderResourceHeader* header = (ShaderResourceHeader*)offsets[g];

        int arrayCount = header->arrayCount;

        switch (header->type)
        {

        case ShaderResourceType::CONSTANT_BUFFER:
        {
            ShaderResourceConstantBuffer* buffer = (ShaderResourceConstantBuffer*)header;
            args->data = buffer->data;
            args->rootParamIndex = rootParamId;
            args->sizeInBytes = buffer->size;
            args->offsetInBytes = buffer->offset;
            args++;
            break;
        }
        }
    }

    return (int)std::distance(headPtr, args);
}



int CreateTablesFromResourceSet(int* descriptorsets, int numDescriptorSet, int rootParamOffset, DX12DescriptorTableBindings* tableBindings)
{
    int samplerCount = 0;

    int descriptorTableCount = numDescriptorSet;

    DX12DescriptorHeapManager* mainHeap = (DX12DescriptorHeapManager*)deviceInstance.GetAndValidateItem(mainSRVDescriptorHeap, D12DESCRIPTORMANAGER);

    DX12DescriptorHeapManager* samplerHeap = (DX12DescriptorHeapManager*)deviceInstance.GetAndValidateItem(mainSamplerDescriptorHeap, D12DESCRIPTORMANAGER);

    DX12DescriptorHeapManager* stageSamplerHeap = (DX12DescriptorHeapManager*)deviceInstance.GetAndValidateItem(stagingSamplerDescriptorHeap, D12DESCRIPTORMANAGER);

    int samplerIndex = stageSamplerHeap->descriptorHeapHandlePointer;

    int maxSamplerFrameCount = 0;

    for (int i = 0; i < numDescriptorSet; i++)
    {
        int descriptorid = descriptorsets[i];

        tableBindings[i].descriptorHeapSelection = 0;

        uintptr_t head = descriptorSets[descriptorid];
        ShaderResourceSet* set = (ShaderResourceSet*)head;

        if (srvDescriptorTablesStart[descriptorid] != -1)
        {
            tableBindings[i].descriptorHeapBase = srvDescriptorTablesStart[descriptorid];
            tableBindings[i].descriptorsCount = set->viewCount;
            continue;
        }


        uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

        int bindingCount = set->bindingCount;

        int numberOfTables = set->setCount;

        maxSamplerFrameCount = max(maxSamplerFrameCount, numberOfTables);

        int mainHeapIndex = mainHeap->descriptorHeapHandlePointer;

        int bufferCounts = set->viewCount;

        samplerCount += set->samplerCount;

        for (int g = 0; g < bindingCount; g++)
        {
            ShaderResourceHeader* header = (ShaderResourceHeader*)offsets[g];

            int arrayCount = header->arrayCount;

            switch (header->type)
            {
            case ShaderResourceType::IMAGE2D:
            {
                ShaderResourceImage* image = (ShaderResourceImage*)header;

                for (int j = 0; j < numberOfTables; j++)
                {
                    for (int h = 0; h < arrayCount; h++)
                    {
                        deviceInstance.CreateImageSRVDescriptor(
                            image->textureHandles[h],
                            0,
                            mainHeap, mainHeapIndex + (j * bufferCounts) + h
                        );
                    }

                }
                mainHeapIndex += arrayCount;
                break;
            }
            case ShaderResourceType::SAMPLERSTATE:
            {
                ShaderResourceSampler* samplers = (ShaderResourceSampler*)header;
                for (int h = 0; h < arrayCount; h++)
                {
                    deviceInstance.CreateSamplerDescriptor(samplers->samplerHandles[h], stageSamplerHeap, samplerIndex++);
                }
                break;
            }
            case ShaderResourceType::CONSTANT_BUFFER:
            case ShaderResourceType::SAMPLER3D:
            case ShaderResourceType::SAMPLER2D:
            case ShaderResourceType::SAMPLERCUBE:
            {
               // printf("Unimplemented! %d\n", header->type);

                break;
            }

            case ShaderResourceType::STORAGE_BUFFER:
            {
                ShaderResourceBuffer* buffer = (ShaderResourceBuffer*)header;
               

                for (int j = 0; j < numberOfTables; j++)
                {
                    for (int h = 0; h < arrayCount; h++)
                    {

                        auto alloc = &allocationHandle[buffer->allocationIndex[h]];
                        size_t stride = (alloc->requestedSize + alloc->alignment - 1) & ~(alloc->alignment - 1);

                        if (buffer->action == ShaderResourceAction::SHADERWRITE || buffer->action == ShaderResourceAction::SHADERWRITE)
                        {
                            deviceInstance.CreateBufferUAVDescriptor(alloc->memIndex, alloc->viewIndex, j, mainHeap, mainHeapIndex + (j * bufferCounts) + h);
                        }
                        else
                        {
                            deviceInstance.CreateBufferSRVDescriptor(alloc->memIndex, alloc->viewIndex, j, mainHeap, mainHeapIndex + (j * bufferCounts) + h);
                        }
                    }
                }


                mainHeapIndex += arrayCount;
                break;
            }
            case ShaderResourceType::UNIFORM_BUFFER:
            {
                ShaderResourceBuffer* buffer = (ShaderResourceBuffer*)header;

                for (int j = 0; j < numberOfTables; j++)
                {
                    for (int h = 0; h < arrayCount; h++)
                    {

                        auto alloc = &allocationHandle[buffer->allocationIndex[h]];
                       
                        deviceInstance.CreateBufferCBVDescriptor
                        (
                            alloc->memIndex,
                            alloc->viewIndex,
                            j,
                            mainHeap,
                            mainHeapIndex + (j * bufferCounts) + h
                        );
                    }
                }

                mainHeapIndex += arrayCount;

                break;
            }


            case ShaderResourceType::BUFFER_VIEW:
            {
                ShaderResourceBufferView* bufferView = (ShaderResourceBufferView*)header;

                for (int j = 0; j < numberOfTables; j++)
                {
                    for (int h = 0; h < arrayCount; h++)
                    {

                        RenderAllocation* alloc = &allocationHandle[bufferView->allocationIndex[h]];

                        size_t stride = (alloc->requestedSize + alloc->alignment - 1) & ~(alloc->alignment - 1);

                        if (bufferView->action == ShaderResourceAction::SHADERWRITE || bufferView->action == ShaderResourceAction::SHADERWRITE)
                        {
                            deviceInstance.CreateBufferUAVDescriptor(alloc->memIndex, alloc->viewIndex, j, mainHeap, mainHeapIndex + (j * bufferCounts) + h);
                        }
                        else
                        {
                            deviceInstance.CreateBufferSRVDescriptor(alloc->memIndex, alloc->viewIndex, j, mainHeap, mainHeapIndex + (j * bufferCounts) + h);
                        }
                    }
                }
                mainHeapIndex += arrayCount;
            }
            }
        }

        tableBindings[i].descriptorHeapBase = srvDescriptorTablesStart[descriptorid] = mainHeap->descriptorHeapHandlePointer;
        tableBindings[i].descriptorsCount = bufferCounts;

        mainHeap->descriptorHeapHandlePointer += (bufferCounts * numberOfTables);
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

        tableBindings[numDescriptorSet].descriptorHeapBase = samplerHeap->descriptorHeapHandlePointer;
        tableBindings[numDescriptorSet].descriptorsCount =  samplerCount;
        tableBindings[numDescriptorSet].descriptorHeapSelection = 1;
        
        samplerHeap->descriptorHeapHandlePointer += (samplerCount * maxSamplerFrameCount);

        descriptorTableCount++;
    }

    return descriptorTableCount;
}










