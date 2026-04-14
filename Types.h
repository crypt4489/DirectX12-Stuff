#pragma once
#include <string>
#include <array>
#include "Files.h"
#pragma once
#include <cstdint>

typedef size_t EntryHandle;

#define KiB 1024
#define MiB 1024 * KiB
#define GiB 1024 * MiB


#define UNBOUNDED_DESCRIPTOR_ARRAY ((uint32_t)1 << 31)
#define DESCRIPTOR_COUNT_MASK 0x7FFFFFFF

/* Rendering State Types */

enum RenderingBackend
{
    VULKAN = 1,
    DXD12 = 2,
};

enum RasterizerTest
{
    NEVER = 0,
    LESS = 1,
    EQUAL = 2,
    LESSEQUAL = 3,
    GREATER = 4,
    NOTEQUAL = 5,
    GREATEREQUAL = 6,
    ALLPASS = 7
};

enum ImageFormat
{
    X8L8U8V8 = 0,
    DXT1 = 1,
    DXT3 = 2,
    R8G8B8A8 = 3,
    B8G8R8A8 = 4,
    D24UNORMS8STENCIL = 5,
    D32FLOATS8STENCIL = 6,
    D32FLOAT = 7,
    R8G8B8A8_UNORM = 8,
    R8G8B8 = 9,
    B8G8R8A8_UNORM = 10,
    IMAGE_UNKNOWN = 0x7fffffff
};

enum TextureIOType
{
    BMP = 0,
};

enum PrimitiveType
{
    TRIANGLES = 0,
    TRISTRIPS = 6,
    TRIFAN = 7,
    POINTSLIST = 8,
    LINELIST = 9,
    LINESTRIPS = 10
};

enum class ImageLayout
{
    UNDEFINED = 0,
    WRITEABLE = 1,
    SHADERREADABLE = 2,
    COLORATTACHMENT = 3,
    DEPTHSTENCILATTACHMENT = 4,
    PRESENT = 5,
    DEPTHATTACHMENT = 6,
    STENCILATTACHMENT = 7,
    GENERAL_LAYOUT = 8
};

enum class ImageUsage
{
    DEPTHSTENCIL = 0,
    COLOR = 1
};


/* Render Management Types */

enum class AllocationType
{
    STATIC = 0,
    PERFRAME = 1,
    PERDRAW = 2
};

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


enum class TransferType
{
    CACHED = 0,
    MEMORY = 1,
};


/* Shader Resource Definitions */

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

enum class ShaderResourceType
{
    SAMPLER2D = 1,
    STORAGE_BUFFER = 2,
    UNIFORM_BUFFER = 4,
    CONSTANT_BUFFER = 8,
    IMAGESTORE2D = 16,
    IMAGESTORE3D = 32,
    IMAGE2D = 33,
    IMAGE3D = 34,
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

struct ShaderResourceSetTemplate
{
    int vulkanDescLayout;
    int dx12DescriptorTable;
    int bindingCount;
    int resourceStart;
    int samplerCount;
    int viewCount;
    int constantsCount;
    int constantStageCount;
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
    int rangeIndex;
};

struct ShaderResourceSet
{
    int bindingCount;
    int layoutHandle;
    int setCount;
    int barrierCount;
    int samplerCount;
    int viewCount;
    int constantsCount;
    int constantStageCount;
};

struct ShaderResourceHeader
{
    ShaderResourceType type;
    ShaderResourceAction action;
    int binding;
    int arrayCount;
};

struct ShaderResourceSampler : public ShaderResourceHeader
{
    EntryHandle* samplerHandles;
    int samplerCount;
    int firstSampler;
};

struct ShaderResourceImage : public ShaderResourceHeader
{
    EntryHandle* textureHandles;
    int textureCount;
    int firstTexture;
};

struct ShaderResourceBuffer : public ShaderResourceHeader
{
    int* allocationIndex;
    int* offsets;
    int firstBuffer;
    int bufferCount;
};

struct ShaderResourceBufferView : public ShaderResourceHeader
{
    int* allocationIndex;
    int firstView;
    int viewCount;
};


struct ShaderResourceConstantBuffer : public ShaderResourceHeader
{
    ShaderStageType stage;
    int size;
    int offset;
    int rangeindex;
    void* data;
};

struct ShaderComputeLayout
{
    unsigned long x;
    unsigned long y;
    unsigned long z;
};

/* Shader Resource Update */

struct ShaderResourceUpdate
{
    ShaderResourceType type;
    int descriptorSet;
    int bindingIndex;
    int copyCount;
    void* data;
    int dataSize;
};

struct DeviceHandleArrayUpdate
{
    int resourceDstBegin;
    int resourceCount;
    EntryHandle* resourceHandles;
};

struct BufferArrayUpdate
{
    int resourceDstBegin;
    int allocationCount;
    int* allocationIndices;
};



/* Intermediary Pipeline Object */

struct GraphicsIntermediaryPipelineInfo
{
    uint32_t drawType;
    int vertexBufferHandle;
    uint32_t vertexCount;
    uint32_t pipelinename;
    uint32_t descCount;
    int* descriptorsetid;
    int indexBufferHandle;
    uint32_t indexCount;
    uint32_t instanceCount;
    uint32_t indexSize;
    uint32_t indexOffset;
    uint32_t vertexOffset;
    int indirectAllocation;
    int indirectDrawCount;
    int indirectCountAllocation;
};

struct ComputeIntermediaryPipelineInfo
{
    uint32_t x;
    uint32_t y;
    uint32_t z;
    uint32_t pipelinename;
    uint32_t descCount;
    int* descriptorsetid;
};



/* Host memory update */

struct BufferMemoryTransferRegion
{
    void* data;
    int size;
    int copyCount;
    int allocationIndex;
    int allocoffset;
};

struct TextureMemoryRegion
{
    void* data;
    uint32_t* imageSizes;
    size_t totalSize;
    EntryHandle textureIndex;
    int width;
    int height;
    int mipLevels;
    int layers;
    ImageFormat format;
};

struct TransferCommand
{
    int fillVal;
    int size;
    int offset;
    int allocationIndex;
    int copycount;
    BarrierStage dstStage;
    BarrierAction dstAction;
};

/* Allocation management */
struct RenderAllocation
{
    size_t offset;
    size_t deviceAllocSize;
    size_t requestedSize;
    size_t alignment;
    EntryHandle viewIndex;
    AllocationType allocType;
    ComponentFormatType formatType;
    int structureCopies;
    int memIndex;
};


/* */

enum class VertexUsage : size_t
{
    POSITION = 0,
    TEX0 = 1,
    TEX1 = 2,
    TEX2 = 3,
    TEX3 = 4,
    NORMAL = 5,
    BONES = 6,
    WEIGHTS = 7,
    COLOR0 = 8,
    TANGENTS = 9,
    NUM_VERTEX_FORMAT
};

enum class VertexBufferRate
{
    PERVERTEX = 0,
    PERINSTANCE = 1,
};

struct VertexInputDescription
{
    ComponentFormatType format;
    int byteoffset;
    VertexUsage vertexusage;
};

struct VertexBufferDescription
{
    VertexBufferRate rate;
    int descCount;
    int perInputSize;
    VertexInputDescription descriptions[10];
};

enum TriangleWinding
{
    CW = 0,
    CCW = 1
};

enum class CullMode
{
    CULL_NONE = 0,
    CULL_BACK = 1,
    CULL_FRONT = 2,
};

enum class BlendOp
{
    LOGIC_COPY = 1
};

enum class StencilOp
{
    REPLACE = 0,
    KEEP = 1,
    ZERO = 2,
};

struct FaceStencilData
{
    StencilOp failOp;
    StencilOp passOp;
    StencilOp depthFailOp;
    RasterizerTest stencilCompare;
    int writeMask;
    int compareMask;
    int reference;
};

struct GenericPipelineStateInfo
{
    PrimitiveType primType;
    float lineWidth;
    TriangleWinding windingOrder;
    bool depthEnable;
    bool depthWrite;
    RasterizerTest depthTest;
    bool StencilEnable;
    FaceStencilData frontFace;
    FaceStencilData backFace;
    int sampleCountLow;
    int sampleCountHigh;
    ImageFormat colorFormat;
    ImageFormat depthFormat;
    BlendOp blendOp;
    CullMode cullMode;
    int vertexBufferDescCount;
    VertexBufferDescription vertexBufferDesc[4];
};

enum AppPipelineHandleType
{
    COMPUTESO,
    GRAPHICSO,
    INDIRECTSO,
};

struct PipelineHandle
{
    int group;
    int indexForHandles;
    int numHandles;
    int pipelineIdentifierGroup;
};

enum class AttachmentDescriptionType
{
    COLORATTACH = 0,
    RESOLVEATTACH = 1,
    DEPTHATTACH = 2,
    STENCILATTACH = 3,
    DEPTHSTENCILATTACH = 4,
};

enum class AttachmentViewType
{
    SWAPCHAIN = 1,
    STATIC = 2,
};

enum class AttachmentLoadUsage
{
    ATTACHNOCARE = 1,
    ATTACHCLEAR = 2,
};

enum class AttachmentStoreUsage
{
    ATTACHDISCARD = 1,
    ATTACHSTORE = 2
};


struct AttachmentResource
{
    AttachmentViewType viewType;
    ImageFormat format;
    int msaa;
};

struct AttachmentDescription
{
    AttachmentDescriptionType attachType;
    AttachmentLoadUsage loadOp;
    AttachmentStoreUsage storeOp;
    ImageLayout srcLayout;
    ImageLayout dstLayout;
    int resourceIndex;
};

enum class RenderPassType
{
    SWAPCHAIN_IMAGE_COUNT = 1,
    PER_FRAME_IMAGE_COUNT = 2
};

struct AttachmentRenderPass
{
    int attachmentCount;
    int resolveCount;
    int depthStencilCount;
    int colorCount;
    AttachmentDescription descs[8];
};

struct AttachmentGraph
{
    int passesCount;
    int resourceCount;
    AttachmentRenderPass holders[4];
    AttachmentResource resources[12];
};

enum class AttachmentResourceInstanceUsage
{
    COLOR_ATTACHMENT_USAGE = 1,
    DEPTH_ATTACHMENT_USAGE = 2,
    STENCIL_ATTACHMENT_USAGE = 4,
    RESOLVE_ATTACHMENT_USAGE = 8,
    PRESERVE_ATTACHMENT_USAGE = 16,
    INPUT_ATTACHMENT_USAGE = 32,
    DEPTH_STENCIL_ATTACHMENT_USAGE = 64,
};

struct AttachmentResourceInstance
{
    EntryHandle** attachmentImage;
    EntryHandle** attachmentImageView;
    AttachmentResourceInstanceUsage usage;
    int sampLo;
    int sampHi;
    int imageCount;
};

enum RPClearType
{
    NOCLEAR = 0,
    CLEARCOLOR = 1,
    CLEARDEPTH = 2
};

union ClearVal {
    struct {
        float cdata[4];
    };
    struct {
        float ddata;
        uint32_t sdata;
    };
};

struct AttachmentClear
{
    RPClearType type;
    ClearVal val;
};

struct AttachmentInstance
{
    AttachmentDescription* descLayout;
    int attachmentResource;
    AttachmentClear clear;
};

struct AttachmentRenderPassInstance
{
    AttachmentInstance* attachInst;
    int attachInstCount;
    int maxSampleCount;
    int baseRenderTargetData;
    int baseRenderPassData;
    int currentSampleCount;
    int graphicsOTQIndex;
    RenderPassType rpType;

};

struct AttachmentGraphInstance
{
    AttachmentGraph* graphLayout;
    AttachmentResourceInstance* resources;
    AttachmentRenderPassInstance* passes;
    int consecutiveRenderPassBase;
    int consecutiveRenderTargetsBase;
};



struct PipelineInstanceData
{
    int frameGraphIndices[4];
    int frameGraphRenderPasses[4];
    int frameGraphPipelineIndices[4];
    int frameGraphCount;
    int pipelineCount;
};

enum GPUCommandStreamType
{
    ATTACHMENT_COMMANDS = 1,
    COMPUTE_QUEUE_COMMANDS = 2,
};

struct GPUCommand
{
    GPUCommandStreamType streamType;
    int indexForStreamType;
};

enum VertexComponents
{
    POSITION = 1,
    TEXTURE0 = 2,
    TEXTURE1 = 4,
    TEXTURE2 = 8,
    NORMAL = 16,
    BONES2 = 32,
    COLOR = 64,
    TANGENT = 128,
    COMPRESSED = 0x80000000,
};

constexpr float dx = 3.051851e-05f;
constexpr float ax = 0.0009770395f;
constexpr float bx = 0.0019550342f;


enum BufferType
{
    HOST_MEMORY_TYPE = 1,
    DEVICE_MEMORY_TYPE = 2,
};

enum BufferAlignmentType
{
    NO_BUFFER_ALIGNMENT = 0,
    UNIFORM_BUFFER_ALIGNMENT = 1,
    STORAGE_BUFFER_ALIGNMENT = 2,
};

struct StringView
{
    char* stringData;
    int charCount;
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

struct ShaderDetails
{
    int shaderNameSize;
    int shaderDataSize;

    ShaderDetails* GetNext();

    char* GetString();

    void* GetShaderData();
};

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
    int pushRangeStage;
};

struct ShaderResourceSetXMLTag : ShaderXMLTag
{
    int resourceCount;
};

static constexpr unsigned long
hash(char* str);

static constexpr unsigned long
hash(const std::string& string);

static int ProcessTag(char* fileData, int size, int currentLocation, unsigned long* hash, bool* opening);

static int SkipLine(char* fileData, int size, int currentLocation);
static int ReadValue(char* fileData, int size, int currentLocation, char* str, int* len);

static int ReadAttributeName(char* fileData, int size, int currentLocation, unsigned long* hash);

static int ReadAttributeValueHash(char* fileData, int size, int currentLocation, unsigned long* hash);

static int ReadAttributeValueVal(char* fileData, int size, int currentLocation, unsigned long* val);

static int ReadAttributes(char* fileData, int size, int currentLocation, unsigned long* hashes, int* stackSize);

static int HandleShaderResourceItem(char* fileData, int size, int currentLocation, uintptr_t* offset);

static constexpr int ASCIIToInt(char* str);

static int HandleComputeLayout(char* fileData, int size, int currentLocation, uintptr_t* offset);



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



extern std::array<int, 50> srvDescriptorTablesStart;
extern std::array<uintptr_t, 50> descriptorSets;
/*
void SetVariableArrayCount(int descriptorSet, int bindingIndex, int varArrayCount)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;

    uintptr_t* setOffsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceHeader* header = (ShaderResourceHeader*)setOffsets[bindingIndex];

    if (!(header->arrayCount & UNBOUNDED_DESCRIPTOR_ARRAY))
    {
        //do something
    }

    header->arrayCount = (header->arrayCount & UNBOUNDED_DESCRIPTOR_ARRAY) | (varArrayCount & DESCRIPTOR_COUNT_MASK);

}
*/

int AllocateShaderResourceSet(ShaderGraph* graph, uint32_t targetSet, int setCount);
void UploadConstant(int descriptorset, void* data, int bufferLocation);
int GetConstantBufferCount(int descriptorSet);
ShaderResourceHeader* GetConstantBuffer(int descriptorSet, int constantBuffer);
void BindImageBarrier(int descriptorSet, int binding, int barrierIndex, BarrierStage stage, BarrierAction action, ImageLayout oldLayout, ImageLayout dstLayout, bool location);
void BindBarrier(int descriptorSet, int binding, BarrierStage stage, BarrierAction action);
void BindBufferView(int descriptorSet, int* allocationIndex, int firstView, int viewCount, int bindingIndex);
void BindSampledImageArrayToShaderResource(int descriptorSet, EntryHandle* indices, int texCount, int firstTexture, int bindingIndex);
void BindSampledImageToShaderResource(int descriptorSet, EntryHandle* index, int textureCount, int firstTexture, int bindingIndex);
void BindSamplerResourceToShaderResource(int descriptorSet, EntryHandle* indices, int samplerCount, int firstSampler, int bindingIndex);
void BindImageResourceToShaderResource(int descriptorSet, EntryHandle* index, int textureCount, int firstTexture, int bindingIndex);
void BindBufferToShaderResource(int descriptorSet, int* allocationIndex, int* offsets, int firstBuffer, int bufferCount, int bindingIndex);

#include <atomic>


template <typename T_AtomicType>
T_AtomicType UpdateAtomic(std::atomic<T_AtomicType>& atomic, T_AtomicType stride, T_AtomicType wrapAroundSize, T_AtomicType alignment)
{
    T_AtomicType val, desired, out;
    val = atomic.load(std::memory_order_relaxed);
    do {
        out = (val + alignment - 1) & ~(alignment - 1);

        if (wrapAroundSize && out >= wrapAroundSize)
        {
            out = 0;
        }

        desired = out + stride;

        if (wrapAroundSize && desired >= wrapAroundSize)
        {
            out = 0;
            desired = stride;
        }
    } while (!atomic.compare_exchange_weak(val, desired, std::memory_order_relaxed,
        std::memory_order_relaxed));

    return out;
}

template <typename T_AtomicType>
T_AtomicType UpdateAtomic(std::atomic<T_AtomicType>& atomic, T_AtomicType stride, T_AtomicType wrapAroundSize)
{
    T_AtomicType val, desired, out;
    val = atomic.load(std::memory_order_relaxed);
    do {
        out = val;
        desired = val + stride;
        if (wrapAroundSize && desired >= wrapAroundSize)
        {
            out = 0;
            desired = stride;
        }
    } while (!atomic.compare_exchange_weak(val, desired, std::memory_order_relaxed,
        std::memory_order_relaxed));

    return out;
}


struct RingAllocator
{
    void* dataHead;
    int dataSize;
    std::atomic<int> dataAllocator;
    constexpr RingAllocator(void* _dataHead, int _size) :
        dataSize(_size), dataAllocator(0), dataHead(_dataHead)
    {

    }
    void* Allocate(int _allocSize);
    void* Allocate(int _allocSize, int alignment);
    void Reset();
    void* Head();
    void* CAllocate(int _allocSize, int alignment);
    void* CAllocate(int _allocSize);

    StringView* AllocateFromNullString(const char* name);
    StringView AllocateFromNullStringCopy(const char* name);
};


struct SlabAllocator
{
    void* dataHead;
    int dataSize;
    std::atomic<int> dataAllocator;
    constexpr SlabAllocator(void* _dataHead, int _size) :
        dataSize(_size), dataAllocator(0), dataHead(_dataHead)
    {

    }

    void* Allocate(int _allocSize, int alignment);
    void* Allocate(int _allocSize);
    void Reset();
    void* Head();
    void* CAllocate(int _allocSize, int alignment);
    void* CAllocate(int _allocSize);

    StringView* AllocateFromNullString(const char* name);
    StringView AllocateFromNullStringCopy(const char* name);
};

struct DeviceSlabAllocator
{
    int dataSize;
    std::atomic<int> dataAllocator;
    constexpr DeviceSlabAllocator(int _size) :
        dataSize(_size), dataAllocator(0)
    {

    }
    int Allocate(int _allocSize, int alignment);
};


ShaderGraph* CreateShaderGraph(StringView filename, RingAllocator* readerMemory, SlabAllocator* graphAllocator, SlabAllocator* shaderAllocator, int* shaderDetailCount);

void CreatePipelineDescription(StringView filename, GenericPipelineStateInfo* stateInfo, RingAllocator* tempAllocator);

void CreateAttachmentGraphFromFile(StringView filename, AttachmentGraph* graph, RingAllocator* inputScratchAllocator);
static int ReadAttributesAttachments(char* fileData, int size, int currentLocation, unsigned long* hashes, int* stackSize);
static int HandleAttachment(char* fileData, int size, int currentLocation, AttachmentDescriptionType descType, AttachmentDescription* description);
static int HandleAttachmentDesc(char* fileData, int size, int currentLocation, AttachmentRenderPass* holder);
static int HandleAttachmentResource(char* fileData, int size, int currentLocation, AttachmentResource* resource);
