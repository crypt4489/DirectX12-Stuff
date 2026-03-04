#pragma once
#include <string>
#include <array>
#include "Files.h"
#pragma once
#include <cstdint>

typedef size_t EntryHandle;

/* common types */

#define KiB 1024
#define MiB 1024 * KiB
#define GiB 1024 * MiB

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
    DEPTHSTENCILATTACHMENT = 4
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

struct ShaderSetLayout
{
    int vulkanDescLayout;
    int dx12DescriptorTable;
    int bindingCount;
    int resourceStart;
    int samplerCount;
    int viewCount;
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
    int samplerCount;
    int viewCount;
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

struct ResourceArrayUpdate
{
    int dstBegin;
    int count;
    EntryHandle* handles;
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

struct HostTransferRegion
{
    TransferType type;
    int size;
    int copyCount;
    int allocationIndex;
    int allocoffset;
    void* data;
};

struct DeviceTransferRegion
{
    TransferType transferType;
    int size;
    int copyCount;
    int allocationIndex;
    int allocoffset;
    void* data;
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

struct TransferRegionLink
{
    int region;
    int next;
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
    EntryHandle memIndex;
    size_t offset;
    size_t deviceAllocSize;
    size_t requestedSize;
    size_t alignment;
    EntryHandle viewIndex;
    AllocationType allocType;
    ComponentFormatType formatType;
    int structureCopies;
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
    int graphIndex;
    int graphCount;
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



struct ShaderXMLTag
{
    unsigned long hashCode;
};
struct ShaderDetailsXMLTag : ShaderXMLTag //followed by shaderNameLen Bytes
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
static ShaderGraph* CreateShaderGraph(
    const std::string& filename,
    char* detailsData,
    ShaderDetails** details
);

static int ProcessTag(char* fileData, int size, int currentLocation, unsigned long* hash, bool* opening);

static int SkipLine(char* fileData, int size, int currentLocation);
static int ReadValue(char* fileData, int size, int currentLocation, char* str, int* len);

static int ReadAttributeName(char* fileData, int size, int currentLocation, unsigned long* hash);

static int ReadAttributeValueHash(char* fileData, int size, int currentLocation, unsigned long* hash);

static int ReadAttributeValueVal(char* fileData, int size, int currentLocation, unsigned long* val);

static int ReadAttributes(char* fileData, int size, int currentLocation, unsigned long* hashes, int* stackSize);

static int HandleShader(char* fileData, int size, int currentLocation, uintptr_t* offset, void* shaderData, int* shaderDataSize);

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



constexpr unsigned long
hash(char* str)
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
hash(const std::string& string)
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

char readerMemBuffer[64 * 1024];
size_t readerMemBufferAllocate = 0;


int ProcessTag(char* fileData, int size, int currentLocation, unsigned long* hash, bool* opening)
{
    int count = 0;
    char* data = fileData + currentLocation;

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

int SkipLine(char* fileData, int size, int currentLocation)
{
    int count = 0;
    char* data = fileData + currentLocation;

    while (currentLocation + count < size && data[count++] != '\n');
    data = fileData + currentLocation + count;
    return count;
}

int ReadValue(char* fileData, int size, int currentLocation, char* str, int* len)
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
int ReadAttributeName(char* fileData, int size, int currentLocation, unsigned long* hash)
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

int ReadAttributeValueHash(char* fileData, int size, int currentLocation, unsigned long* hash)
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

int ReadAttributeValueVal(char* fileData, int size, int currentLocation, unsigned long* val)
{
    int count = 0;
    char* data = fileData + currentLocation;

    unsigned long out = 0;
    bool readingVal = false;

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
            if (readingVal)
                break;
            continue;
        }

        if (c == '\"' || c == '\'')
        {
            if (readingVal)
                break;
            continue;
        }

        if (count == (MAX_ATTRIBUTE_LEN + currentLocation) || ((c - '0') < 0 || (c - '9') > 0)) {
            throw std::runtime_error("malformed xml attribute");
        }

        out = (out * 10) + (c - '0');
        readingVal = true;

    }

    *val = out;

    return count;
}

#define MAX_ATTRIBUTE_LINE_LEN 200

int ReadAttributes(char* fileData, int size, int currentLocation, unsigned long* hashes, int* stackSize)
{
    int count = 0;
    char* data = fileData + currentLocation;

    int ret = 0;
    char c = data[ret];

    while (c != '>' && ret < MAX_ATTRIBUTE_LINE_LEN && (currentLocation + ret) < size)
    {
        ret += ReadAttributeName(fileData, size, currentLocation + ret, &hashes[count]);

        switch (hashes[count])
        {
        case hash("type"):
        case hash("used"):
        case hash("rw"):
        {
            ret += ReadAttributeValueHash(fileData, size, currentLocation + ret, &hashes[count + 1]);
            break;
        }
        case hash("offset"):
        case hash("size"):
        case hash("count"):
        case hash("x"):
        case hash("y"):
        case hash("z"):
        {
            ret += ReadAttributeValueVal(fileData, size, currentLocation + ret, &hashes[count + 1]);
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



int HandleShader(char* fileData, int size, int currentLocation, uintptr_t* offset, void* shaderData, int* shaderDataSize)
{
    unsigned long hashes[6];

    int glslSize = 0;

    int ret = ReadAttributes(fileData, size, currentLocation, hashes, &glslSize);

    uintptr_t detailHead = (uintptr_t)shaderData;

    ShaderDetails* details = (ShaderDetails*)detailHead;

    details->shaderDataSize = 0;
    details->shaderNameSize = 0;

    detailHead += sizeof(ShaderDetails);

    ShaderDetailsXMLTag* tag = (ShaderDetailsXMLTag*)&readerMemBuffer[readerMemBufferAllocate];

    *offset = (uintptr_t)tag;

    tag->hashCode = hash("HLSLShader");

    int stackIter = 0;

    while (glslSize > stackIter)
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
            throw std::runtime_error("Failed Type of Shader");
            break;
        }

        stackIter += 2;
    }

    readerMemBufferAllocate += sizeof(ShaderDetailsXMLTag);

    ret += ReadValue(fileData, size, currentLocation + ret, details->GetString(), &details->shaderNameSize);

    *shaderDataSize = (sizeof(ShaderDetails) + details->shaderNameSize + details->shaderDataSize);

    return ret;
}

int HandleShaderResourceItem(char* fileData, int size, int currentLocation, uintptr_t* offset)
{
    unsigned long hashes[12];

    int dataSize = 0;

    int ret = ReadAttributes(fileData, size, currentLocation, hashes, &dataSize);

    ShaderResourceItemXMLTag* tag = (ShaderResourceItemXMLTag*)&readerMemBuffer[readerMemBufferAllocate];

    *offset = (uintptr_t)tag;

    tag->hashCode = hash("ShaderResourceItem");

    tag->arrayCount = 1;

    int stackIter = 0;

    while (dataSize > stackIter)
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
            case hash("storageImage2D"):
                tag->resourceType = ShaderResourceType::IMAGESTORE2D;
                break;
            case hash("image2D"):
                tag->resourceType = ShaderResourceType::IMAGE2D;
                tag->resourceAction = ShaderResourceAction::SHADERREAD;
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


constexpr int ASCIIToInt(char* str)
{
    int c;
    int out = 0;

    while (c = *str++) {
        if ((c - '0') >= 0 && (c - '9') <= 0)
            out = (out * 10) + (c - '0');
    }

    return out;
}

static char AllocateDSMemory[16 * 1024];
uintptr_t DSAllocator = 0;

std::array<uintptr_t, 50> descriptorSets;
static int DSIndex = 0;

constexpr std::array<int, 50> makeArray(int val) {
    std::array<int, 50> arr{};
    std::fill(arr.begin(), arr.end(), val);
    return arr;
}

std::array<int, 50> srvDescriptorTablesStart = makeArray(-1);
std::array<int, 50> srvDescriptorTablesCounts = makeArray(-1);

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

void BindImageResourceToShaderResource(int descriptorSet, EntryHandle* index, int textureCount, int firstTexture, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceImage* header = (ShaderResourceImage*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::IMAGESTORE2D && header->type != ShaderResourceType::IMAGE2D)
        return;

    header->textureHandles = index;
    header->textureCount = textureCount;
    header->firstTexture = firstTexture;
}

void BindSamplerResourceToShaderResource(int descriptorSet, EntryHandle* indices, int samplerCount, int firstSampler, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceSampler* header = (ShaderResourceSampler*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::SAMPLERSTATE)
        return;

    header->samplerHandles = indices;
    header->samplerCount = samplerCount;
    header->firstSampler = firstSampler;
}

void BindSampledImageToShaderResource(int descriptorSet, EntryHandle* index, int textureCount, int firstTexture, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceImage* header = (ShaderResourceImage*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::SAMPLER2D && header->type != ShaderResourceType::SAMPLERCUBE && header->type != ShaderResourceType::SAMPLER3D)
        return;

    header->textureHandles = index;
    header->textureCount = textureCount;
    header->firstTexture = firstTexture;
}

void BindSampledImageArrayToShaderResource(int descriptorSet, EntryHandle* indices, uint32_t texCount, int firstTexture, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceImage* header = (ShaderResourceImage*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::SAMPLER3D && header->type != ShaderResourceType::SAMPLER2D && header->type != ShaderResourceType::SAMPLERCUBE)
        return;

    header->textureHandles = indices;
    header->textureCount = texCount;
    header->firstTexture = firstTexture;
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
    case ShaderResourceType::IMAGE2D:
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

    if (desc->type != ShaderResourceType::SAMPLER2D && desc->type != ShaderResourceType::SAMPLERCUBE && desc->type != ShaderResourceType::SAMPLER3D && desc->type != ShaderResourceType::IMAGESTORE2D && desc->type != ShaderResourceType::IMAGE2D)
        return;

    switch (desc->type)
    {
    case ShaderResourceType::IMAGE2D:
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
    set->samplerCount = resourceSet->samplerCount;
    set->viewCount = resourceSet->viewCount;

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
            ShaderResourceSampler* image = (ShaderResourceSampler*)ptr;
            image->samplerHandles = nullptr;
            image->samplerCount = 0;
            image->firstSampler = 0;
            ptr += sizeof(ShaderResourceSampler);
            break;
        }
        case ShaderResourceType::IMAGE2D:
        case ShaderResourceType::IMAGESTORE2D:
        {
            ShaderResourceImage* image = (ShaderResourceImage*)ptr;
            image->textureHandles = nullptr;
            image->textureCount = 0;
            image->firstTexture = 0;
            ptr += sizeof(ShaderResourceImage);
            memBarrierType = MemoryBarrierType::IMAGE_BARRIER;
            if (resource->action == ShaderResourceAction::SHADERWRITE || resource->action == ShaderResourceAction::SHADERREADWRITE)
            {
                ImageShaderResourceBarrier* barriers = (ImageShaderResourceBarrier*)ptr;
                //barriers->dstStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers->dstAction = WRITE_SHADER_RESOURCE;
                barriers->type = memBarrierType;

              //  barriers[1].srcStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers[1].srcAction = WRITE_SHADER_RESOURCE;
                barriers[1].type = memBarrierType;

                ptr += (sizeof(ImageShaderResourceBarrier) * 2);
                set->barrierCount += 2;
            }
            break;
        }
        case ShaderResourceType::SAMPLER3D:
        case ShaderResourceType::SAMPLER2D:
        case ShaderResourceType::SAMPLERCUBE:
        {
            ShaderResourceImage* image = (ShaderResourceImage*)ptr;
            image->textureHandles = nullptr;
            image->textureCount = 0;
            image->firstTexture = 0;

            ptr += sizeof(ShaderResourceImage);
            memBarrierType = MemoryBarrierType::IMAGE_BARRIER;
            if (resource->action == ShaderResourceAction::SHADERWRITE || resource->action == ShaderResourceAction::SHADERREADWRITE)
            {
                ImageShaderResourceBarrier* barriers = (ImageShaderResourceBarrier*)ptr;
               // barriers->srcStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers->srcAction = WRITE_SHADER_RESOURCE;
                barriers->type = memBarrierType;
                ptr += (sizeof(ImageShaderResourceBarrier));
                set->barrierCount++;
            }
            break;
        }
        case ShaderResourceType::CONSTANT_BUFFER:
        {
            ShaderResourceConstantBuffer* constants = (ShaderResourceConstantBuffer*)ptr;
            constants->size = resource->size;
            constants->offset = resource->offset;
            constants->stage = resource->stages;
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
                ShaderResourceBarrier* barriers = (ShaderResourceBarrier*)ptr;
               // barriers->srcStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers->srcAction = WRITE_SHADER_RESOURCE;
                barriers->type = memBarrierType;
                ptr += (sizeof(ShaderResourceBufferBarrier));
                set->barrierCount++;
            }
            break;
        }
        case ShaderResourceType::BUFFER_VIEW:
        {
            memBarrierType = MemoryBarrierType::BUFFER_BARRIER;
            ptr += sizeof(ShaderResourceBufferView);
            if (resource->action == ShaderResourceAction::SHADERWRITE || resource->action == ShaderResourceAction::SHADERREADWRITE)
            {
                ShaderResourceBarrier* barriers = (ShaderResourceBarrier*)ptr;
               // barriers->srcStage = ConvertShaderStageToBarrierStage(resource->stages);
                barriers->srcAction = WRITE_SHADER_RESOURCE;
                barriers->type = memBarrierType;
                ptr += (sizeof(ShaderResourceBufferBarrier));
                set->barrierCount++;
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


struct PipelineXMLTag
{
    unsigned long hashCode;
};

struct PipelineDescriptionXMLTag : PipelineXMLTag //followed by shaderNameLen Bytes
{
    int sampLo;
    int sampHi;
};

struct PrimitiveXMLTag : PipelineXMLTag
{
    PrimitiveType primType;
};

struct DepthXMLTag : PipelineXMLTag
{
    bool enabled;
    RasterizerTest depthOp;
};

struct CullModeXMLTag : PipelineXMLTag
{
    CullMode mode;
};


int HandleComputeLayout(char* fileData, int size, int currentLocation, uintptr_t* offset)
{
    unsigned long hashesAndVals[6];

    int dataSize = 0;

    int ret = ReadAttributes(fileData, size, currentLocation, hashesAndVals, &dataSize);

    ShaderComputeLayoutXMLTag* tag = (ShaderComputeLayoutXMLTag*)&readerMemBuffer[readerMemBufferAllocate];

    *offset = (uintptr_t)tag;

    tag->hashCode = hash("ComputeLayout");

    int stackIter = 0;

    while (dataSize > stackIter)
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



int ReadAttributesPipeline(char* fileData, int size, int currentLocation, unsigned long* hashes, int* stackSize)
{
    int count = 0;
    char* data = fileData + currentLocation;

    int ret = 0;
    char c = data[ret];

    while (c != '>' && ret < MAX_ATTRIBUTE_LINE_LEN && (currentLocation + ret) < size)
    {
        ret += ReadAttributeName(fileData, size, currentLocation + ret, &hashes[count]);

        switch (hashes[count])
        {
        case hash("op"):
        case hash("type"):
        case hash("mode"):
        case hash("format"):
        case hash("usage"):
        case hash("rate"):
        case hash("winding"):
        case hash("write"):
        case hash("failop"):
        case hash("passop"):
        case hash("depthfailop"):
        case hash("compareop"):
        {
            ret += ReadAttributeValueHash(fileData, size, currentLocation + ret, &hashes[count + 1]);
            break;
        }
        case hash("writemask"):
        case hash("comparemask"):
        case hash("ref"):
        case hash("offset"):
        case hash("sampLo"):
        case hash("sampHi"):
        case hash("size"):
        {
            ret += ReadAttributeValueVal(fileData, size, currentLocation + ret, &hashes[count + 1]);
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

static int HandlePipelineDescription(char* fileData, int size, int currentLocation, GenericPipelineStateInfo* stateInfo)
{
    unsigned long hashes[6];

    int attrSize = 0;

    int ret = ReadAttributesPipeline(fileData, size, currentLocation, hashes, &attrSize);

    int stackIter = 0;

    while (attrSize > stackIter)
    {
        unsigned long code = hashes[stackIter];
        unsigned long codeV = hashes[stackIter + 1];

        switch (code)
        {
        case hash("sampLo"):
        {
            stateInfo->sampleCountLow = codeV;
            break;
        }
        case hash("sampHi"):
        {
            stateInfo->sampleCountHigh = codeV;
            break;
        }

        default:
            throw std::runtime_error("Failed Pipeline Description");
            break;
        }

        stackIter += 2;
    }

    return ret;
}

int HandleCullMode(char* fileData, int size, int currentLocation, GenericPipelineStateInfo* stateInfo)
{
    unsigned long hashes[6];

    int attrSize = 0;

    int ret = ReadAttributesPipeline(fileData, size, currentLocation, hashes, &attrSize);

    int stackIter = 0;

    while (attrSize > stackIter)
    {
        unsigned long code = hashes[stackIter];
        unsigned long codeV = hashes[stackIter + 1];

        switch (code)
        {
        case hash("mode"):
        {
            switch (codeV)
            {
            case hash("none"):
                stateInfo->cullMode = CullMode::CULL_NONE;
                break;

            case hash("back"):
                stateInfo->cullMode = CullMode::CULL_BACK;
                break;

            case hash("front"):
                stateInfo->cullMode = CullMode::CULL_FRONT;
                break;

            default:
                throw std::runtime_error("Invalid Cull Mode");
            }
            break;
        }
        case hash("winding"):
        {
            switch (codeV)
            {
            case hash("cw"):
                stateInfo->windingOrder = TriangleWinding::CW;
                break;

            case hash("ccw"):
                stateInfo->windingOrder = TriangleWinding::CCW;
                break;

            default:
                throw std::runtime_error("Invalid Winding");
            }
            break;
        }

        default:
            throw std::runtime_error("Failed Cull");
        }

        stackIter += 2;
    }

    return ret;
}

int HandleDepthTest(char* fileData, int size, int currentLocation, GenericPipelineStateInfo* stateInfo)
{
    unsigned long hashes[6];

    int attrSize = 0;

    int ret = ReadAttributesPipeline(fileData, size, currentLocation, hashes, &attrSize);

    int stackIter = 0;

    stateInfo->depthEnable = true;

    while (attrSize > stackIter)
    {
        unsigned long code = hashes[stackIter];
        unsigned long codeV = hashes[stackIter + 1];

        switch (code)
        {
        case hash("write"):
        {
            switch (codeV)
            {
            case hash("true"):
            {
                stateInfo->depthWrite = true;
                break;
            }
            case hash("false"):
            {
                stateInfo->depthWrite = false;
                break;
            }
            }
            break;
        }

        case hash("op"):
        {
            switch (codeV)
            {
            case hash("never"):
                stateInfo->depthTest = NEVER;
                break;

            case hash("less"):
                stateInfo->depthTest = LESS;
                break;

            case hash("equal"):
                stateInfo->depthTest = EQUAL;
                break;

            case hash("lessequal"):
                stateInfo->depthTest = LESSEQUAL;
                break;

            case hash("greater"):
                stateInfo->depthTest = GREATER;
                break;

            case hash("notequal"):
                stateInfo->depthTest = NOTEQUAL;
                break;

            case hash("greaterequal"):
                stateInfo->depthTest = GREATEREQUAL;
                break;

            case hash("always"):
                stateInfo->depthTest = ALLPASS;
                break;

            default:
                throw std::runtime_error("Invalid Depth Compare Op");
            }
            break;
        }

        default:
            throw std::runtime_error("Failed Depth Mode");
        }

        stackIter += 2;
    }

    return ret;
}

StencilOp ParseStencilOp(uint32_t codeV)
{
    switch (codeV)
    {
    case hash("replace"): return StencilOp::REPLACE;
    case hash("keep"):    return StencilOp::KEEP;
    case hash("zero"):    return StencilOp::ZERO;
    default:              return StencilOp::KEEP;
    }
}

static int HandleStencilTest(char* fileData, int size, int currentLocation, FaceStencilData* face)
{
    unsigned long hashes[14];

    int attrSize = 0;

    int ret = ReadAttributesPipeline(fileData, size, currentLocation, hashes, &attrSize);

    int stackIter = 0;

    while (attrSize > stackIter)
    {
        unsigned long code = hashes[stackIter];
        unsigned long codeV = hashes[stackIter + 1];

        switch (code)
        {

        case hash("failop"):
            face->failOp = ParseStencilOp(codeV);
            break;

        case hash("passop"):
            face->passOp = ParseStencilOp(codeV);
            break;


        case hash("depthfailop"):
            face->depthFailOp = ParseStencilOp(codeV);
            break;


        case hash("compareop"):
        {
            switch (codeV)
            {
            case hash("never"):
                face->stencilCompare = NEVER;
                break;

            case hash("less"):
                face->stencilCompare = LESS;
                break;

            case hash("equal"):
                face->stencilCompare = EQUAL;
                break;

            case hash("lessequal"):
                face->stencilCompare = LESSEQUAL;
                break;

            case hash("greater"):
                face->stencilCompare = GREATER;
                break;

            case hash("notequal"):
                face->stencilCompare = NOTEQUAL;
                break;

            case hash("greaterequal"):
                face->stencilCompare = GREATEREQUAL;
                break;

            case hash("always"):
                face->stencilCompare = ALLPASS;
                break;

            default:
                throw std::runtime_error("Invalid Stencil Compare Op");
            }
            break;
        }

        case hash("writemask"):
        {
            face->writeMask = codeV;
            break;
        }

        case hash("comparemask"):
        {
            face->compareMask = codeV;
            break;
        }

        case hash("ref"):
        {
            face->reference = codeV;
            break;
        }

        default:
            throw std::runtime_error("Failed Depth Mode");
        }

        stackIter += 2;
    }

    return ret;
}

int HandlePrimitiveType(char* fileData, int size, int currentLocation, GenericPipelineStateInfo* stateInfo)
{
    unsigned long hashes[6];

    int attrSize = 0;

    int ret = ReadAttributesPipeline(fileData, size, currentLocation, hashes, &attrSize);

    int stackIter = 0;

    while (attrSize > stackIter)
    {
        unsigned long code = hashes[stackIter];
        unsigned long codeV = hashes[stackIter + 1];

        switch (code)
        {
        case hash("type"):
        {
            switch (codeV)
            {
            case hash("trilists"):
                stateInfo->primType = TRIANGLES;
                break;

            case hash("tristrips"):
                stateInfo->primType = TRISTRIPS;
                break;

            case hash("trifans"):
                stateInfo->primType = TRIFAN;
                break;

            case hash("points"):
                stateInfo->primType = POINTSLIST;
                break;

            case hash("linelists"):
                stateInfo->primType = LINELIST;
                break;

            case hash("linestrips"):
                stateInfo->primType = LINESTRIPS;
                break;

            default:
                throw std::runtime_error("Invalid Primitive Type");
            }
            break;
        }
        case hash("size"):
        {
            stateInfo->lineWidth = (float)codeV;
            break;
        }

        default:
            throw std::runtime_error("Failed Prim Mode");
        }

        stackIter += 2;
    }

    return ret;
}

int HandleVertexComponentInput(char* fileData, int size, int currentLocation, GenericPipelineStateInfo* stateInfo, int vertexBufferInputLocation, int perVertexSlotLocation)
{
    unsigned long hashes[8];

    int attrSize = 0;

    int ret = ReadAttributesPipeline(fileData, size, currentLocation, hashes, &attrSize);

    int stackIter = 0;

    while (attrSize > stackIter)
    {
        unsigned long code = hashes[stackIter];
        unsigned long codeV = hashes[stackIter + 1];

        switch (code)
        {
        case hash("usage"):
        {
            switch (codeV)
            {
            case hash("POS0"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].vertexusage = VertexUsage::POSITION;
                break;

            case hash("TEX0"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].vertexusage = VertexUsage::TEX0;
                break;

            case hash("TEX1"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].vertexusage = VertexUsage::TEX1;
                break;

            case hash("TEX2"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].vertexusage = VertexUsage::TEX2;
                break;

            case hash("TEX3"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].vertexusage = VertexUsage::TEX3;
                break;

            case hash("NORMAL"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].vertexusage = VertexUsage::NORMAL;
                break;

            case hash("BONES"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].vertexusage = VertexUsage::BONES;
                break;

            case hash("WEIGHTS"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].vertexusage = VertexUsage::WEIGHTS;
                break;

            case hash("COLOR0"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].vertexusage = VertexUsage::COLOR0;
                break;

            default:
                throw std::runtime_error("Invalid vertex usage");
            }
            break;
        }

        case hash("format"):
        {
            switch (codeV)
            {
            case hash("uint"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R32_UINT;
                break;

            case hash("int"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R32_SINT;
                break;

            case hash("vec4"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R32G32B32A32_SFLOAT;
                break;

            case hash("vec3"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R32G32B32_SFLOAT;
                break;

            case hash("vec2"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R32G32_SFLOAT;
                break;

            case hash("float"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R32_SFLOAT;
                break;

            case hash("ivec2"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R32G32_SINT;
                break;

            case hash("u8vec2"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R8G8_UINT;
                break;

            case hash("i16vec2"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R16G16_SINT;
                break;

            case hash("i16vec3"):
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].format = ComponentFormatType::R16G16B16_SINT;
                break;

            default:
                throw std::runtime_error("Invalid vertex format");
            }
            break;
        }

        case hash("offset"):
        {
            stateInfo->vertexBufferDesc[vertexBufferInputLocation].descriptions[perVertexSlotLocation].byteoffset = codeV;
            break;
        }

        default:
            throw std::runtime_error("Failed Vertex Component");
        }

        stackIter += 2;
    }

    return ret;
}

int HandleVertexInput(char* fileData, int size, int currentLocation, GenericPipelineStateInfo* stateInfo, int vertexBufferInputLocation)
{
    unsigned long hashes[6];

    int attrSize = 0;

    int ret = ReadAttributesPipeline(fileData, size, currentLocation, hashes, &attrSize);

    int stackIter = 0;

    while (attrSize > stackIter)
    {
        unsigned long code = hashes[stackIter];
        unsigned long codeV = hashes[stackIter + 1];

        switch (code)
        {
        case hash("rate"):
        {
            switch (codeV)
            {
            case hash("vertex"):
            {
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].rate = VertexBufferRate::PERVERTEX;
                break;
            }
            case hash("instance"):
            {
                stateInfo->vertexBufferDesc[vertexBufferInputLocation].rate = VertexBufferRate::PERINSTANCE;
                break;
            }
            }
            break;
        }
        case hash("size"):
        {
            stateInfo->vertexBufferDesc[vertexBufferInputLocation].perInputSize = codeV;
            break;
        }

        default:
            throw std::runtime_error("Failed Vertex Input");
            break;
        }

        stackIter += 2;
    }

    return ret;
}

void CreatePipelineDescription(const std::string& filename, GenericPipelineStateInfo* stateInfo);

