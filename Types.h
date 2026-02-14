#pragma once
#include "Files.h"
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
char readerMemBuffer[64 * 1024];
size_t readerMemBufferAllocate = 0;


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
    size_t samplerHandle;
};

struct ShaderResourceImage : public ShaderResourceHeader
{
    size_t textureHandle;
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

void BindImageResourceToShaderResource(int descriptorSet, size_t index, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceImage* header = (ShaderResourceImage*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::IMAGESTORE2D && header->type != ShaderResourceType::IMAGE2D)
        return;

    header->textureHandle = index;
}

void BindSamplerResourceToShaderResource(int descriptorSet, size_t index, int bindingIndex)
{
    uintptr_t head = descriptorSets[descriptorSet];
    ShaderResourceSet* set = (ShaderResourceSet*)head;
    uintptr_t* offsets = (uintptr_t*)(head + sizeof(ShaderResourceSet));

    ShaderResourceSampler* header = (ShaderResourceSampler*)offsets[bindingIndex];

    if (header->type != ShaderResourceType::SAMPLERSTATE)
        return;

    header->samplerHandle = index;
}

void BindSampledImageToShaderResource(int descriptorSet, size_t index, int bindingIndex)
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
        case ShaderResourceType::IMAGE2D:
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
