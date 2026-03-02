#pragma once

#include <d3d12.h>
#include <dxgi1_6.h>
#include <d3dcompiler.h>
#include <DirectXMath.h>
#include "d3dx12.h"
#include <dxgidebug.h>


typedef size_t EntryHandle;

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
    D12RESOURCEHANDLE = 9,
    D12FENCEHANDLE = 10,
    D12FENCEOBJECT = 11,
    D12IMAGEMEMORYPOOL = 12,
    D12BUFFERMEMORYPOOL = 13,
    D12DESCRIPTORMANAGER = 14,
};

struct PoolItem
{
    DX12ComType comType;
    uintptr_t memoryHandle;
};


struct D12Fence
{
    EntryHandle fenceHandle;
    uint64_t fenceValue;
    HANDLE fenceEvent;
};

struct ImageMemoryPool
{
    EntryHandle heap;
    size_t currentPointer;
    size_t sizeOfHeap;
    size_t alignment;
};

struct DriverMemoryBuffer
{
    EntryHandle bufferHandle;
    size_t sizeOfAlloc;
    size_t currentPointer;
    D3D12_RESOURCE_STATES currentState;
    D3D12_RESOURCE_STATES initialState;
};

enum D3D12ShaderType
{
    VERTEX = 0,
    PIXEL = 1
};

struct ShaderHandles
{
    D3D12ShaderType type;
    EntryHandle shader;
};

struct DescriptorHeapManager
{
    D3D12_DESCRIPTOR_HEAP_TYPE type;
    EntryHandle descriptorHeap;
    int descriptorHeapHandlePointer;
    int maxDescriptorHeapHandles;
    UINT descriptorHeapHandleSize;
};

struct PipelineObject
{
    EntryHandle rootSignature;
    EntryHandle pipelineState;
    int heapsCount;
    EntryHandle descriptorHeap[8];
    int descriptorTableCount;
    int descriptorHeapPointer[8];
    int resourceCount[8];
    int descriptorHeapSelection[8]; //number of descriptor tables
    D3D_PRIMITIVE_TOPOLOGY topology;//D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST
    int instanceCount;

    EntryHandle vertexBuffer = ~0ui64;
    SIZE_T vertexBufferOffset;
    SIZE_T vertexBufferSize;
    int vertexSize;
    int vertexCount;
    EntryHandle indexBuffer = ~0ui64;
    SIZE_T indexBufferOffset;
    SIZE_T indexBufferSize;
    int indexSize;
    int indexCount;

};

struct DX12Device
{

    ID3D12Device2* deviceHandle;

    char deviceData[4096];

    char deviceCache[4096];

    uintptr_t memHandleSlots[80];

    PoolItem deviceHandlePool[80];

    EntryHandle handlesPointer = 0;

    size_t deviceDataAlloc = 0;

    size_t deviceCacheAlloc = 0;


    EntryHandle AllocTypeForEntry(void* data, DX12ComType type);

    void* AllocFromDeviceCache(size_t size, size_t alignment);

    void* AllocFromDeviceStorage(size_t size, size_t alignment);

    size_t AllocFromDriverMemoryBuffer(EntryHandle bufferPoolIndex, size_t allocSize, size_t alignment);

    bool CheckTearingSupport();

    void EnableRuntimeValidation();

    void* GetAndValidateItem(EntryHandle poolHandle, DX12ComType type);

    ID3D12Resource* GetResourceHandleForMemoryBuffer(EntryHandle memoryBuffer);

    ID3D12DescriptorHeap* GetDescriptorHeapFromManager(EntryHandle managerIndex);

    UINT GetDescriptorHeapSizeFromManager(EntryHandle managerIndex);

    IDXGIAdapter4* GetAdapter(UINT createFactoryFlags);


    ID3D12Resource* CreateBuffer(ID3D12Device2* device, UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags, D3D12_HEAP_TYPE heapType);

    ID3D12CommandAllocator* CreateCommandAllocator(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type);
    EntryHandle CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE type);

    ID3D12GraphicsCommandList7* CreateCommandList(ID3D12Device2* device,
        ID3D12CommandAllocator* commandAllocator, D3D12_COMMAND_LIST_TYPE type);
    EntryHandle CreateCommandList(EntryHandle commandAllocator, D3D12_COMMAND_LIST_TYPE type);

    ID3D12CommandQueue* CreateCommandQueue(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type);
    EntryHandle CreateCommandQueue(D3D12_COMMAND_LIST_TYPE type);

    EntryHandle CreateCommittedImageResource(UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);

    void CreateCBVDescriptorHandle(EntryHandle bufferPoolHandle, UINT offset, UINT size, DescriptorHeapManager* heap);

    EntryHandle CreateDSVRSVMemoryPool(size_t sizeOfPool, size_t alignment, bool msaa);

    ID3D12DescriptorHeap* CreateDescriptorHeap(ID3D12Device2* device, D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize);
    EntryHandle CreateDescriptorHeap(D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize);

    EntryHandle CreateDescriptorHeapManager(UINT maxDescriptorHandles, D3D12_DESCRIPTOR_HEAP_TYPE type, D3D12_DESCRIPTOR_HEAP_FLAGS flags);

    int CreateDepthStencilView(EntryHandle descriptorHeapIdx, EntryHandle poolIdx, EntryHandle* outBuffers, UINT imageCount, UINT width, UINT height, DXGI_FORMAT format, UINT sampleCount);

    void CreateDevice(bool debug);

    HANDLE CreateEventHandle();

    ID3D12Fence* CreateFence(ID3D12Device2* device);
    EntryHandle CreateFenceObject();

    ID3D12Heap* CreateDX12Heap(SIZE_T size, SIZE_T alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType);

    void CreateImageSampler(DescriptorHeapManager* samplerDescriptorHeap);

    void CreateImageSRVDescriptorHandle(EntryHandle bufferPoolHandle, UINT mipsLevels, DXGI_FORMAT format, DescriptorHeapManager* heap, D3D12_SRV_DIMENSION dimension);

    EntryHandle CreateImageMemoryPool(size_t sizeOfPool, size_t alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType);

    EntryHandle CreateImageResourceFromPool(EntryHandle poolIdx, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);

    ID3D12Resource* CreatePlacedImageResource(ImageMemoryPool* pool, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);

    int CreateRenderTargetView(EntryHandle swapChainIdx, EntryHandle descriptorHeapIdx, EntryHandle* outBuffers, UINT imageCount);

    EntryHandle CreateRootSignature(CD3DX12_ROOT_PARAMETER* rootParameters, UINT parameterCount, D3D12_ROOT_SIGNATURE_FLAGS flags);

    void CreateSRVDescriptorHandle(EntryHandle bufferPoolHandle, UINT offset, UINT numCount, UINT size, DXGI_FORMAT format, DescriptorHeapManager* heap, D3D12_SRV_DIMENSION dimension);

    EntryHandle CreateShaderBlob(const char* shaderfile);

    IDXGISwapChain4* CreateSwapChain(HWND hWnd, ID3D12CommandQueue* commandQueue, int width, int height, int bufferCount, DXGI_FORMAT format, UINT debug);
    EntryHandle CreateSwapChain(HWND hWnd, EntryHandle commandQueue, int width, int height, int bufferCount, DXGI_FORMAT format, UINT debug);

    EntryHandle CreateTextureMemoryPool(size_t sizeOfPool, size_t alignment);

    void CreateUAVDescriptorHandle(EntryHandle bufferPoolHandle, UINT offset, UINT numCount, UINT size, DXGI_FORMAT format, DescriptorHeapManager* heap);

    EntryHandle CreateDeviceBuffer(UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags);

    EntryHandle CreateHostBuffer(UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags);


    void ExecuteCommandListsOnQueue(EntryHandle queueIndex, ID3D12CommandList** lists, UINT numOfLists);

    void Flush(EntryHandle fenceObjIndex, EntryHandle commandQueue);
    void Flush(EntryHandle fenceObjIndex, EntryHandle commandQueue, uint64_t& fenceValue);

    uint64_t Signal(EntryHandle commandQueue, EntryHandle fenceObjIndex);
    uint64_t Signal(EntryHandle commandQueue, EntryHandle fenceObjIndex, uint64_t& fenceValue);

    void TransitionBufferBarrier(EntryHandle cmdBufferHandle, EntryHandle resourceHandle, D3D12_BARRIER_SYNC srcSync, D3D12_BARRIER_ACCESS srcAccess, D3D12_BARRIER_SYNC dstSync, D3D12_BARRIER_ACCESS dstAccess);

    void TransitionImageResource(ID3D12GraphicsCommandList7* cmdBuffer, ID3D12Resource* imageResource,
        D3D12_BARRIER_SYNC srcSync, D3D12_BARRIER_ACCESS srcAccess,
        D3D12_BARRIER_SYNC dstSync, D3D12_BARRIER_ACCESS dstAccess,
        D3D12_BARRIER_LAYOUT srcLayout, D3D12_BARRIER_LAYOUT dstLayout,
        UINT baseArrayIndex, UINT numArrayLayers,
        UINT baseMipIndex, UINT numMipLevels);

    void WaitForFenceValue(EntryHandle fenceObjIndex, uint64_t fenceValue, DWORD duration);

    void WriteToDeviceLocalMemory(EntryHandle deviceLocalMemBuffer, EntryHandle stagingBufferIndex, EntryHandle transferCommandBuffer, void* data, size_t size, size_t offset, size_t stride, int copies);

    void WriteToHostMemory(EntryHandle memoryBuffer, void* data, size_t size, size_t offset, size_t stride, int copies);

    void WriteToImageDeviceLocalMemory(EntryHandle imageResourceHandle, EntryHandle commandBufferIndex, EntryHandle stagingBufferIndex, char* data,
        UINT width, UINT height,
        UINT componentCount, UINT totalImageSize,
        DXGI_FORMAT format,
        UINT mipLevels, UINT layers);


    void ReleaseAllDriverCOMHandles();
};

