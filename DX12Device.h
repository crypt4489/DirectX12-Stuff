#pragma once

#include <d3d12.h>
#include <dxgi1_6.h>
#include <d3dcompiler.h>
#include <DirectXMath.h>
#include <dxgidebug.h>


typedef SIZE_T EntryHandle;

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
    SIZE_T currentPointer;
    SIZE_T sizeOfHeap;
    SIZE_T alignment;
};

struct DriverMemoryBuffer
{
    EntryHandle bufferHandle;
    SIZE_T sizeOfAlloc;
    SIZE_T currentPointer;
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

/*

 D3D12_ROOT_PARAMETER_TYPE
    {
        D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE	= 0,
        D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS	= ( D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE + 1 ) ,
        D3D12_ROOT_PARAMETER_TYPE_CBV	= ( D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS + 1 ) ,
        D3D12_ROOT_PARAMETER_TYPE_SRV	= ( D3D12_ROOT_PARAMETER_TYPE_CBV + 1 ) ,
        D3D12_ROOT_PARAMETER_TYPE_UAV	= ( D3D12_ROOT_PARAMETER_TYPE_SRV + 1 )
    } 	D3D12_ROOT_PARAMETER_TYPE;

    D3D12_DESCRIPTOR_RANGE_TYPE
    {
        D3D12_DESCRIPTOR_RANGE_TYPE_SRV	= 0,
        D3D12_DESCRIPTOR_RANGE_TYPE_UAV	= ( D3D12_DESCRIPTOR_RANGE_TYPE_SRV + 1 ) ,
        D3D12_DESCRIPTOR_RANGE_TYPE_CBV	= ( D3D12_DESCRIPTOR_RANGE_TYPE_UAV + 1 ) ,
        D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER	= ( D3D12_DESCRIPTOR_RANGE_TYPE_CBV + 1 )
    } 	D3D12_DESCRIPTOR_RANGE_TYPE;

    D3D12_DESCRIPTOR_RANGE
    {
    D3D12_DESCRIPTOR_RANGE_TYPE RangeType;
    UINT NumDescriptors;
    UINT BaseShaderRegister;
    UINT RegisterSpace;
    UINT OffsetInDescriptorsFromTableStart;
    } 	D3D12_DESCRIPTOR_RANGE;



    D3D12_ROOT_CONSTANTS
    {
    UINT ShaderRegister;
    UINT RegisterSpace;
    UINT Num32BitValues;
    } 	D3D12_ROOT_CONSTANTS;

    D3D12_ROOT_DESCRIPTOR
    {
    UINT ShaderRegister;
    UINT RegisterSpace;
    } 	D3D12_ROOT_DESCRIPTOR;

*/


struct DX12PipelineStateObjectCreate
{
    D3D12_GRAPHICS_PIPELINE_STATE_DESC desc{};


    void SetDepthStencilState(BOOL depthEnable,
        BOOL stencilEnable, D3D12_COMPARISON_FUNC depthComapreFunc, D3D12_DEPTH_WRITE_MASK depthWriteMask,
        D3D12_DEPTH_STENCILOP_DESC* frontFaceStencil, D3D12_DEPTH_STENCILOP_DESC* backFaceStencil,
        UINT sReadMask, UINT8 sWriteMask
    )
    {
      
        desc.DepthStencilState.DepthEnable = depthEnable;
        desc.DepthStencilState.DepthWriteMask = depthWriteMask;
        desc.DepthStencilState.DepthFunc = depthComapreFunc;
        desc.DepthStencilState.StencilEnable = stencilEnable;

        desc.DepthStencilState.BackFace = *backFaceStencil;
        desc.DepthStencilState.FrontFace = *frontFaceStencil;
        desc.DepthStencilState.StencilReadMask = sReadMask;
        desc.DepthStencilState.StencilWriteMask = sWriteMask;
    }

    void SetRasterizerState(D3D12_CULL_MODE cullMode, BOOL fcc)
    {
        desc.RasterizerState.CullMode = cullMode;
        desc.RasterizerState.FrontCounterClockwise = fcc;
        desc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        desc.RasterizerState.DepthBias = D3D12_DEFAULT_DEPTH_BIAS;
        desc.RasterizerState.DepthBiasClamp = D3D12_DEFAULT_DEPTH_BIAS_CLAMP;
        desc.RasterizerState.SlopeScaledDepthBias = D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS;
        desc.RasterizerState.DepthClipEnable = TRUE;
        desc.RasterizerState.MultisampleEnable = FALSE;
        desc.RasterizerState.AntialiasedLineEnable = FALSE;
        desc.RasterizerState.ForcedSampleCount = 0;
        desc.RasterizerState.ConservativeRaster = D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF;
    }

    void SetInputLayout(D3D12_INPUT_ELEMENT_DESC* inputDescs, UINT numDesc, D3D12_PRIMITIVE_TOPOLOGY_TYPE primType)
    {
        desc.InputLayout = { inputDescs, numDesc };
        desc.PrimitiveTopologyType = primType;
    }

    void SetNumRenderTargets(UINT numRenderTargets)
    {
        desc.NumRenderTargets = numRenderTargets;
    }

    void SetSlotRenderTarget(UINT slotIndex, DXGI_FORMAT format)
    {
        desc.RTVFormats[slotIndex] = format;
    }

    void SetDepthFormat(DXGI_FORMAT dsvFormat)
    {
        desc.DSVFormat = dsvFormat;
    }

    void SetSampleDesc(UINT sampleMask, UINT sampleCount, UINT sampleQuality)
    {
        desc.SampleMask = UINT_MAX;
        desc.SampleDesc.Count = sampleCount;
        desc.SampleDesc.Quality = sampleQuality;
    }

    void SetBlendState()
    {
        desc.BlendState.AlphaToCoverageEnable = FALSE;
        desc.BlendState.IndependentBlendEnable = FALSE;

        const D3D12_RENDER_TARGET_BLEND_DESC defaultRenderTargetBlendDesc =
        {
            FALSE,FALSE,
            D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
            D3D12_BLEND_ONE, D3D12_BLEND_ZERO, D3D12_BLEND_OP_ADD,
            D3D12_LOGIC_OP_NOOP,
            D3D12_COLOR_WRITE_ENABLE_ALL,
        };

        for (UINT i = 0; i < D3D12_SIMULTANEOUS_RENDER_TARGET_COUNT; ++i)
            desc.BlendState.RenderTarget[i] = defaultRenderTargetBlendDesc;
    }

    void SetRootSignature(ID3D12RootSignature* rootSign)
    {
        desc.pRootSignature = rootSign;
    }

};

struct DX12DescriptorTableRanges
{
    D3D12_DESCRIPTOR_RANGE* ranges = nullptr;
    UINT numOfRanges = 0;
    UINT rangeCounter = 0;

    void CreateRange(UINT rangeIndex, D3D12_DESCRIPTOR_RANGE_TYPE rangeType, UINT numDescriptors, UINT baseShaderRegister, UINT registerSpace, UINT offsetInDescriptorsTableStart)
    {
        D3D12_DESCRIPTOR_RANGE* range = &ranges[rangeIndex];
        range->RangeType = rangeType;
        range->NumDescriptors = numDescriptors;
        range->BaseShaderRegister = baseShaderRegister;
        range->RegisterSpace = registerSpace;
        range->OffsetInDescriptorsFromTableStart = offsetInDescriptorsTableStart;
    }

    void AppendSRVRange(UINT numDescriptors, UINT baseShaderRegister, UINT registerSpaceLocation)
    {
        CreateRange(rangeCounter++, D3D12_DESCRIPTOR_RANGE_TYPE_SRV, numDescriptors, baseShaderRegister, registerSpaceLocation, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND);
    }

    void AppendUAVRange(UINT numDescriptors, UINT baseShaderRegister, UINT registerSpaceLocation)
    {
        CreateRange(rangeCounter++, D3D12_DESCRIPTOR_RANGE_TYPE_UAV, numDescriptors, baseShaderRegister, registerSpaceLocation, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND);
    }

    void AppendCBVRange(UINT numDescriptors, UINT baseShaderRegister, UINT registerSpaceLocation)
    {
        CreateRange(rangeCounter++, D3D12_DESCRIPTOR_RANGE_TYPE_CBV, numDescriptors, baseShaderRegister, registerSpaceLocation, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND);
    }

    void AppendSamplerRange(UINT numDescriptors, UINT baseShaderRegister, UINT registerSpaceLocation)
    {
        CreateRange(rangeCounter++, D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER, numDescriptors, baseShaderRegister, registerSpaceLocation, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND);
    }

    void CreateSamplerRange(UINT rangeIndex, UINT numDescriptors, UINT baseShaderRegister, UINT registerSpaceLocation)
    {
        CreateRange(rangeIndex, D3D12_DESCRIPTOR_RANGE_TYPE_SAMPLER, numDescriptors, baseShaderRegister, registerSpaceLocation, D3D12_DESCRIPTOR_RANGE_OFFSET_APPEND);
    }
};

struct DX12RootSignatureCreate
{
    D3D12_ROOT_PARAMETER* rootParameters;
    UINT numOfRootParameters;

    void CreateDescriptorTable(UINT rootParamIndex, DX12DescriptorTableRanges* rangeStruct, UINT offsetInRange, UINT numOfRanges, D3D12_SHADER_VISIBILITY visibility)
    {
        rootParameters[rootParamIndex].ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE;
        rootParameters[rootParamIndex].ShaderVisibility = visibility;
        rootParameters[rootParamIndex].DescriptorTable.NumDescriptorRanges = numOfRanges;
        rootParameters[rootParamIndex].DescriptorTable.pDescriptorRanges = rangeStruct->ranges + offsetInRange;
    }

    void CreateConstants(UINT rootParamIndex, UINT shaderRegister, UINT registerSpaceLocation, UINT num32bitconstants, D3D12_SHADER_VISIBILITY visibility)
    {
        rootParameters[rootParamIndex].ParameterType = D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS;
        rootParameters[rootParamIndex].ShaderVisibility = visibility;
        rootParameters[rootParamIndex].Constants.Num32BitValues = num32bitconstants;
        rootParameters[rootParamIndex].Constants.RegisterSpace= registerSpaceLocation;
        rootParameters[rootParamIndex].Constants.ShaderRegister = shaderRegister;
    }

    void CreateDescriptor(UINT rootParamIndex, D3D12_ROOT_PARAMETER_TYPE paramType, UINT shaderRegister, UINT registerSpaceLocation, D3D12_SHADER_VISIBILITY visibility)
    {
        rootParameters[rootParamIndex].ParameterType = paramType;
        rootParameters[rootParamIndex].ShaderVisibility = visibility;
        rootParameters[rootParamIndex].Descriptor.RegisterSpace= registerSpaceLocation;
        rootParameters[rootParamIndex].Descriptor.ShaderRegister = shaderRegister;
    }

    void CreateUAVDescriptor(UINT rootParamIndex, UINT shaderRegister, UINT registerSpaceLocation, D3D12_SHADER_VISIBILITY visibility)
    {
        CreateDescriptor(rootParamIndex, D3D12_ROOT_PARAMETER_TYPE_UAV, shaderRegister, registerSpaceLocation, visibility);
    }

    void CreateCBVDescriptor(UINT rootParamIndex, UINT shaderRegister, UINT registerSpaceLocation, D3D12_SHADER_VISIBILITY visibility)
    {
        CreateDescriptor(rootParamIndex, D3D12_ROOT_PARAMETER_TYPE_CBV, shaderRegister, registerSpaceLocation, visibility);
    }

    void CreateSRVDescriptor(UINT rootParamIndex, UINT shaderRegister, UINT registerSpaceLocation, D3D12_SHADER_VISIBILITY visibility)
    {
        CreateDescriptor(rootParamIndex, D3D12_ROOT_PARAMETER_TYPE_SRV, shaderRegister, registerSpaceLocation, visibility);
    }
};


struct DX12CPUDescriptorHandle
{
    D3D12_CPU_DESCRIPTOR_HANDLE handle;
    D3D12_CPU_DESCRIPTOR_HANDLE current;

    DX12CPUDescriptorHandle(D3D12_CPU_DESCRIPTOR_HANDLE start)
        :
        handle(start), current(start)
    {

    }

    DX12CPUDescriptorHandle(D3D12_CPU_DESCRIPTOR_HANDLE start, UINT startDescriptor, UINT descriptorStride)
        :
        handle(start), current(start)
    {
        current.ptr = (current.ptr + ((INT64)startDescriptor * (INT64)descriptorStride));
    }


    operator D3D12_CPU_DESCRIPTOR_HANDLE() const { return current; }

    void Advance(UINT numDescriptors, UINT descriptorStride)
    {
        current.ptr = (current.ptr + ((INT64)numDescriptors * (INT64)descriptorStride));
    }

    void Set(UINT numDescriptors, UINT descriptorStride)
    {
        current.ptr = (handle.ptr + ((SIZE_T)numDescriptors * (SIZE_T)descriptorStride));
    }

};


struct DX12GPUDescriptorHandle
{
    D3D12_GPU_DESCRIPTOR_HANDLE handle;
    D3D12_GPU_DESCRIPTOR_HANDLE current;

    DX12GPUDescriptorHandle(D3D12_GPU_DESCRIPTOR_HANDLE start)
        :
        handle(start), current(start)
    {

    }

    DX12GPUDescriptorHandle(D3D12_GPU_DESCRIPTOR_HANDLE start, UINT startDescriptor, UINT descriptorStride)
        :
        handle(start), current(start)
    {
        current.ptr = (current.ptr + ((INT64)startDescriptor * (INT64)descriptorStride));
    }


    operator D3D12_GPU_DESCRIPTOR_HANDLE() const { return current; }

    void Advance(UINT numDescriptors, UINT descriptorStride)
    {
        current.ptr = (current.ptr + ((INT64)numDescriptors * (INT64)descriptorStride));
    }

    void Set(UINT numDescriptors, UINT descriptorStride)
    {
        current.ptr = (handle.ptr + ((SIZE_T)numDescriptors * (SIZE_T)descriptorStride));
    }

};


struct DX12Device
{

    ID3D12Device2* deviceHandle;

    char deviceData[4096];

    char deviceCache[4096];

    uintptr_t memHandleSlots[80];

    PoolItem deviceHandlePool[80];

    EntryHandle handlesPointer = 0;

    SIZE_T deviceDataAlloc = 0;

    SIZE_T deviceCacheAlloc = 0;


    EntryHandle AllocTypeForEntry(void* data, DX12ComType type);

    void* AllocFromDeviceCache(SIZE_T size, SIZE_T alignment);

    void* AllocFromDeviceStorage(SIZE_T size, SIZE_T alignment);

    SIZE_T AllocFromDriverMemoryBuffer(EntryHandle bufferPoolIndex, SIZE_T allocSize, SIZE_T alignment);

    bool CheckTearingSupport();

    void EnableRuntimeValidation();

    void* GetAndValidateItem(EntryHandle poolHandle, DX12ComType type);

    ID3D12Resource* GetResourceHandleForMemoryBuffer(EntryHandle memoryBuffer);

    ID3D12DescriptorHeap* GetDescriptorHeapFromManager(EntryHandle managerIndex);

    UINT GetDescriptorHeapSizeFromManager(EntryHandle managerIndex);

    IDXGIAdapter4* GetAdapter(UINT createFactoryFlags);


    ID3D12Resource* CreateBuffer(ID3D12Device2* device, SIZE_T size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags, D3D12_HEAP_TYPE heapType);

    ID3D12CommandAllocator* CreateCommandAllocator(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type);
    EntryHandle CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE type);

    ID3D12GraphicsCommandList7* CreateCommandList(ID3D12Device2* device,
        ID3D12CommandAllocator* commandAllocator, D3D12_COMMAND_LIST_TYPE type);
    EntryHandle CreateCommandList(EntryHandle commandAllocator, D3D12_COMMAND_LIST_TYPE type);

    ID3D12CommandQueue* CreateCommandQueue(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type);
    EntryHandle CreateCommandQueue(D3D12_COMMAND_LIST_TYPE type);

    EntryHandle CreateCommittedImageResource(UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);

    void CreateCBVDescriptorHandle(EntryHandle bufferPoolHandle, SIZE_T offset, SIZE_T size, DescriptorHeapManager* heap, UINT heapIndex);

    EntryHandle CreateDSVRSVMemoryPool(SIZE_T sizeOfPool, SIZE_T alignment, bool msaa);

    ID3D12DescriptorHeap* CreateDescriptorHeap(ID3D12Device2* device, D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize);
    EntryHandle CreateDescriptorHeap(D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize);

    EntryHandle CreateDescriptorHeapManager(UINT maxDescriptorHandles, D3D12_DESCRIPTOR_HEAP_TYPE type, D3D12_DESCRIPTOR_HEAP_FLAGS flags);

    int CreateDepthStencilView(EntryHandle descriptorHeapIdx, EntryHandle poolIdx, EntryHandle* outBuffers, UINT imageCount, UINT width, UINT height, DXGI_FORMAT format, UINT sampleCount);

    void CreateDevice(bool debug);

    HANDLE CreateEventHandle();

    ID3D12Fence* CreateFence(ID3D12Device2* device);
    EntryHandle CreateFenceObject();

    ID3D12Heap* CreateDX12Heap(SIZE_T size, SIZE_T alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType);

    void CreateImageSampler(DescriptorHeapManager* samplerDescriptorHeap, UINT heapIndex);

    void CreateImageSRVDescriptorHandle(EntryHandle bufferPoolHandle, UINT mipsLevels, DXGI_FORMAT format, DescriptorHeapManager* heap, UINT heapIndex, D3D12_SRV_DIMENSION dimension);

    EntryHandle CreateImageMemoryPool(SIZE_T sizeOfPool, SIZE_T alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType);

    EntryHandle CreateImageResourceFromPool(EntryHandle poolIdx, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);

    ID3D12Resource* CreatePlacedImageResource(ImageMemoryPool* pool, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension);

    int CreateRenderTargetView(EntryHandle swapChainIdx, EntryHandle descriptorHeapIdx, EntryHandle* outBuffers, UINT imageCount);

    EntryHandle CreateRootSignature(DX12RootSignatureCreate* createInfo, D3D12_ROOT_SIGNATURE_FLAGS flags);

    void CreateBufferSRVDescriptorHandle(EntryHandle bufferPoolHandle, SIZE_T  offset, UINT numCount, SIZE_T size, DXGI_FORMAT format, DescriptorHeapManager* heap, UINT heapIndex, D3D12_SRV_DIMENSION dimension);

    EntryHandle CreateShaderBlob(const char* shaderfile);

    IDXGISwapChain4* CreateSwapChain(HWND hWnd, ID3D12CommandQueue* commandQueue, int width, int height, int bufferCount, DXGI_FORMAT format, UINT debug);
    EntryHandle CreateSwapChain(HWND hWnd, EntryHandle commandQueue, int width, int height, int bufferCount, DXGI_FORMAT format, UINT debug);

    EntryHandle CreateTextureMemoryPool(SIZE_T sizeOfPool, SIZE_T alignment);

    void CreateBufferUAVDescriptorHandle(EntryHandle bufferPoolHandle, SIZE_T  offset, UINT numCount, SIZE_T size,SIZE_T  counterOffsetInBytes, DXGI_FORMAT format, DescriptorHeapManager* heap, UINT heapIndex, D3D12_BUFFER_UAV_FLAGS uavFlags);

    EntryHandle CreateDeviceBuffer(UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags);

    EntryHandle CreateHostBuffer(UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags);


    void CopyDescriptors(UINT numDescriptors, UINT startInSrc, UINT startInDst, EntryHandle srcHeap, EntryHandle destHeap, UINT srcHeapSize, UINT dstHeapSize, D3D12_DESCRIPTOR_HEAP_TYPE type);

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

    void WriteToDeviceLocalMemory(EntryHandle deviceLocalMemBuffer, EntryHandle stagingBufferIndex, EntryHandle transferCommandBuffer, void* data, SIZE_T size, SIZE_T offset, SIZE_T stride, int copies);

    void WriteToHostMemory(EntryHandle memoryBuffer, void* data, SIZE_T size, SIZE_T offset, SIZE_T stride, int copies);

    void WriteToImageDeviceLocalMemory(EntryHandle imageResourceHandle, EntryHandle commandBufferIndex, EntryHandle stagingBufferIndex, char* data,
        UINT width, UINT height,
        UINT componentCount, UINT totalImageSize,
        DXGI_FORMAT format,
        UINT mipLevels, UINT layers);


    void ReleaseAllDriverCOMHandles();


    EntryHandle CreatePipelineStateObject(EntryHandle _rootSignature, DX12PipelineStateObjectCreate* createInfo)
    {
        ID3D12RootSignature* rootSignH = (ID3D12RootSignature*)GetAndValidateItem(_rootSignature, D12ROOTSIGNATURE);

        createInfo->SetRootSignature(rootSignH);

        ID3D12PipelineState* pipelineState;

        HRESULT hr = deviceHandle->CreateGraphicsPipelineState(
            &createInfo->desc,
            IID_PPV_ARGS(&pipelineState)
        );

        return AllocTypeForEntry(pipelineState, D12PIPELINESTATE);
    }


    DX12CPUDescriptorHandle GetCPUHandleFromDescriptorManager(DescriptorHeapManager* heapManager)
    {
        ID3D12DescriptorHeap* heap = (ID3D12DescriptorHeap*)GetAndValidateItem(heapManager->descriptorHeap, D12DESCRIPTORHEAP);

        return DX12CPUDescriptorHandle(heap->GetCPUDescriptorHandleForHeapStart());
    }
};

