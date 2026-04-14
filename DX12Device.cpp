#include "DX12Device.h"
#include <stdio.h>
IDXGIAdapter4* DX12Device::GetAdapter(UINT createFactoryFlags)
{
    IDXGIFactory4* dxgiFactory;

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
            SUCCEEDED(D3D12CreateDevice(dxgiAdapter1, D3D_FEATURE_LEVEL_12_2, __uuidof(ID3D12Device), nullptr)) &&
            dxgiAdapterDesc1.DedicatedVideoMemory > maxDedicatedVideoMemory)
        {
            maxDedicatedVideoMemory = dxgiAdapterDesc1.DedicatedVideoMemory;

            if (FAILED(dxgiAdapter1->QueryInterface(IID_PPV_ARGS(&dxgiAdapter4))))
            {
                printf("Cannot convert to adapter 4 %d\n", i);
            }
        }

        dxgiAdapter1->Release();
    }

    dxgiFactory->Release();

    return dxgiAdapter4;
}


bool DX12Device::CheckTearingSupport()
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

void  DX12Device::EnableRuntimeValidation()
{
    ID3D12Debug1* debug = nullptr;
    if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&debug))))
    {
        debug->EnableDebugLayer();
        debug->SetEnableGPUBasedValidation(TRUE); // optional but excellent
        debug->Release();
    }
}


void DX12Device::CreateDevice(bool debug)
{
    ID3D12Device2* d3d12Device2;

    UINT createFactoryFlags = (debug ? DXGI_CREATE_FACTORY_DEBUG : 0);

    IDXGIAdapter4* adapter = GetAdapter(createFactoryFlags);

    if (FAILED(D3D12CreateDevice(adapter, D3D_FEATURE_LEVEL_12_2, IID_PPV_ARGS(&d3d12Device2))))
    {
        printf("Failed to create device with given adapter\n");
        return;
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

            D3D12_MESSAGE_ID DenyIds[3] = {


            D3D12_MESSAGE_ID_CLEARRENDERTARGETVIEW_MISMATCHINGCLEARVALUE,


            D3D12_MESSAGE_ID_MAP_INVALID_NULLRANGE,


            D3D12_MESSAGE_ID_UNMAP_INVALID_NULLRANGE,


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

    deviceHandle = d3d12Device2;

    adapter->Release();

    return;
}

EntryHandle DX12Device::AllocTypeForEntry(void* data, DX12ComType type)
{
    EntryHandle poolHandle = handlesPointer++;

    deviceHandlePool[poolHandle].memoryHandle = (uintptr_t)data;
    deviceHandlePool[poolHandle].comType = type;

    return poolHandle;
}

void* DX12Device::GetAndValidateItem(EntryHandle poolHandle, DX12ComType type)
{
    DX12PoolItem* item = &deviceHandlePool[poolHandle];
    if (type != item->comType)
    {
        printf("Mismatched entry in pools %d %d", type, item->comType);


        return NULL;
    }

    return (void*)(item->memoryHandle);
}

ID3D12Resource* DX12Device::GetResourceHandleForMemoryBuffer(EntryHandle memoryBuffer)
{
    DX12DriverMemoryBuffer* memBuffer = (DX12DriverMemoryBuffer*)GetAndValidateItem(memoryBuffer, D12BUFFERMEMORYPOOL);

    return (ID3D12Resource*)GetAndValidateItem(memBuffer->bufferHandle, D12RESOURCEHANDLE);
}

void* DX12Device::AllocFromDeviceStorage(SIZE_T size, SIZE_T alignment)
{
    SIZE_T current = deviceDataAlloc;

    current = (current + alignment - 1) & ~(alignment - 1);

    deviceDataAlloc += (size + (current - deviceDataAlloc));

    return (void*)(deviceData + current);
}

void* DX12Device::AllocFromDeviceCache(SIZE_T size, SIZE_T alignment)
{
    SIZE_T current = deviceCacheAlloc;

    current = (current + alignment - 1) & ~(alignment - 1);

    if ((current + size) >= 4096)
    {
        current = 0;
        deviceCacheAlloc = 0;
    }

    deviceCacheAlloc += (size + (current - deviceCacheAlloc));

    return (void*)(deviceCache + current);
}

void  DX12Device::ReleaseAllDriverCOMHandles()
{
    for (SIZE_T i = 0; i < handlesPointer; ++i)
    {

        uintptr_t rawPtr = deviceHandlePool[i].memoryHandle;

        if (!rawPtr)
            continue;

        switch (deviceHandlePool[i].comType)
        {
        case DXGISWAPCHAIN:
        case D12QUEUE:
        case D12COMMANDBUFFER7:
        case D12DESCRIPTORHEAP:
        case D12SHADERBLOB:
        case D12MEMHEAP:
        case D12ROOTSIGNATURE:
        case D12PIPELINESTATE:
        case D12RESOURCEHANDLE:
        case D12COMMANDPOOL:
        case D12FENCEHANDLE:
        {
            IUnknown* unknown = reinterpret_cast<IUnknown*>(rawPtr);
            ULONG count = unknown->Release();
        }
        break;

        case D12FENCEOBJECT:
        {
            DX12Fence* fenceObj = reinterpret_cast<DX12Fence*>(rawPtr);
            CloseHandle(fenceObj->fenceEvent);
        }
        }
    }

    handlesPointer = 0;
    deviceDataAlloc = 0;
}

ID3D12CommandQueue* DX12Device::CreateCommandQueue(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type)
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

EntryHandle DX12Device::CreateCommandQueue(D3D12_COMMAND_LIST_TYPE type)
{
    ID3D12CommandQueue* queue = CreateCommandQueue(deviceHandle, type);
    return AllocTypeForEntry(queue, D12QUEUE);
}

ID3D12DescriptorHeap* DX12Device::CreateDescriptorHeap(ID3D12Device2* device, D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize)
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

EntryHandle DX12Device::CreateDescriptorHeap(D3D12_DESCRIPTOR_HEAP_TYPE type, int numDescriptors, D3D12_DESCRIPTOR_HEAP_FLAGS flags, UINT* descriptorSize)
{
    ID3D12DescriptorHeap* heap = CreateDescriptorHeap(deviceHandle, type, numDescriptors, flags, descriptorSize);
    return AllocTypeForEntry(heap, D12DESCRIPTORHEAP);
}

int DX12Device::CreateRenderTargetViews(EntryHandle descriptorHeapIdx, EntryHandle* imageResourceHandles, UINT numberOfRenderTargets, UINT numberOfFrameBuffers)
{
    DX12DescriptorHeapManager* heapManager = (DX12DescriptorHeapManager*)GetAndValidateItem(descriptorHeapIdx, D12DESCRIPTORMANAGER);
    ID3D12DescriptorHeap* heap = (ID3D12DescriptorHeap*)GetAndValidateItem(heapManager->descriptorHeap, D12DESCRIPTORHEAP);

    int rtvStart = heapManager->descriptorHeapHandlePointer;

    DX12CPUDescriptorHandle rtvHandle(heap->GetCPUDescriptorHandleForHeapStart(), rtvStart, heapManager->descriptorHeapHandleSize);

    UINT resourceIndex = 0;

    for (UINT i = 0; i < numberOfFrameBuffers; i++)
    {
        for (UINT j = 0; j < numberOfRenderTargets; j++)
        {
            ID3D12Resource* backBuffer = (ID3D12Resource*)GetAndValidateItem(imageResourceHandles[resourceIndex++], D12RESOURCEHANDLE);

            deviceHandle->CreateRenderTargetView(backBuffer, NULL, rtvHandle);

            rtvHandle.Advance(1, heapManager->descriptorHeapHandleSize);
        }
    }

    heapManager->descriptorHeapHandlePointer = rtvStart + resourceIndex;

    return rtvStart;
}

ID3D12Heap* DX12Device::CreateDX12Heap(SIZE_T size, SIZE_T alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType)
{
    D3D12_HEAP_DESC heapDesc = {};

    heapDesc.SizeInBytes = size;
    heapDesc.Alignment = alignment;
    heapDesc.Flags = heapFlags;
    heapDesc.Properties.Type = heapType;
    heapDesc.Properties.CPUPageProperty = D3D12_CPU_PAGE_PROPERTY_UNKNOWN;
    heapDesc.Properties.MemoryPoolPreference = D3D12_MEMORY_POOL_UNKNOWN;
    heapDesc.Properties.CreationNodeMask = 1;
    heapDesc.Properties.VisibleNodeMask = 1;

    ID3D12Heap* heap;

    deviceHandle->CreateHeap(&heapDesc, IID_PPV_ARGS(&heap));

    return heap;
}

EntryHandle DX12Device::CreateImageMemoryPool(SIZE_T sizeOfPool, SIZE_T alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType)
{
    DX12ImageMemoryPool* pool = (DX12ImageMemoryPool*)AllocFromDeviceStorage(sizeof(DX12ImageMemoryPool), 4);

    ID3D12Heap* heap = CreateDX12Heap(sizeOfPool, alignment, heapFlags, heapType);

    pool->heap = AllocTypeForEntry(heap, D12MEMHEAP);
    pool->sizeOfHeap = sizeOfPool;
    pool->currentPointer = 0ui64;
    pool->alignment = alignment;

    EntryHandle ret = AllocTypeForEntry(pool, D12IMAGEMEMORYPOOL);

    return ret;
}

EntryHandle DX12Device::CreateTextureMemoryPool(SIZE_T sizeOfPool, SIZE_T alignment)
{
    alignment = (alignment + D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT - 1) & ~(D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT - 1);

    return CreateImageMemoryPool(sizeOfPool, alignment, D3D12_HEAP_FLAG_ALLOW_ONLY_NON_RT_DS_TEXTURES, D3D12_HEAP_TYPE_DEFAULT);
}

EntryHandle DX12Device::CreateDSVRSVMemoryPool(SIZE_T sizeOfPool, SIZE_T alignment, bool msaa)
{
    if (msaa)
    {
        alignment = (alignment + D3D12_DEFAULT_MSAA_RESOURCE_PLACEMENT_ALIGNMENT - 1) & ~(D3D12_DEFAULT_MSAA_RESOURCE_PLACEMENT_ALIGNMENT - 1);
    }
    else
    {
        alignment = (alignment + D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT - 1) & ~(D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT - 1);
    }


    return CreateImageMemoryPool(sizeOfPool, alignment, D3D12_HEAP_FLAG_ALLOW_ONLY_RT_DS_TEXTURES, D3D12_HEAP_TYPE_DEFAULT);
}

int DX12Device::CreateDepthStencilView(EntryHandle descriptorHeapIdx, EntryHandle* inResources, EntryHandle* inViews, UINT imageCount)
{
    DX12DescriptorHeapManager* heapManager = (DX12DescriptorHeapManager*)GetAndValidateItem(descriptorHeapIdx, D12DESCRIPTORMANAGER);

    int dsvStart = heapManager->descriptorHeapHandlePointer;

    ID3D12DescriptorHeap* descHeap = (ID3D12DescriptorHeap*)GetAndValidateItem(heapManager->descriptorHeap, D12DESCRIPTORHEAP);

    DX12CPUDescriptorHandle dsvHandle(descHeap->GetCPUDescriptorHandleForHeapStart(), dsvStart, heapManager->descriptorHeapHandleSize);

    for (UINT i = 0; i < imageCount; ++i)
    {

        DX12ImageViewHandle* viewHandle = (DX12ImageViewHandle*)GetAndValidateItem(inViews[i], D12IMAGEVIEW);

        ID3D12Resource* depthBuffer = (ID3D12Resource*)GetAndValidateItem(inResources[i], D12RESOURCEHANDLE);

        D3D12_DEPTH_STENCIL_VIEW_DESC dsvDesc{};

        dsvDesc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
        dsvDesc.Format = viewHandle->shaderViewDesc.Format;
        dsvDesc.Flags = D3D12_DSV_FLAG_NONE;
        dsvDesc.Texture2D.MipSlice = 0;

        deviceHandle->CreateDepthStencilView(depthBuffer, &dsvDesc, dsvHandle);

        dsvHandle.Advance(1, heapManager->descriptorHeapHandleSize);
    }

    heapManager->descriptorHeapHandlePointer = dsvStart + imageCount;

    return dsvStart;
}

IDXGISwapChain4* DX12Device::CreateSwapChain(HWND hWnd, ID3D12CommandQueue* commandQueue, int width, int height, int bufferCount, DXGI_FORMAT format, UINT debug)
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


    swapChainDesc.Format = format;


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


    if (FAILED(hr = dxgiFactory4->CreateSwapChainForHwnd(commandQueue, hWnd, &swapChainDesc, NULL, NULL, &swapChain1)))
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


EntryHandle DX12Device::CreateSwapChain(HWND hWnd, EntryHandle commandQueue, int width, int height, int bufferCount, DXGI_FORMAT format, UINT debug)
{
    ID3D12CommandQueue* queue = (ID3D12CommandQueue*)GetAndValidateItem(commandQueue, D12QUEUE);

    IDXGISwapChain4* swcChain = CreateSwapChain(hWnd, queue, width, height, bufferCount, format, debug);

    return AllocTypeForEntry(swcChain, DXGISWAPCHAIN);
}

ID3D12CommandAllocator* DX12Device::CreateCommandAllocator(ID3D12Device2* device, D3D12_COMMAND_LIST_TYPE type)
{
    ID3D12CommandAllocator* commandAllocator = NULL;

    if (FAILED(device->CreateCommandAllocator(type, IID_PPV_ARGS(&commandAllocator))))
    {
        printf("Failed to create command pool\n");
    }

    return commandAllocator;
}



EntryHandle DX12Device::CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE type)
{
    ID3D12CommandAllocator* commandAllocator = CreateCommandAllocator(deviceHandle, type);

    return AllocTypeForEntry(commandAllocator, D12COMMANDPOOL);
}

ID3D12GraphicsCommandList7* DX12Device::CreateCommandList(ID3D12Device2* device,
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

    commandList->Release();

    return commandList7;
}

EntryHandle DX12Device::CreateCommandList(EntryHandle commandAllocator, D3D12_COMMAND_LIST_TYPE type)
{
    ID3D12CommandAllocator* commAllocator = (ID3D12CommandAllocator*)GetAndValidateItem(commandAllocator, D12COMMANDPOOL);

    ID3D12GraphicsCommandList* commandList = CreateCommandList(deviceHandle, commAllocator, type);

    return AllocTypeForEntry(commandList, D12COMMANDBUFFER7);
}

ID3D12Fence* DX12Device::CreateFence(ID3D12Device2* device)
{
    ID3D12Fence* fence = NULL;
    if (FAILED(device->CreateFence(0, D3D12_FENCE_FLAG_NONE, IID_PPV_ARGS(&fence))))
    {
        printf("Failed to create a fence\n");
    }

    return fence;

}

HANDLE DX12Device::CreateEventHandle()
{
    HANDLE fenceEvent;
    fenceEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (fenceEvent == INVALID_HANDLE_VALUE)
        printf("Failed to create fence event.\n");

    return fenceEvent;
}

EntryHandle DX12Device::CreateFenceObject()
{
    DX12Fence* fence = (DX12Fence*)AllocFromDeviceStorage(sizeof(DX12Fence), alignof(HANDLE));

    ID3D12Fence* fenceHandle = CreateFence(deviceHandle);

    fence->fenceEvent = CreateEventHandle();
    fence->fenceHandle = AllocTypeForEntry(fenceHandle, D12FENCEHANDLE);
    fence->fenceValue = 0;

    return AllocTypeForEntry(fence, D12FENCEOBJECT);
}

uint64_t DX12Device::Signal(EntryHandle commandQueue, EntryHandle fenceObjIndex, uint64_t& fenceValue)
{
    uint64_t fenceValueForSignal = ++fenceValue;

    DX12Fence* fence = (DX12Fence*)GetAndValidateItem(fenceObjIndex, D12FENCEOBJECT);

    ID3D12Fence* fenceHandle = (ID3D12Fence*)GetAndValidateItem(fence->fenceHandle, D12FENCEHANDLE);

    ID3D12CommandQueue* lQueueHandle = (ID3D12CommandQueue*)GetAndValidateItem(commandQueue, D12QUEUE);

    if (FAILED(lQueueHandle->Signal(fenceHandle, fenceValueForSignal)))
    {
        printf("Could not signal fence\n");
        return ~0ui64;
    }
    return fenceValueForSignal;

}

uint64_t DX12Device::Signal(EntryHandle commandQueue, EntryHandle fenceObjIndex)
{

    ID3D12CommandQueue* lQueueHandle = (ID3D12CommandQueue*)GetAndValidateItem(commandQueue, D12QUEUE);

    DX12Fence* fence = (DX12Fence*)GetAndValidateItem(fenceObjIndex, D12FENCEOBJECT);

    ID3D12Fence* fenceHandle = (ID3D12Fence*)GetAndValidateItem(fence->fenceHandle, D12FENCEHANDLE);

    uint64_t fenceValueForSignal = ++fence->fenceValue;

    if (FAILED(lQueueHandle->Signal(fenceHandle, fenceValueForSignal)))
    {
        printf("Could not signal fence\n");
        return ~0ui64;
    }
    return fenceValueForSignal;

}

void DX12Device::WaitForFenceValue(EntryHandle fenceObjIndex, uint64_t fenceValue, DWORD duration)
{

    DX12Fence* fence = (DX12Fence*)GetAndValidateItem(fenceObjIndex, D12FENCEOBJECT);

    ID3D12Fence* fenceHandle = (ID3D12Fence*)GetAndValidateItem(fence->fenceHandle, D12FENCEHANDLE);

    if (fenceHandle->GetCompletedValue() < fenceValue)
    {
        if (SUCCEEDED(fenceHandle->SetEventOnCompletion(fenceValue, fence->fenceEvent)))
        {
            WaitForSingleObject(fence->fenceEvent, duration);
        }
    }
}

void DX12Device::Flush(EntryHandle fenceObjIndex, EntryHandle commandQueue, uint64_t& fenceValue)
{
    uint64_t fenceValueForSignal = Signal(commandQueue, fenceObjIndex, fenceValue);

    WaitForFenceValue(fenceObjIndex, fenceValueForSignal, UINT32_MAX);
}


void DX12Device::Flush(EntryHandle fenceObjIndex, EntryHandle commandQueue)
{
    uint64_t fenceValueForSignal = Signal(commandQueue, fenceObjIndex);

    WaitForFenceValue(fenceObjIndex, fenceValueForSignal, UINT32_MAX);
}

EntryHandle DX12Device::CreateShaderBlob(const char* shaderfile)
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


EntryHandle DX12Device::CreateHostBuffer(UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags)
{

    DX12DriverMemoryBuffer* dmb = (DX12DriverMemoryBuffer*)AllocFromDeviceStorage(sizeof(DX12DriverMemoryBuffer), 4);

    dmb->sizeOfAlloc = size;
    dmb->currentPointer = 0;

    ID3D12Resource* resource = CreateBuffer(deviceHandle, size, format, flags, D3D12_HEAP_TYPE_UPLOAD);

    dmb->bufferHandle = AllocTypeForEntry(resource, D12RESOURCEHANDLE);

    dmb->currentState = dmb->initialState = D3D12_RESOURCE_STATE_GENERIC_READ;

    return AllocTypeForEntry(dmb, D12BUFFERMEMORYPOOL);

}

EntryHandle DX12Device::CreateDeviceBuffer(UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags)
{
    DX12DriverMemoryBuffer* dmb = (DX12DriverMemoryBuffer*)AllocFromDeviceStorage(sizeof(DX12DriverMemoryBuffer), 4);

    dmb->sizeOfAlloc = size;
    dmb->currentPointer = 0;

    ID3D12Resource* resource = CreateBuffer(deviceHandle, size, format, flags, D3D12_HEAP_TYPE_DEFAULT);

    dmb->bufferHandle = AllocTypeForEntry(resource, D12RESOURCEHANDLE);

    dmb->currentState = dmb->initialState = D3D12_RESOURCE_STATE_GENERIC_READ;

    return AllocTypeForEntry(dmb, D12BUFFERMEMORYPOOL);

}

ID3D12Resource* DX12Device::CreateBuffer(ID3D12Device2* device, SIZE_T size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags, D3D12_HEAP_TYPE heapType)
{
    D3D12_RESOURCE_DESC bufferDesc = {};
    bufferDesc.Dimension = D3D12_RESOURCE_DIMENSION_BUFFER;
    bufferDesc.Alignment = 0;
    bufferDesc.Width = size;
    bufferDesc.Height = 1;
    bufferDesc.DepthOrArraySize = 1;
    bufferDesc.MipLevels = 1;
    bufferDesc.Format = format;
    bufferDesc.SampleDesc.Count = 1;
    bufferDesc.SampleDesc.Quality = 0;
    bufferDesc.Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR;
    bufferDesc.Flags = flags;


    D3D12_HEAP_PROPERTIES heapProps = {};
    heapProps.Type = heapType;

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

EntryHandle DX12Device::CreateBufferView(SIZE_T offset, SIZE_T bufferStrideSize, UINT numStrideCopies, DXGI_FORMAT format, UINT numViewsToCreate)
{
  
	UINT firstElement = (UINT)(offset / bufferStrideSize);

    DX12BufferView* bufferViews = (DX12BufferView*)AllocFromDeviceStorage(sizeof(DX12BufferView) * numViewsToCreate, 4);

	for (UINT i = 0; i < numViewsToCreate; i++)
	{
	    bufferViews[i] = {};
		bufferViews[i].format = format; // structured buffer
		bufferViews[i].firstOffset = firstElement;
        bufferViews[i].numStructuredStrides = numStrideCopies;
		bufferViews[i].structuredStrideSize = (UINT)bufferStrideSize;
        bufferViews[i].rawBufferOrNot = 0;
	
        firstElement += numStrideCopies;
	}

    EntryHandle drivViewIndex = AllocTypeForEntry(bufferViews, D12BUFFERVIEW);

    return drivViewIndex;
}


EntryHandle DX12Device::CreateSampler(UINT maxMipLevels)
{

    D3D12_SAMPLER_DESC* samplerDesc = (D3D12_SAMPLER_DESC*)AllocFromDeviceStorage(sizeof(D3D12_SAMPLER_DESC), 4);

    samplerDesc->Filter = D3D12_FILTER_MIN_MAG_MIP_LINEAR;
    samplerDesc->AddressU = D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    samplerDesc->AddressV = D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    samplerDesc->AddressW = D3D12_TEXTURE_ADDRESS_MODE_WRAP;
    samplerDesc->MipLODBias = 0.0f;
    samplerDesc->MaxAnisotropy = 1;
    samplerDesc->ComparisonFunc = D3D12_COMPARISON_FUNC_NONE;
    samplerDesc->BorderColor[0] = 0.0f;
    samplerDesc->BorderColor[1] = 0.0f;
    samplerDesc->BorderColor[2] = 0.0f;
    samplerDesc->BorderColor[3] = 0.0f;
    samplerDesc->MinLOD = 0.0f;
    samplerDesc->MaxLOD = (FLOAT)maxMipLevels;

    EntryHandle samplerIndex = AllocTypeForEntry(samplerDesc, D12SAMPLER);

    return samplerIndex;
}

EntryHandle DX12Device::CreateImageView(DXGI_FORMAT format, UINT planeSlice, UINT mipLevels)
{
    DX12ImageViewHandle* viewHandle = (DX12ImageViewHandle*)AllocFromDeviceStorage(sizeof(DX12ImageViewHandle), 4);

   // viewHandle->resourceIndex = imageResource;

    viewHandle->shaderViewDesc.ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D;
    viewHandle->shaderViewDesc.Format = format; 
    viewHandle->shaderViewDesc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;
    viewHandle->shaderViewDesc.Texture2D.MipLevels = mipLevels;
    viewHandle->shaderViewDesc.Texture2D.PlaneSlice = planeSlice;
    viewHandle->shaderViewDesc.Texture2D.MostDetailedMip = 0;
    viewHandle->shaderViewDesc.Texture2D.ResourceMinLODClamp = 0;

    EntryHandle viewHandleIndex = AllocTypeForEntry(viewHandle, D12IMAGEVIEW);

    return viewHandleIndex;
}

SIZE_T DX12Device::AllocFromDriverMemoryBuffer(EntryHandle bufferPoolIndex, SIZE_T allocSize, SIZE_T alignment)
{
    DX12DriverMemoryBuffer* dmb = (DX12DriverMemoryBuffer*)GetAndValidateItem(bufferPoolIndex, D12BUFFERMEMORYPOOL);

    SIZE_T current = dmb->currentPointer;
    SIZE_T start = current;

    current = (current + alignment - 1) & ~(alignment - 1);

    dmb->currentPointer += (allocSize + (current - start));

    return current;
}


void DX12Device::CopyDescriptors(UINT numDescriptors, UINT startInSrc, UINT startInDst, EntryHandle srcHeap, EntryHandle destHeap, UINT srcHeapSize, UINT dstHeapSize, D3D12_DESCRIPTOR_HEAP_TYPE type)
{
    ID3D12DescriptorHeap* srcDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(srcHeap, D12DESCRIPTORHEAP);

    DX12CPUDescriptorHandle srcHandle(srcDescriptor->GetCPUDescriptorHandleForHeapStart(), startInSrc, srcHeapSize);

    ID3D12DescriptorHeap* dstDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(destHeap, D12DESCRIPTORHEAP);

    DX12CPUDescriptorHandle dstHandle(dstDescriptor->GetCPUDescriptorHandleForHeapStart(), startInDst, dstHeapSize);

    deviceHandle->CopyDescriptorsSimple(numDescriptors, dstHandle, srcHandle, type);
}

void DX12Device::ExecuteCommandListsOnQueue(EntryHandle queueIndex, ID3D12CommandList** lists, UINT numOfLists)
{
    ID3D12CommandQueue* lQueueHandle = (ID3D12CommandQueue*)GetAndValidateItem(queueIndex, D12QUEUE);

    lQueueHandle->ExecuteCommandLists(numOfLists, lists);
}

void DX12Device::WriteToHostMemory(EntryHandle memoryBuffer, void* data, SIZE_T size, SIZE_T offset, SIZE_T stride, int copies)
{
    void* mappedData = nullptr;

    DX12DriverMemoryBuffer* dmb = (DX12DriverMemoryBuffer*)GetAndValidateItem(memoryBuffer, D12BUFFERMEMORYPOOL);

    ID3D12Resource* buffer = (ID3D12Resource*)GetAndValidateItem(dmb->bufferHandle, D12RESOURCEHANDLE);

    buffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData);

    for (int i = 0; i < copies; i++)
    {
        memcpy((void*)(cdata + offset), data, size);
        offset += stride;
    }

    buffer->Unmap(0, nullptr);
}

void DX12Device::WriteToDeviceLocalMemory(EntryHandle deviceLocalMemBuffer, EntryHandle stagingBufferIndex, DX12GraphicsCommandRecorder* commandRecorder, void* data, SIZE_T size, SIZE_T offset, SIZE_T stride, int copies)
{
    DX12DriverMemoryBuffer* dmb = (DX12DriverMemoryBuffer*)GetAndValidateItem(deviceLocalMemBuffer, D12BUFFERMEMORYPOOL);

    DX12DriverMemoryBuffer* stagingDmb = (DX12DriverMemoryBuffer*)GetAndValidateItem(stagingBufferIndex, D12BUFFERMEMORYPOOL);

    ID3D12Resource* stagingBuffer = (ID3D12Resource*)GetAndValidateItem(stagingDmb->bufferHandle, D12RESOURCEHANDLE);

    void* mappedData = nullptr;

    SIZE_T allocLoc = AllocFromDriverMemoryBuffer(stagingBufferIndex, stride * copies, 64);

    stagingBuffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData)+allocLoc;

    for (int i = 0; i < copies; i++)
    {
        memcpy((void*)(cdata), data, size);
        cdata += stride;
    }

    stagingBuffer->Unmap(0, nullptr);

    commandRecorder->CopyBufferToRegion( stagingDmb->bufferHandle, dmb->bufferHandle, allocLoc, offset, stride * copies);
}

void DX12Device::TransitionBufferBarrier(EntryHandle cmdBufferHandle, EntryHandle resourceHandle, D3D12_BARRIER_SYNC srcSync, D3D12_BARRIER_ACCESS srcAccess, D3D12_BARRIER_SYNC dstSync, D3D12_BARRIER_ACCESS dstAccess)
{
    ID3D12GraphicsCommandList7* cmdBuffer = (ID3D12GraphicsCommandList7*)GetAndValidateItem(cmdBufferHandle, D12COMMANDBUFFER7);

    ID3D12Resource* resource = (ID3D12Resource*)GetAndValidateItem(resourceHandle, D12RESOURCEHANDLE);

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


void DX12Device::TransitionImageResource(ID3D12GraphicsCommandList7* cmdBuffer, ID3D12Resource* imageResource,
    D3D12_BARRIER_SYNC srcSync, D3D12_BARRIER_ACCESS srcAccess,
    D3D12_BARRIER_SYNC dstSync, D3D12_BARRIER_ACCESS dstAccess, D3D12_BARRIER_LAYOUT srcLayout, D3D12_BARRIER_LAYOUT dstLayout, UINT baseArrayIndex, UINT numArrayLayers, UINT baseMipIndex, UINT numMipLevels)
{
    D3D12_TEXTURE_BARRIER barrierInfo{};
    barrierInfo.Flags = D3D12_TEXTURE_BARRIER_FLAG_NONE;
    barrierInfo.pResource = imageResource;
    barrierInfo.Subresources.FirstArraySlice = baseArrayIndex;
    barrierInfo.Subresources.IndexOrFirstMipLevel = baseMipIndex;
    barrierInfo.Subresources.FirstPlane = 0;
    barrierInfo.Subresources.NumArraySlices = numArrayLayers;
    barrierInfo.Subresources.NumMipLevels = numMipLevels;
    barrierInfo.Subresources.NumPlanes = 1;
    barrierInfo.SyncBefore = srcSync;
    barrierInfo.SyncAfter = dstSync;
    barrierInfo.AccessBefore = srcAccess;
    barrierInfo.AccessAfter = dstAccess;
    barrierInfo.LayoutBefore = srcLayout;
    barrierInfo.LayoutAfter = dstLayout;

    D3D12_BARRIER_GROUP barrierGroup = {};
    barrierGroup.Type = D3D12_BARRIER_TYPE_TEXTURE;
    barrierGroup.NumBarriers = 1;
    barrierGroup.pTextureBarriers = &barrierInfo;

    cmdBuffer->Barrier(1, &barrierGroup);
}

void DX12Device::WriteToImageDeviceLocalMemory(EntryHandle imageHandle, DX12GraphicsCommandRecorder* commandRecorder, EntryHandle stagingBufferIndex, char* data,
    UINT width, UINT height,
    UINT componentCount, UINT totalImageSize,
    DXGI_FORMAT format,
    UINT mipLevels, UINT layers)
{
    void* mappedData = nullptr;

    UINT stride = ((width * componentCount) + (255)) & ~255;

    DX12DriverMemoryBuffer* dmb = (DX12DriverMemoryBuffer*)GetAndValidateItem(stagingBufferIndex, D12BUFFERMEMORYPOOL);

    SIZE_T allocLoc = AllocFromDriverMemoryBuffer(stagingBufferIndex, stride * height, 255);

    ID3D12Resource* stagingBuffer = (ID3D12Resource*)GetAndValidateItem(dmb->bufferHandle, D12RESOURCEHANDLE);

    stagingBuffer->Map(0, nullptr, &mappedData);

    uintptr_t cdata = (uintptr_t)(mappedData)+allocLoc;

    for (UINT i = 0; i < height; i++)
    {
        memcpy((void*)(cdata), data + (i * width * componentCount), width * componentCount);
        cdata += stride;
    }


    stagingBuffer->Unmap(0, nullptr);

    DX12ImageHandle* imageHandleT = (DX12ImageHandle*)GetAndValidateItem(imageHandle, D12IMAGEHANDLE);

    commandRecorder->TransitionImageResource(imageHandleT->resourceIndex,
        D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS,
        D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST,
        D3D12_BARRIER_LAYOUT_COMMON, D3D12_BARRIER_LAYOUT_COPY_DEST,
        0, layers, 0, mipLevels
    );

    D3D12_TEXTURE_COPY_LOCATION dest{}, src{};

    src.Type = D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT;
    src.PlacedFootprint.Offset = allocLoc;
    src.PlacedFootprint.Footprint.Depth = 1;
    src.PlacedFootprint.Footprint.Width = width;
    src.PlacedFootprint.Footprint.Height = height;
    src.PlacedFootprint.Footprint.Format = format;
    src.PlacedFootprint.Footprint.RowPitch = stride;

    dest.Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX;
    dest.SubresourceIndex = 0;

    commandRecorder->CopyTextureRegion(&src, &dest, dmb->bufferHandle, imageHandleT->resourceIndex, NULL, 0, 0, 0);

    commandRecorder->TransitionImageResource(imageHandleT->resourceIndex,
        D3D12_BARRIER_SYNC_COPY, D3D12_BARRIER_ACCESS_COPY_DEST,
        D3D12_BARRIER_SYNC_PIXEL_SHADING, D3D12_BARRIER_ACCESS_SHADER_RESOURCE,

        D3D12_BARRIER_LAYOUT_COPY_DEST, D3D12_BARRIER_LAYOUT_SHADER_RESOURCE,
        0, layers, 0, mipLevels
    );


}


EntryHandle DX12Device::CreateCommittedImageResource(UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension)
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
    deviceHandle->CreateCommittedResource(
        &heapProps,
        D3D12_HEAP_FLAG_NONE,
        &imageDesc,
        D3D12_RESOURCE_STATE_COMMON, // initial state
        nullptr,
        IID_PPV_ARGS(&imageHandle)
    );

    return AllocTypeForEntry(imageHandle, D12RESOURCEHANDLE);
}

EntryHandle DX12Device::CreateGraphicsPipelineObject(DX12PipelineCreationInfo* info, DX12ConstantBufferPipelineArguments* constantArgs)
{
    DX12GraphicsPipelineObject* obj = (DX12GraphicsPipelineObject*)AllocFromDeviceStorage(sizeof(DX12GraphicsPipelineObject), alignof(DX12GraphicsPipelineObject));

	obj->rootSignature = info->rootSignature;
	obj->pipelineState = info->pipelineState;

	obj->heapsCount = info->heapsCounts;


	for (UINT i = 0; i < info->heapsCounts && i < 8; i++)
	{
		obj->descriptorHeap[i] = info->descriptorHeapHandles[i];
	}

	obj->descriptorTableCount = info->descriptorTableCount;

	for (UINT i = 0; i < info->descriptorTableCount && i < 8; i++)
	{
        obj->tables[i] = info->tables[i];
	}

	obj->topology = info->topology;
	obj->instanceCount = info->instanceCount;

	obj->vertexBuffer = info->vertexBufferHandle;
	obj->vertexBufferOffset = info->vertexBufferOffset;
	obj->vertexBufferSize = info->vertexBufferSize;
	obj->vertexSize = info->vertexSize;
	obj->vertexCount = info->vertexCount;

	obj->indexBuffer = info->indexBuffer;
	obj->indexBufferOffset = info->indexBufferOffset;
	obj->indexBufferSize = info->indexBufferSize;
	obj->indexSize = info->indexSize;
	obj->indexCount = info->indexCount;

	obj->cbvArgsCount = info->cbvArgsCount;

    obj->constantsRangesCount = (obj->cbvArgsCount ? 1 : 0);

    memcpy(obj->cbvArgs, constantArgs, sizeof(DX12ConstantBufferPipelineArguments) * obj->cbvArgsCount);

    obj->device = this;

    return AllocTypeForEntry(obj, D12PIPELINEOBJECT);

}

ID3D12Resource* DX12Device::CreatePlacedImageResource(DX12ImageMemoryPool* pool, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension, D3D12_RESOURCE_STATES resourceState)
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
        deviceHandle->GetResourceAllocationInfo(0, 1, &imageDesc);


    UINT64 location = (pool->currentPointer + allocInfo.Alignment - 1) & ~(allocInfo.Alignment - 1);

    pool->currentPointer += (allocInfo.SizeInBytes + (location - pool->currentPointer));

    ID3D12Heap* heap = (ID3D12Heap*)GetAndValidateItem(pool->heap, D12MEMHEAP);

    ID3D12Resource* imageHandle = nullptr;

    deviceHandle->CreatePlacedResource(
        heap,
        location,
        &imageDesc,
        resourceState,
        nullptr,
        IID_PPV_ARGS(&imageHandle)
    );

    return imageHandle;
}

EntryHandle DX12Device::CreateSampledImageHandle(EntryHandle poolIdx, UINT width, UINT height, UINT depth, UINT mips, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension)
{
    DX12ImageHandle* handle = (DX12ImageHandle*)AllocFromDeviceStorage(sizeof(DX12ImageHandle), alignof(DX12ImageHandle));

    handle->arrayCount = depth;
    handle->memHeapIndex = poolIdx;
    handle->mipLevels = mips;
    handle->format = format;
    handle->resourceIndex = CreateImageResourceFromPool(poolIdx, width, height, depth, mips, D3D12_RESOURCE_FLAG_NONE, format, dimension, D3D12_RESOURCE_STATE_COMMON);

    handle->views[0] = CreateImageView(format, 0, mips);

    return AllocTypeForEntry(handle, D12IMAGEHANDLE);
}

EntryHandle DX12Device::CreateImageResourceFromPool(EntryHandle poolIdx, UINT width, UINT height, UINT depth, UINT mips, D3D12_RESOURCE_FLAGS flags, DXGI_FORMAT format, D3D12_RESOURCE_DIMENSION dimension, D3D12_RESOURCE_STATES resourceState)
{
    DX12ImageMemoryPool* pool = (DX12ImageMemoryPool*)GetAndValidateItem(poolIdx, D12IMAGEMEMORYPOOL);

    ID3D12Resource* resource = CreatePlacedImageResource(pool, width, height, depth, mips, flags, format, dimension, resourceState);

    return AllocTypeForEntry(resource, D12RESOURCEHANDLE);
}

EntryHandle DX12Device::CreateRootSignature(DX12RootSignatureCreate* createInfo, D3D12_ROOT_SIGNATURE_FLAGS flags)
{
    ID3D12RootSignature* rootSignature;

    ID3DBlob* rootSigDescriptorLayout;

    D3D12_ROOT_SIGNATURE_DESC rsigDesc = {};

    rsigDesc.NumParameters = createInfo->numOfRootParameters;
    rsigDesc.pParameters = createInfo->rootParameters;
    rsigDesc.NumStaticSamplers = 0;
    rsigDesc.pStaticSamplers = NULL;
    rsigDesc.Flags = flags;

    D3D12SerializeRootSignature(&rsigDesc, D3D_ROOT_SIGNATURE_VERSION_1, &rootSigDescriptorLayout, nullptr);

    HRESULT hr = deviceHandle->CreateRootSignature(0, rootSigDescriptorLayout->GetBufferPointer(), rootSigDescriptorLayout->GetBufferSize(), IID_PPV_ARGS(&rootSignature));

    if (FAILED(hr))
    {
        printf("Failed to create root signature\n");
    }

    rootSigDescriptorLayout->Release();

    return AllocTypeForEntry(rootSignature, D12ROOTSIGNATURE);
}


void DX12Device::CreateSamplerDescriptor(EntryHandle samplerIndex, DX12DescriptorHeapManager* samplerDescriptorHeap, UINT heapIndex)
{
    ID3D12DescriptorHeap* sampDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(samplerDescriptorHeap->descriptorHeap, D12DESCRIPTORHEAP);

    DX12CPUDescriptorHandle samplerHandle(sampDescriptor->GetCPUDescriptorHandleForHeapStart(), heapIndex, samplerDescriptorHeap->descriptorHeapHandleSize);

    D3D12_SAMPLER_DESC* samplerDesc = (D3D12_SAMPLER_DESC*)GetAndValidateItem(samplerIndex, D12SAMPLER);

    deviceHandle->CreateSampler(samplerDesc, samplerHandle);
}

void DX12Device::CreateBufferSRVDescriptor(EntryHandle bufferPoolHandle, EntryHandle viewIndex, UINT subViewIndex, DX12DescriptorHeapManager* heap, UINT heapIndex)
{
  
    ID3D12Resource* bufferHandle = GetResourceHandleForMemoryBuffer(bufferPoolHandle);

    ID3D12DescriptorHeap* srvDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(heap->descriptorHeap, D12DESCRIPTORHEAP);

    DX12CPUDescriptorHandle srvHandle(srvDescriptor->GetCPUDescriptorHandleForHeapStart(), heapIndex, heap->descriptorHeapHandleSize);

    DX12BufferView* views = (DX12BufferView*)GetAndValidateItem(viewIndex, D12BUFFERVIEW);

    DX12BufferView& specView = views[subViewIndex];

    D3D12_SHADER_RESOURCE_VIEW_DESC srvDesc = {};
    srvDesc.ViewDimension = D3D12_SRV_DIMENSION_BUFFER;
    srvDesc.Format = specView.format; // structured buffer
    srvDesc.Buffer.FirstElement = specView.firstOffset;
    srvDesc.Buffer.NumElements = specView.numStructuredStrides;
    srvDesc.Buffer.StructureByteStride = specView.structuredStrideSize;
    srvDesc.Buffer.Flags = (specView.rawBufferOrNot ? D3D12_BUFFER_SRV_FLAG_RAW : D3D12_BUFFER_SRV_FLAG_NONE);
    srvDesc.Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING;

    deviceHandle->CreateShaderResourceView(bufferHandle, &srvDesc, srvHandle);
}

void DX12Device::CreateBufferUAVDescriptor(EntryHandle bufferPoolHandle, EntryHandle viewIndex, UINT subViewIndex, DX12DescriptorHeapManager* heap, UINT heapIndex)
{
    ID3D12Resource* bufferHandle = GetResourceHandleForMemoryBuffer(bufferPoolHandle);

    ID3D12DescriptorHeap* uavDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(heap->descriptorHeap, D12DESCRIPTORHEAP);

    DX12CPUDescriptorHandle uavHandle(uavDescriptor->GetCPUDescriptorHandleForHeapStart(), heapIndex, heap->descriptorHeapHandleSize);

    DX12BufferView* views = (DX12BufferView*)GetAndValidateItem(viewIndex, D12BUFFERVIEW);

    DX12BufferView& specView = views[subViewIndex];

    D3D12_UNORDERED_ACCESS_VIEW_DESC uavDesc{};

    uavDesc.ViewDimension = D3D12_UAV_DIMENSION_BUFFER;
    uavDesc.Format = specView.format; // structured buffer
    uavDesc.Buffer.FirstElement = specView.firstOffset;
    uavDesc.Buffer.NumElements = specView.numStructuredStrides;
    uavDesc.Buffer.StructureByteStride = specView.structuredStrideSize;
    uavDesc.Buffer.Flags = (specView.rawBufferOrNot ? D3D12_BUFFER_UAV_FLAG_RAW : D3D12_BUFFER_UAV_FLAG_NONE);
    uavDesc.Buffer.CounterOffsetInBytes = 0;


    deviceHandle->CreateUnorderedAccessView(bufferHandle, nullptr, &uavDesc, uavHandle);
}

void DX12Device::CreateBufferCBVDescriptor(EntryHandle bufferPoolHandle, EntryHandle viewIndex, UINT subViewIndex, DX12DescriptorHeapManager* heap, UINT heapIndex)
{
    ID3D12DescriptorHeap* cbvDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(heap->descriptorHeap, D12DESCRIPTORHEAP);

    DX12CPUDescriptorHandle cbvHandle(cbvDescriptor->GetCPUDescriptorHandleForHeapStart(), heapIndex, heap->descriptorHeapHandleSize);

    ID3D12Resource* bufferHandle = GetResourceHandleForMemoryBuffer(bufferPoolHandle);

    DX12BufferView* views = (DX12BufferView*)GetAndValidateItem(viewIndex, D12BUFFERVIEW);

    DX12BufferView& specView = views[subViewIndex];

    UINT64 offset = (specView.firstOffset * specView.structuredStrideSize);

    UINT size = (specView.numStructuredStrides * specView.structuredStrideSize);

    D3D12_CONSTANT_BUFFER_VIEW_DESC cbvDesc = {};

    cbvDesc.BufferLocation = bufferHandle->GetGPUVirtualAddress() + offset;
    cbvDesc.SizeInBytes = ((UINT)size + (255)) & ~255;

    deviceHandle->CreateConstantBufferView(&cbvDesc, cbvHandle);
}

void DX12Device::CreateImageSRVDescriptor(EntryHandle imageHandle, UINT viewIndex, DX12DescriptorHeapManager* heap, UINT heapIndex)
{
    ID3D12DescriptorHeap* srvDescriptor = (ID3D12DescriptorHeap*)GetAndValidateItem(heap->descriptorHeap, D12DESCRIPTORHEAP);

    DX12ImageHandle* imageHandleDriv = (DX12ImageHandle*)GetAndValidateItem(imageHandle, D12IMAGEHANDLE);

    ID3D12Resource* imageResourceHandle = (ID3D12Resource*)GetAndValidateItem(imageHandleDriv->resourceIndex, D12RESOURCEHANDLE);

    D3D12_SHADER_RESOURCE_VIEW_DESC* desc = (D3D12_SHADER_RESOURCE_VIEW_DESC*)GetAndValidateItem(imageHandleDriv->views[viewIndex], D12IMAGEVIEW);

    DX12CPUDescriptorHandle srvHandle(srvDescriptor->GetCPUDescriptorHandleForHeapStart(), heapIndex, heap->descriptorHeapHandleSize);

    deviceHandle->CreateShaderResourceView(imageResourceHandle, desc, srvHandle);
}

EntryHandle DX12Device::CreateSwapChainHandle(HWND hWnd, EntryHandle commandQueue, UINT numberOfImages, UINT width, UINT height, DXGI_FORMAT format)
{
    DX12SwapChain* dx12swc = (DX12SwapChain*)AllocFromDeviceStorage(sizeof(DX12SwapChain), 4);

    ID3D12CommandQueue* queue = (ID3D12CommandQueue*)GetAndValidateItem(commandQueue, D12QUEUE);

    IDXGISwapChain4* swcChain = CreateSwapChain(hWnd, queue, width, height, numberOfImages, format, 0);

    EntryHandle swcChainIndex = AllocTypeForEntry(swcChain, DXGISWAPCHAIN);

    dx12swc->height = height;
    dx12swc->width = width;
    dx12swc->rtvFormat = format;
    dx12swc->numberOfImages = numberOfImages;
    dx12swc->dxgiSwapChainHandle = swcChainIndex;

  /*  dx12swc->rtvDescriptorHeap = CreateDescriptorHeapManager(
        numberOfImages,
        D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
        D3D12_DESCRIPTOR_HEAP_FLAG_NONE
    ); */

    dx12swc->backBufferResources = (EntryHandle*)AllocFromDeviceStorage(sizeof(EntryHandle) * numberOfImages, alignof(EntryHandle));

    dx12swc->backbufferViews = (EntryHandle*)AllocFromDeviceStorage(sizeof(EntryHandle) * numberOfImages, alignof(EntryHandle));

    for (UINT i = 0; i < numberOfImages; ++i)
    {
        ID3D12Resource* backBuffer;

        if (FAILED(swcChain->GetBuffer(i, IID_PPV_ARGS(&backBuffer))))
        {
            printf("Failed to get back buffer handle from swapchain\n");
            return -1;
        }

        dx12swc->backBufferResources[i] = AllocTypeForEntry(backBuffer, D12RESOURCEHANDLE);

        dx12swc->backbufferViews[i] = CreateImageView(format, 0, 1);
    }

    return AllocTypeForEntry(dx12swc, D12SWAPCHAINHANDLE);
}

void DX12Device::TransitionSWCImageToRenderTarget(EntryHandle swcIndex, UINT currentImage, DX12GraphicsCommandRecorder* recorder)
{
    DX12SwapChain* dx12swc = (DX12SwapChain*)GetAndValidateItem(swcIndex, D12SWAPCHAINHANDLE);

    ID3D12Resource* backBuffer = (ID3D12Resource*)GetAndValidateItem(dx12swc->backBufferResources[currentImage], D12RESOURCEHANDLE);

   /* recorder->TransitionImageResource(backBuffer,
        D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS,
        D3D12_BARRIER_SYNC_RENDER_TARGET, D3D12_BARRIER_ACCESS_RENDER_TARGET,
        D3D12_BARRIER_LAYOUT_PRESENT, D3D12_BARRIER_LAYOUT_RENDER_TARGET,
        0, 1, 0, 1
    );*/
}

void DX12Device::BeginRenderPass(D3D12_RENDER_PASS_RENDER_TARGET_DESC *rtvDesc, UINT renderTargetDescritions, D3D12_RENDER_PASS_DEPTH_STENCIL_DESC* depthDesc, UINT depthStencilCount, DX12GraphicsCommandRecorder* recorder)
{
     
  

    D3D12_RENDER_PASS_DEPTH_STENCIL_DESC* ldepthDesc = nullptr;

    if (depthStencilCount)
        ldepthDesc = depthDesc;

    recorder->BeginRenderPass(renderTargetDescritions, rtvDesc, ldepthDesc, D3D12_RENDER_PASS_FLAG_NONE);


}

void DX12Device::BeginRenderPassForSWCMSAA(EntryHandle swcIndex, UINT currentImage, FLOAT clearColor[4], D3D12_RENDER_PASS_DEPTH_STENCIL_DESC* depthDesc, DX12GraphicsCommandRecorder* recorder, D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE beginningAccess)
{
    DX12SwapChain* dx12swc = (DX12SwapChain*)GetAndValidateItem(swcIndex, D12SWAPCHAINHANDLE);

    D3D12_VIEWPORT viewport{};
    viewport.Width = (FLOAT)dx12swc->width;
    viewport.Height = (FLOAT)dx12swc->height;
    viewport.MinDepth = 0.0f;
    viewport.MaxDepth = 1.0f;

    D3D12_RECT scissor{};
    scissor.left = 0;
    scissor.top = 0;
    scissor.right = dx12swc->width;
    scissor.bottom = dx12swc->height;

    UINT rtvCount = 1;
    D3D12_RENDER_PASS_RENDER_TARGET_DESC rtvDesc{};

    rtvDesc.BeginningAccess.Type = beginningAccess;

    if (beginningAccess == D3D12_RENDER_PASS_BEGINNING_ACCESS_TYPE_CLEAR)
    {
        rtvDesc.BeginningAccess.Clear.ClearValue.Format = dx12swc->rtvFormat;
        memcpy(rtvDesc.BeginningAccess.Clear.ClearValue.Color, clearColor, sizeof(FLOAT) * 4);
    }

    rtvDesc.EndingAccess.Type = D3D12_RENDER_PASS_ENDING_ACCESS_TYPE_RESOLVE;

   
        
     ID3D12Resource* backBuffer = (ID3D12Resource*)GetAndValidateItem(dx12swc->backBufferResources[currentImage], D12RESOURCEHANDLE);
     rtvDesc.EndingAccess.Resolve.pDstResource = backBuffer;
     rtvDesc.EndingAccess.Resolve.Format = dx12swc->rtvFormat;
     rtvDesc.EndingAccess.Resolve.ResolveMode = D3D12_RESOLVE_MODE_AVERAGE;
     rtvDesc.EndingAccess.Resolve.SubresourceCount = 1;
        //rtvDesc.cpuDescriptor = *msaaTarget;
       
 


    recorder->BeginRenderPass(rtvCount, &rtvDesc, depthDesc, D3D12_RENDER_PASS_FLAG_NONE);

    recorder->SetScissor(1, &scissor);
    recorder->SetViewports(1, &viewport);
}

void DX12Device::TransitionSWCToPresent(EntryHandle swcIndex, UINT currentImageIndex, DX12GraphicsCommandRecorder* recorder)
{

    DX12SwapChain* dx12swc = (DX12SwapChain*)GetAndValidateItem(swcIndex, D12SWAPCHAINHANDLE);

    ID3D12Resource* backBuffer = (ID3D12Resource*)GetAndValidateItem(dx12swc->backBufferResources[currentImageIndex], D12RESOURCEHANDLE);

    /*
    recorder->TransitionImageResource(backBuffer,

        D3D12_BARRIER_SYNC_RENDER_TARGET, D3D12_BARRIER_ACCESS_RENDER_TARGET,
        D3D12_BARRIER_SYNC_NONE, D3D12_BARRIER_ACCESS_NO_ACCESS,
        D3D12_BARRIER_LAYOUT_RENDER_TARGET, D3D12_BARRIER_LAYOUT_PRESENT,
        0, 1, 0, 1
    );
    */
}

int DX12Device::PresentSwapChainImage(EntryHandle swcIndex, UINT syncIntervalFlag, UINT presentFlags)
{
    DX12SwapChain* dx12swc = (DX12SwapChain*)GetAndValidateItem(swcIndex, D12SWAPCHAINHANDLE);

    IDXGISwapChain4* lSwapChain = (IDXGISwapChain4*)GetAndValidateItem(dx12swc->dxgiSwapChainHandle, DXGISWAPCHAIN);

    if (FAILED(lSwapChain->Present(syncIntervalFlag, presentFlags)))
    {
        printf("Cannot present image\n");
        return -1;
    }

    return 0;
}

UINT DX12Device::AcquireNextImageFromSWC(EntryHandle swcIndex)
{
    DX12SwapChain* dx12swc = (DX12SwapChain*)GetAndValidateItem(swcIndex, D12SWAPCHAINHANDLE);

    IDXGISwapChain4* lSwapChain = (IDXGISwapChain4*)GetAndValidateItem(dx12swc->dxgiSwapChainHandle, DXGISWAPCHAIN);

    return lSwapChain->GetCurrentBackBufferIndex();
}

EntryHandle DX12Device::CreateDescriptorHeapManager(UINT maxDescriptorHandles, D3D12_DESCRIPTOR_HEAP_TYPE type, D3D12_DESCRIPTOR_HEAP_FLAGS flags)
{
    DX12DescriptorHeapManager* manager = (DX12DescriptorHeapManager*)AllocFromDeviceStorage(sizeof(DX12DescriptorHeapManager), 4);

    manager->descriptorHeap = CreateDescriptorHeap(type, maxDescriptorHandles, flags, &manager->descriptorHeapHandleSize);
    manager->maxDescriptorHeapHandles = maxDescriptorHandles;
    manager->descriptorHeapHandlePointer = 0;
    manager->type = type;

    return AllocTypeForEntry(manager, D12DESCRIPTORMANAGER);

}

ID3D12DescriptorHeap* DX12Device::GetDescriptorHeapFromManager(EntryHandle managerIndex)
{
    DX12DescriptorHeapManager* heapManager = (DX12DescriptorHeapManager*)GetAndValidateItem(managerIndex, D12DESCRIPTORMANAGER);

    return (ID3D12DescriptorHeap*)GetAndValidateItem(heapManager->descriptorHeap, D12DESCRIPTORHEAP);
}


UINT DX12Device::GetDescriptorHeapSizeFromManager(EntryHandle managerIndex)
{
    DX12DescriptorHeapManager* heapManager = (DX12DescriptorHeapManager*)GetAndValidateItem(managerIndex, D12DESCRIPTORMANAGER);

    return heapManager->descriptorHeapHandleSize;
}

DX12GraphicsCommandRecorder DX12Device::CreateRecorder(EntryHandle commandListIndex, EntryHandle commandPoolIndex)
{
    DX12GraphicsCommandRecorder recorder{};

    recorder.device = this;
    recorder.commandPool = (ID3D12CommandAllocator*)GetAndValidateItem(commandPoolIndex, D12COMMANDPOOL);
    recorder.cmdList = (ID3D12GraphicsCommandList7*)GetAndValidateItem(commandListIndex, D12COMMANDBUFFER7);

    return recorder;
}

void DX12GraphicsPipelineObject::DrawObject(DX12GraphicsCommandRecorder* gCommandBuffer, UINT currentSet)
{
    gCommandBuffer->SetRootSignature(rootSignature);

    gCommandBuffer->SetDescriptorHeaps(descriptorHeap, heapsCount);
    
    gCommandBuffer->SetPipelineState(pipelineState);

    gCommandBuffer->SetPrimitiveTopology(topology);

    UINT pushArgsCount = cbvArgsCount;

    UINT pushRangeCount = constantsRangesCount;

    for (UINT j = 0; j < pushArgsCount; j++)
    {
        DX12ConstantBufferPipelineArguments* pipeArgs = &cbvArgs[j];
        gCommandBuffer->Set32BitConstants(
            pipeArgs->rootParamIndex,
            pipeArgs->sizeInBytes,
            pipeArgs->offsetInBytes,
            pipeArgs->data
        );
    }

    for (UINT j = pushRangeCount; j < pushRangeCount +descriptorTableCount; j++)
    {
        UINT descriptorTableIndex = j - pushRangeCount;
        UINT descriptorTableSize = tables[descriptorTableIndex].descriptorsCount;
        UINT descriptorTablesBase = tables[descriptorTableIndex].descriptorHeapBase;
        UINT heapindex = tables[descriptorTableIndex].descriptorHeapSelection;
        UINT descriptorTableOffset = descriptorTablesBase + (currentSet * descriptorTableSize);

        gCommandBuffer->SetDescriptorTable(j, descriptorHeap[tables[descriptorTableIndex].descriptorHeapSelection], descriptorTableOffset);
    }

    if (vertexBuffer != ~0ui64)
    {
        gCommandBuffer->SetVertexBuffers(&vertexBuffer, &vertexBufferSize, &vertexBufferOffset, &vertexSize, 1);
    }

    if (indexBuffer != ~0ui64)
    {
        gCommandBuffer->SetIndexBuffers(indexBuffer, indexBufferOffset, indexBufferSize, indexSize);

        gCommandBuffer->DrawIndexedInstanced(indexCount, instanceCount, 0, 0, 0);
    }
    else
    {
        gCommandBuffer->DrawInstanced(vertexCount, instanceCount, 0, 0);
    }

}

/*struct DX12GraphicsCommandRecorder
{
    ID3D12GraphicsCommandList* cmdList;
    ID3D12CommandAllocator* commandPool;
    EntryHandle fenceVal = ~0ui64;
    DX12Device* device;
*/
void DX12GraphicsCommandRecorder::SetDescriptorHeaps(EntryHandle* heaps, UINT heapCount)
{
    ID3D12DescriptorHeap** heapsHandles = (ID3D12DescriptorHeap**)device->AllocFromDeviceCache(sizeof(ID3D12DescriptorHeap*) * heapCount, 4);

    for (UINT j = 0; j < heapCount; j++)
    {
        heapsHandles[j] = (ID3D12DescriptorHeap*)device->GetDescriptorHeapFromManager(heaps[j]);
    }

    cmdList->SetDescriptorHeaps(heapCount, heapsHandles);
}

void DX12GraphicsCommandRecorder::SetPipelineState(EntryHandle pipelineState)
{
    ID3D12PipelineState* pipelineStateHandle = (ID3D12PipelineState*)device->GetAndValidateItem(pipelineState, D12PIPELINESTATE);

    cmdList->SetPipelineState(pipelineStateHandle);
}

void DX12GraphicsCommandRecorder::SetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY topology)
{
    cmdList->IASetPrimitiveTopology(topology);
}

void DX12GraphicsCommandRecorder::SetVertexBuffers(EntryHandle* vertexBufferIds, SIZE_T* vertexBufferSizes, SIZE_T* vertexBufferOffsets, UINT* vertexStrides, UINT vertexBufferCount)
{
    D3D12_VERTEX_BUFFER_VIEW *vertexViews = (D3D12_VERTEX_BUFFER_VIEW*)device->AllocFromDeviceCache(sizeof(D3D12_VERTEX_BUFFER_VIEW)*vertexBufferCount, alignof(D3D12_VERTEX_BUFFER_VIEW));

    for (UINT i = 0; i < vertexBufferCount; i++)
    {
        ID3D12Resource* vertexBufferHandle = (ID3D12Resource*)device->GetAndValidateItem(vertexBufferIds[i], D12RESOURCEHANDLE);

        vertexViews[i].BufferLocation = vertexBufferHandle->GetGPUVirtualAddress() + vertexBufferOffsets[i];
        vertexViews[i].SizeInBytes = (UINT)vertexBufferSizes[i];
        vertexViews[i].StrideInBytes = vertexStrides[i];
    }

    cmdList->IASetVertexBuffers(0, vertexBufferCount, vertexViews);
}

void DX12GraphicsCommandRecorder::SetIndexBuffers(EntryHandle indexBufferId, SIZE_T indexBufferOffset, SIZE_T indexBufferSize, UINT indexStride)
{
    D3D12_INDEX_BUFFER_VIEW indexView{};

    ID3D12Resource* indexBufferHandle = (ID3D12Resource*)device->GetAndValidateItem(indexBufferId, D12RESOURCEHANDLE);

    indexView.BufferLocation = indexBufferHandle->GetGPUVirtualAddress() + indexBufferOffset;
    indexView.SizeInBytes = (UINT)indexBufferSize;
    indexView.Format = (indexStride == 2) ? DXGI_FORMAT_R16_UINT : DXGI_FORMAT_R32_UINT;

    cmdList->IASetIndexBuffer(&indexView);
}

void DX12GraphicsCommandRecorder::DrawIndexedInstanced(UINT indexCount, UINT instanceCount, UINT startingIndex, UINT startingVertex, UINT firstInstance)
{
    cmdList->DrawIndexedInstanced(indexCount, instanceCount, startingIndex, startingVertex, firstInstance);
}

void DX12GraphicsCommandRecorder::DrawInstanced(UINT vertexCount, UINT instanceCount, UINT startingVertex, UINT firstInstance)
{
    cmdList->DrawInstanced(vertexCount, instanceCount, startingVertex, firstInstance);
}

void DX12GraphicsCommandRecorder::Set32BitConstants(UINT rootParamIndex, UINT sizeInBytes, UINT offsetInBytes, void* data)
{
    cmdList->SetGraphicsRoot32BitConstants(
        rootParamIndex,
        sizeInBytes / 4,
        data,
        offsetInBytes / 4
    );
}

void DX12GraphicsCommandRecorder::SetDescriptorTable(UINT rootParam, EntryHandle heapIdx, UINT offsetInDescriptorHeap)
{
    DX12GPUDescriptorHandle handle = device->GetGPUHandleFromDescriptorManager(heapIdx, offsetInDescriptorHeap);
    cmdList->SetGraphicsRootDescriptorTable(rootParam, handle);
}

void DX12GraphicsCommandRecorder::SetRootSignature(EntryHandle rootIndex)
{
    ID3D12RootSignature* sign = (ID3D12RootSignature*)device->GetAndValidateItem(rootIndex, D12ROOTSIGNATURE);

    cmdList->SetGraphicsRootSignature(sign);
}

void DX12GraphicsCommandRecorder::ResetCommandBuffer()
{
    cmdList->Reset(commandPool, nullptr);
}

void DX12GraphicsCommandRecorder::ResetCommandPool()
{
    commandPool->Reset();
}

void DX12GraphicsCommandRecorder::ResetCommandPoolandBuffer()
{
    ResetCommandPool(); //this order matters
    ResetCommandBuffer();
}
void DX12GraphicsCommandRecorder::SetViewports(UINT viewportCount, D3D12_VIEWPORT* viewports)
{
    cmdList->RSSetViewports(viewportCount, viewports);
}

void DX12GraphicsCommandRecorder::SetScissor(UINT scissorCount, D3D12_RECT* rects)
{
    cmdList->RSSetScissorRects(scissorCount, rects);
}

void DX12GraphicsCommandRecorder::BeginRenderPass(UINT numRenderTargets, D3D12_RENDER_PASS_RENDER_TARGET_DESC* renderTargetDescriptions, D3D12_RENDER_PASS_DEPTH_STENCIL_DESC* depthDesc, D3D12_RENDER_PASS_FLAGS renderPassFlags)
{
    cmdList->BeginRenderPass(1, renderTargetDescriptions, depthDesc, renderPassFlags);
}

void DX12GraphicsCommandRecorder::EndRenderPass()
{
    cmdList->EndRenderPass();
}

int DX12GraphicsCommandRecorder::CloseCommandBuffer()
{
    if (FAILED(cmdList->Close()))
    {
        return -1;
    }
    return 0;
}

void DX12GraphicsCommandRecorder::TransitionImageResource(EntryHandle imageResource,
    D3D12_BARRIER_SYNC srcSync, D3D12_BARRIER_ACCESS srcAccess,
    D3D12_BARRIER_SYNC dstSync, D3D12_BARRIER_ACCESS dstAccess, D3D12_BARRIER_LAYOUT srcLayout, D3D12_BARRIER_LAYOUT dstLayout, UINT baseArrayIndex, UINT numArrayLayers, UINT baseMipIndex, UINT numMipLevels)
{

    ID3D12Resource* imageResourceHandle = (ID3D12Resource*)device->GetAndValidateItem(imageResource, D12RESOURCEHANDLE);

    D3D12_TEXTURE_BARRIER barrierInfo{};
    barrierInfo.Flags = D3D12_TEXTURE_BARRIER_FLAG_NONE;
    barrierInfo.pResource = imageResourceHandle;
    barrierInfo.Subresources.FirstArraySlice = baseArrayIndex;
    barrierInfo.Subresources.IndexOrFirstMipLevel = baseMipIndex;
    barrierInfo.Subresources.FirstPlane = 0;
    barrierInfo.Subresources.NumArraySlices = numArrayLayers;
    barrierInfo.Subresources.NumMipLevels = numMipLevels;
    barrierInfo.Subresources.NumPlanes = 1;
    barrierInfo.SyncBefore = srcSync;
    barrierInfo.SyncAfter = dstSync;
    barrierInfo.AccessBefore = srcAccess;
    barrierInfo.AccessAfter = dstAccess;
    barrierInfo.LayoutBefore = srcLayout;
    barrierInfo.LayoutAfter = dstLayout;

    D3D12_BARRIER_GROUP barrierGroup = {};
    barrierGroup.Type = D3D12_BARRIER_TYPE_TEXTURE;
    barrierGroup.NumBarriers = 1;
    barrierGroup.pTextureBarriers = &barrierInfo;

    cmdList->Barrier(1, &barrierGroup);
}

void DX12GraphicsCommandRecorder::TransitionBufferBarrier(EntryHandle resourceHandle, D3D12_BARRIER_SYNC srcSync, D3D12_BARRIER_ACCESS srcAccess, D3D12_BARRIER_SYNC dstSync, D3D12_BARRIER_ACCESS dstAccess)
{
    ID3D12Resource* resource = (ID3D12Resource*)device->GetAndValidateItem(resourceHandle, D12RESOURCEHANDLE);

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

    cmdList->Barrier(1, &barrierGroup);
}

void DX12GraphicsCommandRecorder::CopyBufferToRegion(EntryHandle srcResource, EntryHandle dstResource, UINT64 srcOffset, UINT64 dstOffset, UINT64 numBytes)
{
    ID3D12Resource* srcResourceHandle = (ID3D12Resource*)device->GetAndValidateItem(srcResource, D12RESOURCEHANDLE);
    ID3D12Resource* dstResourceHandle = (ID3D12Resource*)device->GetAndValidateItem(dstResource, D12RESOURCEHANDLE);

    cmdList->CopyBufferRegion(dstResourceHandle, dstOffset, srcResourceHandle, srcOffset, numBytes);
}


void DX12GraphicsCommandRecorder::CopyTextureRegion(D3D12_TEXTURE_COPY_LOCATION* src, D3D12_TEXTURE_COPY_LOCATION* dest, EntryHandle srcResource, EntryHandle dstResource, D3D12_BOX* box, UINT x, UINT y, UINT z)
{
    ID3D12Resource* srcResourceHandle = (ID3D12Resource*)device->GetAndValidateItem(srcResource, D12RESOURCEHANDLE);
    ID3D12Resource* dstResourceHandle = (ID3D12Resource*)device->GetAndValidateItem(dstResource, D12RESOURCEHANDLE);

    src->pResource = srcResourceHandle;
    dest->pResource = dstResourceHandle;
  
    cmdList->CopyTextureRegion(dest, x, y, z, src, box);
}