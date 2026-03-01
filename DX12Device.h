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

struct DX12Device
{

	ID3D12Device2* deviceHandle;

    char deviceData[4096];

    uintptr_t memHandleSlots[80];

    PoolItem deviceHandlePool[80];

    EntryHandle handlesPointer = 0;

    size_t deviceDataAlloc = 0;


    IDXGIAdapter4* GetAdapter(UINT createFactoryFlags)
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
        ID3D12Debug1* debug = nullptr;
        if (SUCCEEDED(D3D12GetDebugInterface(IID_PPV_ARGS(&debug))))
        {
            debug->EnableDebugLayer();
            debug->SetEnableGPUBasedValidation(TRUE); // optional but excellent
            debug->Release();
        }
    }


    void CreateDevice(bool debug)
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

    EntryHandle AllocTypeForEntry(void* data,  DX12ComType type)
    {
        EntryHandle poolHandle = handlesPointer++;
     

      
        
        deviceHandlePool[poolHandle].memoryHandle = (uintptr_t)data;
        deviceHandlePool[poolHandle].comType = type;

        return poolHandle;
    }

    void* GetAndValidateItem(EntryHandle poolHandle, DX12ComType type)
    {
        PoolItem* item = &deviceHandlePool[poolHandle];
        if (type != item->comType)
        {
            printf("Mismatched entry in pools");
            return NULL;
        }

        return (void*)(item->memoryHandle);
    }

    void* AllocFromDeviceStorage(size_t size, size_t alignment)
    {
        size_t current = deviceDataAlloc;

        current = (current + alignment - 1) & ~(alignment - 1);

        deviceDataAlloc += (size + (current - deviceDataAlloc));

        return (void*)(deviceData + current);
    }

    void ReleaseAllDriverCOMHandles()
    {
        for (size_t i = 0; i < handlesPointer; ++i)
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
                D12Fence* fenceObj = reinterpret_cast<D12Fence*>(rawPtr);
                CloseHandle(fenceObj->fenceEvent);
            }
            }
        }
        
        handlesPointer = 0;
        deviceDataAlloc = 0;
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

    int CreateRenderTargetView(EntryHandle swapChainIdx, EntryHandle descriptorHeapIdx, EntryHandle* outBuffers, UINT imageCount)
    {
        UINT rtvDescriptorSize = deviceHandle->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV);

        IDXGISwapChain4* swapChain = (IDXGISwapChain4*)GetAndValidateItem(swapChainIdx, DXGISWAPCHAIN);
        ID3D12DescriptorHeap* heap = (ID3D12DescriptorHeap*)GetAndValidateItem(descriptorHeapIdx, D12DESCRIPTORHEAP);

        CD3DX12_CPU_DESCRIPTOR_HANDLE rtvHandle(heap->GetCPUDescriptorHandleForHeapStart());

        for (UINT i = 0; i < imageCount; ++i)
        {
            ID3D12Resource* backBuffer;

            if (FAILED(swapChain->GetBuffer(i, IID_PPV_ARGS(&backBuffer))))
            {
                printf("Failed to get back buffer handle from swapchain\n");
                return -1;
            }

            deviceHandle->CreateRenderTargetView(backBuffer, NULL, rtvHandle);

            rtvHandle.Offset(rtvDescriptorSize);

            outBuffers[i] = AllocTypeForEntry(backBuffer, D12RESOURCEHANDLE);
        }

        return 0;
    }

    ID3D12Heap* CreateDX12Heap(SIZE_T size, SIZE_T alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType)
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

    EntryHandle CreateImageMemoryPool(size_t sizeOfPool, size_t alignment, D3D12_HEAP_FLAGS heapFlags, D3D12_HEAP_TYPE heapType)
    {
        ImageMemoryPool* pool = (ImageMemoryPool*)AllocFromDeviceStorage(sizeof(ImageMemoryPool), 4);

        ID3D12Heap* heap = CreateDX12Heap(sizeOfPool, alignment, heapFlags, heapType);

        pool->heap = AllocTypeForEntry(heap, D12MEMHEAP);
        pool->sizeOfHeap = sizeOfPool;
        pool->currentPointer = 0ui64;
        pool->alignment = alignment;

        EntryHandle ret = AllocTypeForEntry(pool, D12IMAGEMEMORYPOOL);

        return ret;
    }

    EntryHandle CreateTextureMemoryPool(size_t sizeOfPool, size_t alignment)
    {
        alignment = (alignment + D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT - 1) & ~(D3D12_DEFAULT_RESOURCE_PLACEMENT_ALIGNMENT - 1);

        return CreateImageMemoryPool(sizeOfPool, alignment, D3D12_HEAP_FLAG_ALLOW_ONLY_NON_RT_DS_TEXTURES, D3D12_HEAP_TYPE_DEFAULT);
    }

    EntryHandle CreateDSVRSVMemoryPool(size_t sizeOfPool, size_t alignment, bool msaa)
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

    int CreateDepthStencilView(EntryHandle descriptorHeapIdx, EntryHandle poolIdx, EntryHandle* outBuffers, UINT imageCount, UINT width, UINT height, DXGI_FORMAT format, UINT sampleCount)
    {

        UINT dsvDescriptorSize = deviceHandle->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_DSV);
   
        ID3D12DescriptorHeap* descHeap = (ID3D12DescriptorHeap*)GetAndValidateItem(descriptorHeapIdx, D12DESCRIPTORHEAP);

        ImageMemoryPool* pool = (ImageMemoryPool*)GetAndValidateItem(poolIdx, D12IMAGEMEMORYPOOL);

        CD3DX12_CPU_DESCRIPTOR_HANDLE dsvHandle(descHeap->GetCPUDescriptorHandleForHeapStart());

        ID3D12Heap* heap = (ID3D12Heap*)GetAndValidateItem(pool->heap, D12MEMHEAP);

        for (UINT i = 0; i < imageCount; ++i)
        {
            ID3D12Resource* depthBuffer = nullptr;

            D3D12_RESOURCE_DESC depthStencilDesc = {};
            depthStencilDesc.Dimension = D3D12_RESOURCE_DIMENSION_TEXTURE2D;
            depthStencilDesc.Width = width;
            depthStencilDesc.Height = height;
            depthStencilDesc.Format = format;
            depthStencilDesc.SampleDesc.Count = sampleCount;
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
                deviceHandle->GetResourceAllocationInfo(0, 1, &depthStencilDesc);

            UINT64 location = (pool->currentPointer + allocInfo.Alignment - 1) & ~(allocInfo.Alignment - 1);

            pool->currentPointer += (allocInfo.SizeInBytes + (location - pool->currentPointer));

            if (FAILED(deviceHandle->CreatePlacedResource(heap, location, &depthStencilDesc, D3D12_RESOURCE_STATE_DEPTH_WRITE, &clearValue, IID_PPV_ARGS(&depthBuffer))))
            {
                printf("Failed to create depth stencil resource\n");
                return -1;
            }

            D3D12_DEPTH_STENCIL_VIEW_DESC dsvDesc{};

            dsvDesc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
            dsvDesc.Format = DXGI_FORMAT_D32_FLOAT;
            dsvDesc.Flags = D3D12_DSV_FLAG_NONE;
            dsvDesc.Texture2D.MipSlice = 0;

            deviceHandle->CreateDepthStencilView(depthBuffer, &dsvDesc, dsvHandle);

            dsvHandle.Offset(dsvDescriptorSize);

            outBuffers[i] = AllocTypeForEntry(depthBuffer, D12RESOURCEHANDLE);
        }

        return 0;
    }

    IDXGISwapChain4* CreateSwapChain(HWND hWnd, ID3D12CommandQueue* commandQueue, int width, int height, int bufferCount, DXGI_FORMAT format, UINT debug)
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


    EntryHandle CreateSwapChain(HWND hWnd, EntryHandle commandQueue, int width, int height, int bufferCount, DXGI_FORMAT format, UINT debug)
    {
        ID3D12CommandQueue* queue = (ID3D12CommandQueue*)GetAndValidateItem(commandQueue, D12QUEUE);

        IDXGISwapChain4* swcChain = CreateSwapChain(hWnd, queue, width, height, bufferCount, format, debug);

        return AllocTypeForEntry(swcChain, DXGISWAPCHAIN);
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

        commandList->Release();

        return commandList7;
    }

    EntryHandle CreateCommandList(EntryHandle commandAllocator, D3D12_COMMAND_LIST_TYPE type)
    {
        ID3D12CommandAllocator* commAllocator = (ID3D12CommandAllocator*)GetAndValidateItem(commandAllocator, D12COMMANDPOOL);

        ID3D12GraphicsCommandList* commandList = CreateCommandList(deviceHandle, commAllocator, type);

        return AllocTypeForEntry(commandList, D12COMMANDBUFFER7);
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
        fenceEvent = CreateEvent(NULL, FALSE, FALSE, NULL);
        if (fenceEvent == INVALID_HANDLE_VALUE)
            printf("Failed to create fence event.\n");

        return fenceEvent;
    }

    EntryHandle CreateFenceObject()
    {
        D12Fence* fence = (D12Fence*)AllocFromDeviceStorage(sizeof(D12Fence), alignof(HANDLE));

        ID3D12Fence* fenceHandle = CreateFence(deviceHandle);

        fence->fenceEvent = CreateEventHandle();
        fence->fenceHandle = AllocTypeForEntry(fenceHandle, D12FENCEHANDLE);
        fence->fenceValue = 0;

        return AllocTypeForEntry(fence, D12FENCEOBJECT);
    }

    uint64_t Signal(EntryHandle commandQueue, EntryHandle fenceObjIndex, uint64_t& fenceValue)
    {
        uint64_t fenceValueForSignal = ++fenceValue;

        D12Fence* fence = (D12Fence*)GetAndValidateItem(fenceObjIndex, D12FENCEOBJECT);

        ID3D12Fence* fenceHandle = (ID3D12Fence*)GetAndValidateItem(fence->fenceHandle, D12FENCEHANDLE);

        ID3D12CommandQueue* lQueueHandle = (ID3D12CommandQueue*)GetAndValidateItem(commandQueue, D12QUEUE);

        if (FAILED(lQueueHandle->Signal(fenceHandle, fenceValueForSignal)))
        {
            printf("Could not signal fence\n");
            return ~0ui64;
        }
        return fenceValueForSignal;

    }

    uint64_t Signal(EntryHandle commandQueue, EntryHandle fenceObjIndex)
    {

        ID3D12CommandQueue* lQueueHandle = (ID3D12CommandQueue*)GetAndValidateItem(commandQueue, D12QUEUE);

        D12Fence* fence = (D12Fence*)GetAndValidateItem(fenceObjIndex, D12FENCEOBJECT);

        ID3D12Fence* fenceHandle = (ID3D12Fence*)GetAndValidateItem(fence->fenceHandle, D12FENCEHANDLE);

        uint64_t fenceValueForSignal = ++fence->fenceValue;

        if (FAILED(lQueueHandle->Signal(fenceHandle, fenceValueForSignal)))
        {
            printf("Could not signal fence\n");
            return ~0ui64;
        }
        return fenceValueForSignal;

    }

    void WaitForFenceValue(EntryHandle fenceObjIndex, uint64_t fenceValue, DWORD duration)
    {

        D12Fence* fence = (D12Fence*)GetAndValidateItem(fenceObjIndex, D12FENCEOBJECT);

        ID3D12Fence* fenceHandle = (ID3D12Fence*)GetAndValidateItem(fence->fenceHandle, D12FENCEHANDLE);

        if (fenceHandle->GetCompletedValue() < fenceValue)
        {
            if (SUCCEEDED(fenceHandle->SetEventOnCompletion(fenceValue, fence->fenceEvent)))
            {
                WaitForSingleObject(fence->fenceEvent, duration);
            }
        }
    }

    void Flush(EntryHandle fenceObjIndex, EntryHandle commandQueue, uint64_t& fenceValue)
    {
        uint64_t fenceValueForSignal = Signal(commandQueue, fenceObjIndex, fenceValue);

        WaitForFenceValue(fenceObjIndex, fenceValueForSignal, UINT32_MAX);
    }


    void Flush(EntryHandle fenceObjIndex, EntryHandle commandQueue)
    {
        uint64_t fenceValueForSignal = Signal(commandQueue, fenceObjIndex);

        WaitForFenceValue(fenceObjIndex, fenceValueForSignal, UINT32_MAX);
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


    EntryHandle CreateHostBuffer(UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags)
    {
 
        DriverMemoryBuffer* dmb = (DriverMemoryBuffer*)AllocFromDeviceStorage(sizeof(DriverMemoryBuffer), 4);

        dmb->sizeOfAlloc = size;
        dmb->currentPointer = 0;

        ID3D12Resource* resource = CreateBuffer(deviceHandle, size, format, flags, D3D12_HEAP_TYPE_UPLOAD);

        dmb->bufferHandle = AllocTypeForEntry(resource, D12RESOURCEHANDLE);

        dmb->currentState = dmb->initialState = D3D12_RESOURCE_STATE_GENERIC_READ;

        return AllocTypeForEntry(dmb, D12BUFFERMEMORYPOOL);

    }

    EntryHandle CreateDeviceBuffer(UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags)
    {
        DriverMemoryBuffer* dmb = (DriverMemoryBuffer*)AllocFromDeviceStorage(sizeof(DriverMemoryBuffer), 4);

        dmb->sizeOfAlloc = size;
        dmb->currentPointer = 0;

        ID3D12Resource* resource = CreateBuffer(deviceHandle, size, format, flags, D3D12_HEAP_TYPE_DEFAULT);

        dmb->bufferHandle = AllocTypeForEntry(resource, D12RESOURCEHANDLE);

        dmb->currentState = dmb->initialState = D3D12_RESOURCE_STATE_GENERIC_READ;

        return AllocTypeForEntry(dmb, D12BUFFERMEMORYPOOL);

    }

    ID3D12Resource* CreateBuffer(ID3D12Device2* device, UINT size, DXGI_FORMAT format, D3D12_RESOURCE_FLAGS flags, D3D12_HEAP_TYPE heapType)
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



};

