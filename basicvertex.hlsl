
cbuffer RootConstants : register(b0)
{
    float4x4 world;
}

struct Cam
{
    float4x4 proj;
    float4x4 view;
};


StructuredBuffer<Cam> myBuffer : register(t0);

struct VSInput
{
    uint vertexID : SV_VertexID;
};

struct VSOutput
{
    float4 position : SV_Position;
    float4 color : COLOR0;
};

VSOutput VertexMain(VSInput input)
{
    
    float4x4 identity = float4x4(
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0
);

    VSOutput outv;
    
    float2 positions[3] =
    {
        float2(0.0f, 0.5f),
        float2(0.5f, -0.5f),
        float2(-0.5f, -0.5f)
    };
    
    Cam cam = myBuffer[0];
   
    
    float4x4 viewProj = mul(cam.proj, cam.view);
    outv.position = mul(viewProj, mul(world, float4(positions[input.vertexID], 0.5f, 1.0f)));
    outv.color = transpose(cam.view)[2];
    return outv;
}

struct PSInput
{
    float4 position : SV_Position;
    float4 color : COLOR0;
};

float4 PixelShaderFunction(PSInput input) : SV_Target
{ 
    return float4(input.color.xyz, 1.0);
}