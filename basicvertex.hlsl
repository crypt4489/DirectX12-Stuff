

//dxc.exe -T ps_6_0 -E PixelShaderFunction basicvertex.hlsl -Fo PS.bin
//dxc.exe -T vs_6_0 -E VertexMain basicvertex.hlsl -Fo VS.bin

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

Texture2D myTexture : register(t1); // SRV bound to t0
SamplerState mySampler : register(s0); // Sampler bound to s0

struct VSInput
{
    float4 position : POSITION;
    float4 texcoords : TEXCOORD0;
    uint vertexID : SV_VertexID;
};

struct VSOutput
{
    float4 position : SV_Position;
    float2 tex : TEXCOORD0;
};

VSOutput VertexMain(VSInput input)
{
    /*
    float4x4 identity = float4x4(
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0
    );*/

    VSOutput outv;
   
    Cam cam = myBuffer[0];
   
    
    float4x4 viewProj = mul(cam.proj, cam.view);
    outv.position = mul(viewProj, mul(world, input.position));
    outv.tex = input.texcoords.xy;
    return outv;
}

struct PSInput
{
    float4 position : SV_Position;
    float2 tex : TEXCOORD0;
};

float4 PixelShaderFunction(PSInput input) : SV_Target
{ 
    float2 outc = input.tex;
    return myTexture.Sample(mySampler, outc);
}