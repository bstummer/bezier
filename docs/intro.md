---
sidebar_position: 1
---

# Introduction

This is an advanced Bézier curves module for Luau, which supports Vector3 curves of any degree.
It provides all functions that you need for game development.

Although it encompasses many features, it is very lightweight. It won't waste any calculations on things you don't use.

## Installation

Get the module [here](https://example.com/) and insert into your game via the Toolbox.
You can also just paste this in the command bar:
```lua
game:GetObjects("rbxassetid://...")[1].Parent=game.ReplicatedStorage
```

## How to create a Bézier curve

There are multiple constructors:

`Bezier.new` is the default one, which simply requires an array of Vector3 points.

`Bezier.fromParts` can be used to create a curve from a folder of Parts. The parts must have a number as their name (1, 2, 3, ...) so the order of the points is clear.

`Bezier.random` allows you to generate a random curve based on the generation settings.

```lua
local Bezier = require(game.ReplicatedStorage.Bezier)

local curve = Bezier.fromParts(workspace.BezierCurve)
```

## Get a point on the curve

:::tip
For testing and visualization purposes, you can get my visualization module [here](https://example.com/).
:::

The recommended way to calculate a point on the curve is `BezierCurve:DeCasteljau` (named after De Casteljau's algorithm).

It takes a `t` value of 0 to 1 as input, where 0 is the first point of the curve and 1 the last.

You can visualize the curve with this code:
```lua
local Bezier = require(game.ReplicatedStorage.Bezier)
local curve = Bezier.fromParts(workspace.BezierCurve)

local function createPoint(pos: Vector3, color: Color3?)
	local part = Instance.new("Part")
	part.Anchored = true
	part.Color = color or Color3.fromRGB(170, 0, 0)
	part.Shape = Enum.PartType.Ball
	part.Position = pos
	part.Size = Vector3.new(1, 1, 1)
	part.Name = "Point"
	part.TopSurface = Enum.SurfaceType.Smooth
	part.BottomSurface = Enum.SurfaceType.Smooth
	part.Parent = workspace
end

local iterations = 50
for i = 0, iterations do
	local t = i / iterations
	createPoint(curve:DeCasteljau(t))
end
```

## Arc Length Parameterization

If you visualize the path of the curve with the code above, you will notice that the points don't have an equal distance to each other. They are not evenly distributed although the points are spread at equal intervals.

This means that t = 0.5 is usually not at 50% of the Bézier curve's length.

This can be solved approximately using arc length parameterization. I created the function `ConvertT`, which takes a length as input and returns the time t at which this length occurs.

Now we can distribute the points evenly along the curve:
```lua
local iterations = 50
for i = 0, iterations do
	local t = i / iterations
	local convertedT = curve:ConvertT(t)
	createPoint(curve:DeCasteljau(convertedT))
end
```

** Continue to the API section [here](https://example.com/)!**

## Things to consider

- There are no checks of the arguments you pass in functions. You should look at the type annotations and understand what the function does. Furthermore, you can look at the source code.
- The use of `GetExtrema` requires the installation of a complex numbers module, more on that in the API section.
- It is always possible to modify the control points. However, if you use the functions `ConvertT`, `GetNormal` or the property `Length`, you have to call `BezierCurve:UpdateLUT` after the modifications. This is due to the fact that the internal lookup tables won't be accurate anymore.