# Advanced Bézier Curves for Luau
This is an advanced Bézier curves module for Luau, designed to support Vector3 curves of any degree. It is built to provide all the necessary functions for game development while remaining incredibly lightweight. The module ensures that calculations are never wasted on features you aren't actively using.

<br>

## Features
- Multiple Constructors: Create curves using standard arrays of Vector3 points, folders of numbered parts, or generate them randomly.
- Arc Length Parameterization: Distribute points evenly along a curve using the ConvertT function, solving the common issue of uneven point distribution.
- Composite Curves (PolyBézier): Join multiple curves end-to-end with guaranteed C¹ continuity, meaning the position and derivative seamlessly match at the connection points.
- Advanced Mathematics: Includes functionality for calculating derivatives, second derivatives, curvature, and even bounding boxes.
- Rotation Minimising Frames: Easily retrieve the normal vector and cross product for constructing CFrames using GetNormal.

<br>

## Installation
You can install the Bezier module directly from Roblox by grabbing it from the [library](https://www.roblox.com/library/11467361559) or running the following snippet in your command bar:

```lua
game:GetObjects("rbxassetid://11467361559")[1].Parent=game.ReplicatedStorage
```

<br>

## Quick Start
### Creating a Basic Curve

You can easily create a curve from a folder of parts (named sequentially like "1", "2", "3"):

```lua
local Bezier = require(game.ReplicatedStorage.Bezier)

local curve = Bezier.fromParts(workspace.BezierCurve)
```

### Visualizing the Curve
The recommended method for getting a point on your curve is `DeCasteljau`, which takes a t value between 0 and 1.

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

### Arc Length Parameterization

If you visualize the path of the curve with the code above, you will notice that the points might not have an equal distance to each other. They are not evenly distributed although the points are spread at equal intervals.

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

<br>

## Important Considerations
- Argument Validation: The module does not perform internal checks on the arguments you pass to its functions. You should rely on the type annotations and read the source code to understand expected inputs.
- Lookup Tables (LUT): It is possible to modify control points at runtime. However, if you do, you must manually call `BezierCurve:UpdateLUT()`. This is because functions like `ConvertT`, `GetNormal`, and the `Length` property rely on internal lookup tables that will become inaccurate after modifications.
- Extrema Calculations: Using the `GetExtrema` function requires a numerical root-finding algorithm. Therefore, you must install a separate complex numbers module and place it as a child of the Bezier module.

[**Check out the documentation website!**](https://bstummer.github.io/bezier/docs/intro)
