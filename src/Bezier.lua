--[[
	
Documentation: https://bstummer.github.io/bezier/

Version of this module: 1.0.0

Created by Vaschex

To use the GetExtrema function, you have to install this complex numbers
module and put it as the child of this module:
https://create.roblox.com/marketplace/asset/8152231789

Bezier © 2022 by Vaschex is licensed under CC BY-NC 4.0.
https://creativecommons.org/licenses/by-nc/4.0/

]]

--factorial
local function fac(n:number):number
	if n == 0 then return 1 end
	return n * fac(n - 1)
end

--[=[
    @class Bezier

    The class used to construct Bézier curves.
]=]
local Bezier = {}

--[=[
	@class BezierCurve

	Represents a Bézier curve.
]=]
local BezierCurve = {}
BezierCurve.__index = BezierCurve

--[=[
	@within BezierCurve
	@prop Points {Vector3}
	The points of the curve.
]=]

--[=[
	@within BezierCurve
	@prop Length number?
	The approximate length of the curve. Only exists if `UpdateLUT` has been called.
]=]

--[=[
	@within BezierCurve
	@prop LUT {number}?
	@private
	@readonly
	Internal lookup table for arc length parameterization.
]=]

--[=[
	@within BezierCurve
	@prop RMFLUT {{Vector3}}?
	@private
	@readonly
	Internal lookup table for calculating normal vectors.
]=]

--[=[
	Constructs a new BezierCurve from an array of points.
	Obviously, you should use two or more points.

	@param points -- The array of points
	@return BezierCurve
]=]
function Bezier.new(points:{Vector3})
	local self = {}

	self.Points = points

	return setmetatable(self, BezierCurve)
end

--[=[
	Constructs a new BezierCurve from a folder with Parts.
	The parts must have a number as their name (1, 2, 3, ...) so the order of the points is clear.

	@param folder -- The instance that has the Parts as children
	@return BezierCurve
]=]
function Bezier.fromParts(folder:Instance)
	local points = {}
	local parts = folder:GetChildren()
	table.sort(parts, function(a, b)
		return tonumber(a.Name) < tonumber(b.Name)
	end)
	for _, v in parts do
		table.insert(points, v.Position)
	end
	return Bezier.new(points)
end

--[=[
	Constructs a random BezierCurve.
	If `start` is a CFrame, the curve will go in its LookVector's direction, otherwise in a random direction.

	@param start -- The first point
	@param points -- The amount of points
	@param distanceBetweenPoints -- The approximate distance between points
	@param variation -- The smaller the variation, the more it will look like a straight line
	@return BezierCurve
]=]
function Bezier.random(
	start: CFrame | Vector3,
	points: number,
	distanceBetweenPoints: NumberRange,
	variation: number?
)
	variation = variation or 0.9
	if typeof(start) == "Vector3" then
		start = CFrame.new(start) * CFrame.fromEulerAnglesYXZ(
			math.random() * math.pi * 2 - math.pi,
			math.random() * math.pi * 2 - math.pi,
			math.random() * math.pi * 2 - math.pi
		)
	end
	local result = {start.Position}

	for i = 1, points - 1 do
		local distance = math.random(
			distanceBetweenPoints.Min, distanceBetweenPoints.Max
		)
		local range = distance * variation / 2
		local mid = start.LookVector * distance + start.Position
		local midCF = CFrame.new(mid.X, mid.Y, mid.Z, select(4, start:GetComponents()))
		local point = midCF * CFrame.new(
			math.random(-range, range),
			math.random(-range, range),
			0
		)
		table.insert(result, point.Position)
		local rot = CFrame.lookAt(start.Position, point.Position)
		start = CFrame.new(point.X, point.Y, point.Z, select(4, rot:GetComponents()))
	end

	return Bezier.new(result)
end

--[=[
	This is the recommended way to get a point on the curve. Uses De Casteljau's algorithm.

	@param t -- A number between 0 and 1
]=]
function BezierCurve:DeCasteljau(t:number):Vector3
	local copy = {unpack(self.Points)}
	local n = #copy
	for j = 1, n - 1 do
		for k = 1, n - j do
			copy[k] = copy[k] * (1 - t) + copy[k + 1] * t
		end
	end
	return copy[1]
end

--[=[
	Returns a point on the curve using the explicit definition of Bézier curves,
	which is a bit slower than De Casteljau's algorithm.

	@param t -- A number between 0 and 1
]=]
function BezierCurve:GetPoint(t:number):Vector3
	local result = Vector3.zero
	local points = self.Points
	local n = #points - 1
	for i = 0, n do
		result += (fac(n)/(fac(i)*fac(n-i))) * t^i * (1-t)^(n-i) * points[i+1]
	end
	return result
end

--[=[
	Calculates the polynomial form of the curve. Returns a function,
	which can be used to calculate a point on the curve, and the coefficients.
	Since much is already precalculated, the function is good to use when
	you have to calculate points very often in order to save performance.

	@return (t: number) -> (Vector3), {Vector3}
]=]
function BezierCurve:Polynomial()
	local coeff = {}
	local points = self.Points
	local n = #points - 1
	for j = 0, n do
		local c = Vector3.zero
		for i = 0, j do
			c += ((-1)^(i+j)*points[i+1]) / (fac(i)*fac(j-i))
		end
		coeff[j] = c * fac(n)/fac(n-j)
	end

	return function(t:number):Vector3
		local result = Vector3.zero
		for j = 0, n do
			result += t^j * coeff[j]
		end
		return result
	end, coeff
end

--[=[
	Updates internal lookup tables. This function should be called after you modified
	the points of the curve, since these tables won't be accurate anymore.
]=]
function BezierCurve:UpdateLUT(steps:number?, calcNormals:boolean?)
	steps = steps or 101
	local points = self.Points
	local lut = {}
	local len = 0

	--[[
		LUT:
		stores lengths
	]]
	local last
	for i = 0, steps - 1 do
		local t = i / (steps - 1)
		local point = self:DeCasteljau(t)
		if i ~= 0 then
			len += (last - point).Magnitude
		end
		lut[i] = len
		last = point
	end

	--[[
		RMFLUT (rotation minimising frames):
		1 = point
		2 = derivative
		3 = normal vector
		4 = cross product of normal and derivative
	]]
	self.LUT = lut
	self.Length = len

	if calcNormals or self.RMFLUT then
		local dt = self:GetDerivative(-0.001)
		local r = (dt + self:GetSecondDerivative(-0.001)):Cross(dt).Unit
		local first = {
			self:DeCasteljau(0),
			dt,
			r,
			r:Cross(dt)
		}

		local frames = {[0] = first}
		local steps = 100
		for i = 0, steps - 1 do
			local x0 = frames[i]
			local t = (i + 1) / steps
			local x1 = {
				self:DeCasteljau(t),
				self:GetDerivative(t)
			}
			local v1 = x1[1] - x0[1]
			local c1 = v1:Dot(v1)
			local riL = x0[3] - (v1 * 2 / c1 * v1:Dot(x0[3]))
			local dtiL = x0[2] - (v1 * 2 / c1 * v1:Dot(x0[2]))

			local v2 = x1[2] - dtiL

			x1[3] = riL - (v2 * 2 / v2:Dot(v2) * v2:Dot(riL))
			x1[4] = x1[3]:Cross(x1[2])
			frames[i + 1] = x1
		end
		self.RMFLUT = frames
	end
end

--[=[
	Usually the points are not evenly distributed along the curve and the t value is not
	equal to the length of the curve. Using arc length parameterization, the function converts a length
	into the time t at which this length occurs.

	@param t -- A number between 0 and 1
]=]
function BezierCurve:ConvertT(t:number):number
	--[[
	local function Speed(v)
		return self:GetDerivative(v).Magnitude
	end
	
	local iterations = 50
	local t = 0
	local h = s / iterations
	for i = 1, iterations do
		local k1 = h / Speed(t)
		local k2 = h / Speed(t + k1 / 2)
		local k3 = h / Speed(t + k2 / 2)
		local k4 = h / Speed(t + k3)
		t += (k1 + 2 * (k2 + k3) + k4) / 6
	end
	return t
	]]

	if not self.Length then self:UpdateLUT() end
	local lookup = self.LUT
	local targetLength = self.Length * t

	--find the largest length that's smaller than targetLength
	local largestIndex = 0
	local largestValue = 0
	for i, v in lookup do
		if v <= targetLength and v > largestValue then
			largestIndex = i
			largestValue = v
		end
	end

	if largestValue == targetLength then
		return largestIndex / #lookup
	else
		if lookup[largestIndex + 1] then
			return (largestIndex + (targetLength - largestValue) /
				(lookup[largestIndex + 1] - largestValue)) / #lookup
		else
			return (largestIndex + (targetLength - largestValue) /
				(lookup[largestIndex - 1] - largestValue)) / #lookup
		end
	end
end

--[=[
	Returns the first derivative of the curve at the time t.
	This is basically the direction in which the point is looking.

	@param t -- A number between 0 and 1
]=]
function BezierCurve:GetDerivative(t:number):Vector3
	local result = Vector3.zero
	local points = self.Points
	local n = #points - 2
	for i = 0, n do
		result += (fac(n)/(fac(i)*fac(n-i))) * t^i * (1-t)^(n-i)
			* (points[i+2] - points[i+1])
	end
	return result * (n+1)
end

--[=[
	Returns the second derivative of the curve at the time t.

	@param t -- A number between 0 and 1
]=]
function BezierCurve:GetSecondDerivative(t:number):Vector3
	local result = Vector3.zero
	local points = self.Points
	local n = #points - 3
	for i = 0, n do
		result += (fac(n)/(fac(i)*fac(n-i))) * t^i * (1-t)^(n-i) *
			((n+2)*(n+1) * (points[i+3] - 2 * points[i+2] + points[i+1]))
	end
	return result
end

--[=[
	Calculates the curve of a derivative.
	`BezierCurve:GetDerivative(t).Unit ≈ BezierCurve:CreateDerivativeCurve():GetPoint(t).Unit`

	@param k -- The number of the derivative, default is 1
	@return BezierCurve
]=]
function BezierCurve:CreateDerivativeCurve(k:number?)
	k = k or 1
	local points = self.Points
	local new = {}
	for i = 1, #points - k do
		local p = Vector3.zero
		for j = 0, k do
			p += (-1)^(k-j) * (fac(k)/(fac(j)*fac(k-j))) * points[i+j]
		end
		table.insert(new, p)
	end
	return Bezier.new(new)
end

--[=[
	Iterates from t = 0 to 1 in the given amount of steps and passes the t value
	in a function in each step.
	By default, it uses [BezierCurve:DeCasteljau]. This may be useful
	to visualize the path of the curve or other things.

	@param steps -- Number of iterations
]=]
function BezierCurve:GetIterations(steps:number, func:(number)->(Vector3)?):{Vector3}
	func = func or BezierCurve.DeCasteljau
	local iterations = {}
	
	for i = 0, steps - 1 do
		local t = i / (steps - 1)
		table.insert(iterations, func(self, t))
	end
	
	return iterations
end

--[=[
	Subdivides the curve into two other curves.

	@param t -- Position where it divides
	@return BezierCurve, BezierCurve
]=]
function BezierCurve:Subdivide(t:number?)
	t = t or 0.5
	local left = {}
	local right = {}

	local points = {unpack(self.Points)}
	local n = #points
	for j = 1, n - 1 do
		for k = 1, n - j do
			if k == 1 then
				table.insert(left, points[1])
			elseif k == n - j then
				table.insert(right, points[k+1])
			end
			points[k] = points[k] * (1 - t) + points[k + 1] * t
		end
	end
	table.insert(left, points[1])
	table.insert(right, points[1])

	return Bezier.new(left), Bezier.new(right)
end

--[=[
	Calculates a new curve with an elevated degree (higher amount of points).
	The curve itself stays unchanged.

	@param times -- Number of elevations, default is 1
	@return BezierCurve
]=]
function BezierCurve:ElevateDegree(times:number?)
	times = times or 1
	local new
	for _ = 1, times do
		local old = new or self.Points
		new = {}
		local k = #old
		new[1] = old[1]
		new[k+1] = old[k]
		for i = 2, k do
			new[i] = (i-1) / k * old[i-1] + (k-i+1) / k * old[i]
		end
	end
	return Bezier.new(new)
end

local comp
if script:FindFirstChildWhichIsA("ModuleScript") then
	comp = require(script:FindFirstChildWhichIsA("ModuleScript"))
end

local function len(t)
	local l = 0
	for _ in t do
		l += 1
	end
	return l - 1
end

local function findRoots(a)
	local roots = {}
	for k = 0, len(a) - 1 do
		roots[k] = 0
	end
	local n = len(a)
	local img = comp.cmplx(0, 1)
	local rad = math.pow(math.abs(a[0] / a[n]), 1 / n)
	local theta = 2 * math.pi / n
	local offset = theta / (n + 1)
	for k = 0, n - 1 do
		roots[k] = comp.cmplx(rad, 0)
			* comp.exp(img * comp.cmplx(theta * k + offset, 0))
	end

	local old = {}
	for k = 0, n do
		old[k] = 0
	end
	local loop = 0
	local EPS = comp.cmplx(10e-15, 0)

	local diff = 0
	for i = 0, len(old) - 1 do
		diff += comp.pow(comp.abs(old[i] - roots[i]), 2)
	end
	diff = comp.sqrt(diff)

	while diff > EPS and loop < 10000 do
		loop += 1

		for i, v in roots do
			old[i] = comp.cmplx(v.Re, v.Im)
		end

		for index = 0, len(roots) do
			local n = len(a) + 1
			local q = comp.cmplx(0, 0)
			local p = comp.cmplx(a[n - 1], 0)
			for i = n - 2, 0, -1 do
				q = q * old[index] + p
				p = comp.cmplx(a[i], 0) + old[index] * p
			end
			local newt = p / q
			local beta = comp.cmplx(0, 0)
			for i = 0, len(old)-1 do
				if i ~= index then
					beta += 1 / (old[index] - old[i])
				end
			end
			roots[index] = roots[index] - newt/(1 - newt * beta)
		end

		diff = 0
		for i = 0, len(old) - 1 do
			diff += comp.pow(comp.abs(old[i] - roots[i]), 2)
		end
		diff = comp.sqrt(diff)
	end

	for k, v in roots do
		roots[k] = v.Re
	end
	return roots
end

--[=[
	Calculates the extrema (minimum and maximum) of the curve for every axis.
	They are returned as t values in an array, where the first value is the
	minimum and the second value the maxmimum.

	In order to use this function, you have to install this complex numbers module
	and put it as the child of this module:
	https://create.roblox.com/marketplace/asset/8152231789

	This is due to the fact that a numerical root-finding algorithm has to be used.
	In the future, I will try to remove the requirement to install it.
]=]
function BezierCurve:GetExtrema():{
	X: {Vector3},
	Y: {Vector3},
	Z: {Vector3}
	}
	if not comp then
		error("GetExtrema requires the installation of another module,"
			.." please check the Bézier module or docs for more information.")
	end

	local result = {}
	--local points = self.Points
	local _, coeff = self:CreateDerivativeCurve():Polynomial()

	for _, dim in {"X", "Y", "Z"} do
		local t1 = 0
		local t2 = 1
		local y1 = self:DeCasteljau(t1)[dim]
		local y2 = self:DeCasteljau(t2)[dim]

		local newCoeff = {}
		for i, v in coeff do
			newCoeff[i] = v[dim]
		end
		local roots = findRoots(newCoeff)

		for _, root in roots do
			if root > 0.01 and root < 0.99 then --if in interval
				local dp = self:DeCasteljau(root)[dim]
				if y2 < y1 then
					if dp < y2 then
						t2 = root
						y2 = dp
					end
					if dp > y1 then
						t1 = root
						y1 = dp
					end
				else
					if dp < y1 then
						t1 = root
						y1 = dp
					end
					if dp > y2 then
						t2 = root
						y2 = dp
					end
				end
			end
		end
		result[dim] = {t1, t2}
	end

	return result
end

--[=[
	Calculates the bounding box of the curve.
	By default, this is a fast approximation algorithm which returns
	the minimal and maximal coordinates (the corners) of an axis-aligned bounding box.
	Bounding boxes are typically used to make a quick exit from an algorithm
	to avoid doing more detailed computations. For this, they don't need to be minimal.
	
	However, if `minimal` is true, it returns the minimal and maximal coordinates
	of a minimal axis-aligned bounding box.

	If `rotated` is true, it returns the CFrame and size,
	plus the minimal and maximal coordinates of a minimal rotated bounding box.
	This isn't the smallest bounding box possible, but I wasn't motivated to implement
	a better algorithm because it is a lot of effort.

	@param minimal -- Determines if the bounding box is minimal or approximate
	@param rotated -- Determines if the minimal bounding box is rotated or axis-aligned
]=]
function BezierCurve:GetBoundingBox(minimal:boolean?, rotated:boolean?):
	(Vector3|CFrame, Vector3, Vector3?, Vector3?)

	if not minimal then
		local minX = math.huge
		local maxX = -math.huge
		local minY = minX
		local maxY = maxX
		local minZ = minX
		local maxZ = maxX

		for _, point in self.Points do
			if point.X < minX then minX = point.X end
			if point.X > maxX then maxX = point.X end
			if point.Y < minY then minY = point.Y end
			if point.Y > maxY then maxY = point.Y end
			if point.Z < minZ then minZ = point.Z end
			if point.Z > maxZ then maxZ = point.Z end
		end

		return Vector3.new(minX, minY, minZ), Vector3.new(maxX, maxY, maxZ)
	else
		if not rotated then
			local extrema = self:GetExtrema()

			return Vector3.new(
				self:DeCasteljau(extrema.X[1]).X,
				self:DeCasteljau(extrema.Y[1]).Y,
				self:DeCasteljau(extrema.Z[1]).Z
			), Vector3.new(
				self:DeCasteljau(extrema.X[2]).X,
				self:DeCasteljau(extrema.Y[2]).Y,
				self:DeCasteljau(extrema.Z[2]).Z
			)
		else
			local aligned = {}
			local first = self.Points[1]
			for _, v in self.Points do
				table.insert(aligned, v - first)
			end
			local n = #aligned
			local a = math.atan2(aligned[n].Z, aligned[n].X)
			for k, v in aligned do
				aligned[k] = Vector3.new(
					v.X * math.cos(-a) - v.Z * math.sin(-a),
					v.Y,
					v.X * math.sin(-a) + v.Z * math.cos(-a)
				)
			end
			aligned = Bezier.new(aligned)

			local extrema = aligned:GetExtrema()

			--determine box bounds
			local minX = aligned:DeCasteljau(extrema.X[1]).X
			local maxX = aligned:DeCasteljau(extrema.X[2]).X
			local minY = aligned:DeCasteljau(extrema.Y[1]).Y
			local maxY = aligned:DeCasteljau(extrema.Y[2]).Y
			local minZ = aligned:DeCasteljau(extrema.Z[1]).Z
			local maxZ = aligned:DeCasteljau(extrema.Z[2]).Z

			local size = Vector3.new(
				math.abs(maxX - minX),
				math.abs(maxY - minY),
				math.abs(maxZ - minZ)
			)

			local mid = Vector3.new(
				(minX + maxX) / 2,
				(minY + maxY) / 2,
				(minZ + maxZ) / 2
			)

			local lookAt = (Vector3.new(0, 0, -size.Z) + mid * 2) / 2

			mid = Vector3.new(
				first.X + mid.X * math.cos(a) - mid.Z * math.sin(a),
				first.Y + mid.Y,
				first.Z + mid.X * math.sin(a) + mid.Z * math.cos(a)
			)
			lookAt = Vector3.new(
				first.X + lookAt.X * math.cos(a) - lookAt.Z * math.sin(a),
				first.Y + lookAt.Y,
				first.Z + lookAt.X * math.sin(a) + lookAt.Z * math.cos(a)
			)

			return CFrame.lookAt(mid, lookAt),
				size,
				Vector3.new(minX, minY, minZ),
				Vector3.new(maxX, maxY, maxZ)
		end
	end
end

--[=[
	Returns 4 vectors at the given t value: the point, the derivative,
	the normal vector and the cross product of normal and derivative vector.
	This is ideal to construct a CFrame.

	@param t -- A number between 0 and 1
]=]
function BezierCurve:GetNormal(t:number):{
	o: Vector3,
	dt: Vector3,
	r: Vector3,
	n: Vector3
	}

	--[[
		RMFLUT (rotation minimising frames):
		1 = point
		2 = derivative
		3 = normal vector
		4 = cross product of normal and derivative
	]]

	--generate LUT
	if not self.RMFLUT then
		self:UpdateLUT(nil, true)
	end

	local lut = self.RMFLUT
	local last = len(lut)
	local f =  t * last
	local i = math.floor(f)

	if f == i then
		return {
			o = lut[i][1],
			dt = lut[i][2],
			r = lut[i][3],
			n = lut[i][4]
		}
	end

	local j = i + 1
	local ti = i / last
	local ratio = (t - ti) / (j / last - ti)
	local result = {}
	for idx, type_ in {"o", "dt", "r", "n"} do
		result[type_] = lut[i][idx]:Lerp(lut[j][idx], ratio)
	end
	return result
end

local function deepCopy(t)
	local copy = {}
	for k, v in t do
		if type(v) == "table" then
			v = deepCopy(v)
		end
		copy[k] = v
	end
	return copy
end

--[=[
	Clones the curve.

	@return BezierCurve
]=]
function BezierCurve:Clone()
	return setmetatable(deepCopy(self), BezierCurve)
end

--[=[
	Destroys the curve.
]=]
function BezierCurve:Destroy()
	table.clear(self)
	setmetatable(self, nil)
end

return Bezier