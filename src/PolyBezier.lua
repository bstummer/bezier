--[[
	
Documentation: https://bstummer.github.io/bezier/

Version of this module: 1.0.0

Created by Vaschex

PolyBezier © 2022 by Vaschex is licensed under CC BY-NC 4.0.
https://creativecommons.org/licenses/by-nc/4.0/

]]

--[=[
	@class PolyBezier

	The class used to construct PolyBézier curves.

	PolyBézier curves, or composite Bézier curves, are multiple
	Bézier curves that are joined end to end. This module guarantees
	a C¹ continuity between the curves, which means that the position
	and derivative are the same in the connection points.

	The module can be installed [here](https://www.roblox.com/library/11603069195).
]=]
local PolyBezier = {}

--[=[
    @class PolyBezierCurve

    Represents a composite Bézier curve.
]=]
local PolyBezierCurve = {}
PolyBezierCurve.__index = PolyBezierCurve

--[=[
	@within PolyBezierCurve
	@prop Curves {BezierCurve?}
	The points of the curve.
]=]

--[=[
	@within PolyBezierCurve
	@prop Length number?
	The approximate length of the curve. Only exists if `GetLength` has been called.
]=]

--[=[
	Constructs a new [PolyBezierCurve] from an array of [BezierCurve]s.
	The array is optional, the class doesn't necessarily has to have curves added to it.

	@param curves {BezierCurve}? -- The optional array of curves
	@return PolyBezierCurve
]=]
function PolyBezier.new(curves:{any}?)
	local self = setmetatable({}, PolyBezierCurve)

	if curves then
		self.Curves = {curves[1]}

		if #curves > 1 then
			for i = 2, #curves do
				self:AddCurve(curves[i])
			end
		end
	else
		self.Curves = {}
	end

	return self
end

--[=[
	Calculates the total length of all curves.
]=]
function PolyBezierCurve:GetLength():number
	local len = 0
	for _, v in self.Curves do
		if not v.Length then v:UpdateLUT() end
		len += v.Length
	end
	self.Length = len
	return len
end

--[=[
	Returns all points of the curves, the connection points only get
	get returned once.
]=]
function PolyBezierCurve:GetAllPoints():{Vector3}
	local points = {}
	for i, v in self.Curves do
		for j, p in v.Points do
			if i > 1 and j == 1 then continue end
			table.insert(points, p)
		end
	end
	return points
end

--[=[
	Adds a new curve at the end. Take into consideration that the first two points
	of the new curve may get changed in order to guarantee a smooth join.

	@param curve BezierCurve -- The curve to be added
]=]
function PolyBezierCurve:AddCurve(curve:any)
	if #self.Curves > 0 then
		local points = curve.Points
		local lastPoints = self.Curves[#self.Curves].Points
		local p0 = lastPoints[#lastPoints]
		points[1] = p0
		points[2] = 2 * p0 - lastPoints[#lastPoints - 1]
	end
	table.insert(self.Curves, curve)
	if self.Length then
		self:GetLength() --update length
	end
end

--[=[
	Calculates a point on the whole composite curve using the time t.
	This uses arc length parameterization, so t = 0.25 is actually
	guaranteed to be approximately at 25% of the length.
]=]
function PolyBezierCurve:GetPoint(t:number):Vector3
	local targetLength = (self.Length or self:GetLength()) * t
	
	local curve
	local len = 0
	for i, v in self.Curves do
		if len + v.Length < targetLength then
			len += v.Length
		else
			curve = v
			break
		end
	end
	
	return curve:DeCasteljau(curve:ConvertT((targetLength - len) / curve.Length))
end

--[=[
	Given a t value of the whole composite curve, it returns the Bézier curve
	and a t value for it which are at the same position. This is essentially
	what `GetPoint` does internally, but this can be used for many other functions.

	@return BezierCurve, number
]=]
function PolyBezierCurve:GetCurveAndT(t:number):(any, number)
	local targetLength = (self.Length or self:GetLength()) * t

	local curve
	local len = 0
	for i, v in self.Curves do
		if len + v.Length < targetLength then
			len += v.Length
		else
			curve = v
			break
		end
	end

	return curve, curve:ConvertT((targetLength - len) / curve.Length)
end

--[=[
	Fast approximation algorithm which returns the minimal and maximal coordinates
	(the corners) of an axis-aligned bounding box.
]=]
function PolyBezierCurve:GetBoundingBox():(Vector3, Vector3)
	local minX = math.huge
	local maxX = -math.huge
	local minY = minX
	local maxY = maxX
	local minZ = minX
	local maxZ = maxX

	for _, v in self.Curves do
		for _, point in v.Points do
			if point.X < minX then minX = point.X end
			if point.X > maxX then maxX = point.X end
			if point.Y < minY then minY = point.Y end
			if point.Y > maxY then maxY = point.Y end
			if point.Z < minZ then minZ = point.Z end
			if point.Z > maxZ then maxZ = point.Z end
		end
	end

	return Vector3.new(minX, minY, minZ), Vector3.new(maxX, maxY, maxZ)
end

return PolyBezier