"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[69],{4985:e=>{e.exports=JSON.parse('{"functions":[{"name":"DeCasteljau","desc":"This is the recommended way to get a point on the curve. Uses De Casteljau\'s algorithm.","params":[{"name":"t","desc":"A number between 0 and 1","lua_type":"number"}],"returns":[{"desc":"","lua_type":"Vector3\\r\\n"}],"function_type":"method","source":{"line":152,"path":"src/Bezier.lua"}},{"name":"GetPoint","desc":"Returns a point on the curve using the explicit definition of B\xe9zier curves,\\nwhich is a bit slower than De Casteljau\'s algorithm.","params":[{"name":"t","desc":"A number between 0 and 1","lua_type":"number"}],"returns":[{"desc":"","lua_type":"Vector3\\r\\n"}],"function_type":"method","source":{"line":169,"path":"src/Bezier.lua"}},{"name":"Polynomial","desc":"Calculates the polynomial form of the curve. Returns a function,\\nwhich can be used to calculate a point on the curve, and the coefficients.\\nSince much is already precalculated, the function is good to use when\\nyou have to calculate points very often in order to save performance.","params":[],"returns":[{"desc":"","lua_type":"(t: number) -> (Vector3), {Vector3}"}],"function_type":"method","source":{"line":187,"path":"src/Bezier.lua"}},{"name":"UpdateLUT","desc":"Updates internal lookup tables. This function should be called after you modified\\nthe points of the curve, since these tables won\'t be accurate anymore.","params":[{"name":"steps","desc":"","lua_type":"number?"},{"name":"calcNormals","desc":"","lua_type":"boolean?"}],"returns":[],"function_type":"method","source":{"line":212,"path":"src/Bezier.lua"}},{"name":"ConvertT","desc":"Usually the points are not evenly distributed along the curve and the t value is not\\nequal to the length of the curve. Using arc length parameterization, the function converts a length\\ninto the time t at which this length occurs.","params":[{"name":"t","desc":"A number between 0 and 1","lua_type":"number"}],"returns":[{"desc":"","lua_type":"number\\r\\n"}],"function_type":"method","source":{"line":284,"path":"src/Bezier.lua"}},{"name":"GetDerivative","desc":"Returns the first derivative of the curve at the time t.\\nThis is basically the direction in which the point is looking.","params":[{"name":"t","desc":"A number between 0 and 1","lua_type":"number"}],"returns":[{"desc":"","lua_type":"Vector3\\r\\n"}],"function_type":"method","source":{"line":336,"path":"src/Bezier.lua"}},{"name":"GetSecondDerivative","desc":"Returns the second derivative of the curve at the time t.","params":[{"name":"t","desc":"A number between 0 and 1","lua_type":"number"}],"returns":[{"desc":"","lua_type":"Vector3\\r\\n"}],"function_type":"method","source":{"line":352,"path":"src/Bezier.lua"}},{"name":"CreateDerivativeCurve","desc":"Calculates the curve of a derivative.\\n`BezierCurve:GetDerivative(t).Unit \u2248 BezierCurve:CreateDerivativeCurve():GetPoint(t).Unit`","params":[{"name":"k","desc":"The number of the derivative, default is 1","lua_type":"number?"}],"returns":[{"desc":"","lua_type":"BezierCurve"}],"function_type":"method","source":{"line":370,"path":"src/Bezier.lua"}},{"name":"GetCurvature","desc":"Returns the curvature of the curve at the time t.","params":[{"name":"t","desc":"A number between 0 and 1","lua_type":"number"}],"returns":[{"desc":"","lua_type":"number\\r\\n"}],"function_type":"method","source":{"line":389,"path":"src/Bezier.lua"}},{"name":"GetIterations","desc":"Iterates from t = 0 to 1 in the given amount of steps and passes the t value\\nin a function in each step.\\nBy default, it uses [BezierCurve:DeCasteljau]. This may be useful\\nto visualize the path of the curve or other things.","params":[{"name":"steps","desc":"Number of iterations","lua_type":"number"},{"name":"func","desc":"","lua_type":"(number)->(Vector3)?"}],"returns":[{"desc":"","lua_type":"{Vector3}\\r\\n"}],"function_type":"method","source":{"line":402,"path":"src/Bezier.lua"}},{"name":"Subdivide","desc":"Subdivides the curve into two other curves.","params":[{"name":"t","desc":"Position where it divides","lua_type":"number?"}],"returns":[{"desc":"","lua_type":"BezierCurve, BezierCurve"}],"function_type":"method","source":{"line":420,"path":"src/Bezier.lua"}},{"name":"ElevateDegree","desc":"Calculates a new curve with an elevated degree (higher amount of points).\\nThe curve itself stays unchanged.","params":[{"name":"times","desc":"Number of elevations, default is 1","lua_type":"number?"}],"returns":[{"desc":"","lua_type":"BezierCurve"}],"function_type":"method","source":{"line":451,"path":"src/Bezier.lua"}},{"name":"GetExtrema","desc":"Calculates the extrema (minimum and maximum) of the curve for every axis.\\nThey are returned as t values in an array, where the first value is the\\nminimum and the second value the maxmimum.\\n\\nIn order to use this function, you have to install this complex numbers module\\nand put it as the child of this module:\\nhttps://create.roblox.com/marketplace/asset/8152231789\\n\\nThis is due to the fact that a numerical root-finding algorithm has to be used.\\nIn the future, I will try to remove the requirement to install it.","params":[],"returns":[{"desc":"","lua_type":"{\\r\\n\\tX: {number},\\r\\n\\tY: {number},\\r\\n\\tZ: {number}\\r\\n\\t}\\r\\n"}],"function_type":"method","source":{"line":558,"path":"src/Bezier.lua"}},{"name":"GetBoundingBox","desc":"Calculates the bounding box of the curve.\\nBy default, this is a fast approximation algorithm which returns\\nthe minimal and maximal coordinates (the corners) of an axis-aligned bounding box.\\nBounding boxes are typically used to make a quick exit from an algorithm\\nto avoid doing more detailed computations. For this, they don\'t need to be minimal.\\n\\nHowever, if `minimal` is true, it returns the minimal and maximal coordinates\\nof a minimal axis-aligned bounding box.\\n\\nIf `rotated` is true, it returns the CFrame and size,\\nplus the minimal and maximal coordinates of a minimal rotated bounding box.\\nThis isn\'t the smallest bounding box possible, but I wasn\'t motivated to implement\\na better algorithm because it is a lot of effort.","params":[{"name":"minimal","desc":"Determines if the bounding box is minimal or approximate","lua_type":"boolean?"},{"name":"rotated","desc":"Determines if the minimal bounding box is rotated or axis-aligned","lua_type":"boolean?"}],"returns":[{"desc":"","lua_type":"Vector3|CFrame"},{"desc":"","lua_type":"Vector3"},{"desc":"","lua_type":"Vector3?"},{"desc":"","lua_type":"Vector3?"}],"function_type":"method","source":{"line":632,"path":"src/Bezier.lua"}},{"name":"GetNormal","desc":"Returns 4 vectors at the given t value: the point, the derivative,\\nthe normal vector and the cross product of normal and derivative vector.\\nThis is ideal to construct a CFrame.","params":[{"name":"t","desc":"A number between 0 and 1","lua_type":"number"}],"returns":[{"desc":"","lua_type":"{\\r\\n\\to: Vector3,\\r\\n\\tdt: Vector3,\\r\\n\\tr: Vector3,\\r\\n\\tn: Vector3\\r\\n\\t}\\r\\n"}],"function_type":"method","source":{"line":733,"path":"src/Bezier.lua"}},{"name":"Clone","desc":"Clones the curve.","params":[],"returns":[{"desc":"","lua_type":"BezierCurve"}],"function_type":"method","source":{"line":793,"path":"src/Bezier.lua"}},{"name":"Destroy","desc":"Destroys the curve.","params":[],"returns":[],"function_type":"method","source":{"line":800,"path":"src/Bezier.lua"}}],"properties":[{"name":"Points","desc":"The points of the curve.","lua_type":"{Vector3}","source":{"line":44,"path":"src/Bezier.lua"}},{"name":"Length","desc":"The approximate length of the curve. Only exists if `UpdateLUT` has been called.","lua_type":"number?","source":{"line":50,"path":"src/Bezier.lua"}},{"name":"LUT","desc":"Internal lookup table for arc length parameterization.","lua_type":"{number}?","private":true,"readonly":true,"source":{"line":58,"path":"src/Bezier.lua"}},{"name":"RMFLUT","desc":"Internal lookup table for calculating normal vectors.","lua_type":"{{Vector3}}?","private":true,"readonly":true,"source":{"line":66,"path":"src/Bezier.lua"}}],"types":[],"name":"BezierCurve","desc":"Represents a B\xe9zier curve.","source":{"line":36,"path":"src/Bezier.lua"}}')}}]);