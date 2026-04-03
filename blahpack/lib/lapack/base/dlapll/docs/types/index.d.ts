/*
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

// TypeScript declarations for @stdlib/lapack/base/dlapll

/**
* Interface describing `dlapll`.
*/
interface Routine {
	/**
	* Measures the linear dependence of two vectors X and Y by computing the
	* smallest singular value of the N-by-2 matrix A = ( X Y ).
	*
	* @param N - length of the vectors
	* @param x - first input vector (overwritten on exit)
	* @param strideX - stride for x
	* @param y - second input vector (overwritten on exit)
	* @param strideY - stride for y
	* @param ssmin - output array; on exit, ssmin[0] contains the smallest singular value
	*/
	( N: number, x: Float64Array, strideX: number, y: Float64Array, strideY: number, ssmin: Float64Array ): void;

	/**
	* Measures the linear dependence of two vectors using alternative indexing semantics.
	*
	* @param N - length of the vectors
	* @param x - first input vector (overwritten on exit)
	* @param strideX - stride for x
	* @param offsetX - starting index for x
	* @param y - second input vector (overwritten on exit)
	* @param strideY - stride for y
	* @param offsetY - starting index for y
	* @param ssmin - output array; on exit, ssmin[0] contains the smallest singular value
	*/
	ndarray( N: number, x: Float64Array, strideX: number, offsetX: number, y: Float64Array, strideY: number, offsetY: number, ssmin: Float64Array ): void;
}

/**
* Measures the linear dependence of two vectors.
*/
declare var dlapll: Routine;

export = dlapll;
