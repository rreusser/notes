/**
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

// TypeScript declarations for @stdlib/blas/base/zcopy

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Copy a complex double-precision vector
	*/
	(
		N: number,
		x: Float64Array,
		strideX: number,
		offsetX: number,
		incx: number,
		y: Float64Array,
		strideY: number,
		offsetY: number,
		incy: number
	): Float64Array;
}

/**
* Copy a complex double-precision vector
*/
declare var zcopy: Routine;

export = zcopy;
