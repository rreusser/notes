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

// TypeScript declarations for @stdlib/lapack/base/zgghrd

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Reduce a pair of complex matrices to generalized upper Hessenberg form.
	*/
	(
		compq: string,
		compz: string,
		N: number,
		ilo: number,
		ihi: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		Q: Float64Array,
		strideQ1: number,
		strideQ2: number,
		offsetQ: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number
	): Float64Array;
}

/**
* Reduce a pair of complex matrices to generalized upper Hessenberg form.
*/
declare var zgghrd: Routine;

export = zgghrd;
