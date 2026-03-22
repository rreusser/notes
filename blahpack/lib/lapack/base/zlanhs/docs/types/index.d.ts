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

// TypeScript declarations for @stdlib/lapack/base/zlanhs

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Return the value of the one norm, Frobenius norm, infinity norm, or max absolute value of an upper Hessenberg complex matrix
	*/
	(
		norm: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Return the value of the one norm, Frobenius norm, infinity norm, or max absolute value of an upper Hessenberg complex matrix
*/
declare var zlanhs: Routine;

export = zlanhs;
