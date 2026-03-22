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

// TypeScript declarations for @stdlib/blas/base/izamax

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Find the index of the element having the maximum sum of absolute values of real and imaginary parts.
	*/
	(
		N: number,
		x: Float64Array,
		stride: number,
		offset: number,
		incx: number
	): Float64Array;
}

/**
* Find the index of the element having the maximum sum of absolute values of real and imaginary parts.
*/
declare var izamax: Routine;

export = izamax;
