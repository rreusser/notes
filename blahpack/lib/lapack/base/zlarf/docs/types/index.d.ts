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

// TypeScript declarations for @stdlib/lapack/base/zlarf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Apply a complex Householder reflector
	*/
	(
		side: string,
		M: number,
		N: number,
		v: Float64Array,
		strideV: number,
		offsetV: number,
		tau: any,
		C: Float64Array,
		strideC1: number,
		strideC2: number,
		offsetC: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Apply a complex Householder reflector
*/
declare var zlarf: Routine;

export = zlarf;
