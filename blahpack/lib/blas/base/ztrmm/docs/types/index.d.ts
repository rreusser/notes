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

// TypeScript declarations for @stdlib/blas/base/ztrmm

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Perform one of the matrix-matrix operations B := alpha*op(A)*B or B := alpha*B*op(A)
	*/
	(
		side: string,
		uplo: string,
		transa: string,
		diag: string,
		M: number,
		N: number,
		alpha: any,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number
	): Float64Array;
}

/**
* Perform one of the matrix-matrix operations B := alpha*op(A)*B or B := alpha*B*op(A)
*/
declare var ztrmm: Routine;

export = ztrmm;
