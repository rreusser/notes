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

// TypeScript declarations for @stdlib/lapack/base/dlange

/**
* Interface describing `dlange`.
*/
interface Routine {
	/**
	* Computes the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a real matrix.
	*
	* @param order - storage layout
	* @param norm - norm type: 'max', 'one-norm', 'inf-norm', or 'frobenius'
	* @param M - number of rows
	* @param N - number of columns
	* @param A - input matrix
	* @param LDA - leading dimension of A
	* @param WORK - workspace array
	* @param strideWORK - stride length for WORK
	* @returns norm value
	*/
	(
		order: string,
		norm: string,
		M: number,
		N: number,
		A: Float64Array,
		LDA: number,
		WORK: Float64Array,
		strideWORK: number
	): number;

	/**
	* Computes the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a real matrix using alternative indexing semantics.
	*
	* @param norm - norm type: 'max', 'one-norm', 'inf-norm', or 'frobenius'
	* @param M - number of rows
	* @param N - number of columns
	* @param A - input matrix
	* @param strideA1 - stride of the first dimension of A
	* @param strideA2 - stride of the second dimension of A
	* @param offsetA - starting index for A
	* @param WORK - workspace array
	* @param strideWORK - stride length for WORK
	* @param offsetWORK - starting index for WORK
	* @returns norm value
	*/
	ndarray(
		norm: string,
		M: number,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): number;
}

/**
* Computes the value of the one norm, Frobenius norm, infinity norm, or largest absolute value of a real matrix.
*/
declare var dlange: Routine;

export = dlange;
