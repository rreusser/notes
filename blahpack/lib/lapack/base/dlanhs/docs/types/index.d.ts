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

// TypeScript declarations for @stdlib/lapack/base/dlanhs

/**
* Interface describing the ndarray method.
*/
interface NdarrayMethod {
	/**
	* Returns the value of the one norm, Frobenius norm, infinity norm, or the largest absolute value of a real upper Hessenberg matrix using alternative indexing semantics.
	*
	* @param norm - norm type: 'max', 'one-norm', 'inf-norm', or 'frobenius'
	* @param N - order of the matrix
	* @param A - upper Hessenberg matrix
	* @param strideA1 - stride of the first dimension of A
	* @param strideA2 - stride of the second dimension of A
	* @param offsetA - starting index for A
	* @param WORK - workspace array
	* @param strideWORK - stride for WORK
	* @param offsetWORK - starting index for WORK
	* @returns matrix norm value
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
	): number;
}

/**
* Interface describing `dlanhs`.
*/
interface Routine {
	/**
	* Returns the value of the one norm, Frobenius norm, infinity norm, or the largest absolute value of a real upper Hessenberg matrix.
	*
	* @param order - storage layout ('row-major' or 'column-major')
	* @param norm - norm type: 'max', 'one-norm', 'inf-norm', or 'frobenius'
	* @param N - order of the matrix
	* @param A - upper Hessenberg matrix
	* @param LDA - leading dimension of A
	* @param WORK - workspace array
	* @param strideWORK - stride for WORK
	* @returns matrix norm value
	*/
	(
		order: string,
		norm: string,
		N: number,
		A: Float64Array,
		LDA: number,
		WORK: Float64Array,
		strideWORK: number
	): number;

	/**
	* Returns the value of the one norm, Frobenius norm, infinity norm, or the largest absolute value of a real upper Hessenberg matrix using alternative indexing semantics.
	*/
	ndarray: NdarrayMethod;
}

/**
* Returns the value of the one norm, Frobenius norm, infinity norm, or the largest absolute value of a real upper Hessenberg matrix.
*/
declare var dlanhs: Routine;

export = dlanhs;
