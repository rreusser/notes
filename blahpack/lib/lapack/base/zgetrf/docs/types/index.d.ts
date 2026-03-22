

// TypeScript declarations for @stdlib/lapack/base/zgetrf

import { Complex128Array } from '@stdlib/array/complex128';

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes an LU factorization of a general complex M-by-N matrix using
	* partial pivoting with row interchanges.
	*/
	(
		M: number,
		N: number,
		A: Complex128Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number
	): number;
}

/**
* Computes an LU factorization of a general complex M-by-N matrix using
* partial pivoting with row interchanges.
*/
declare var zgetrf: Routine;

export = zgetrf;
