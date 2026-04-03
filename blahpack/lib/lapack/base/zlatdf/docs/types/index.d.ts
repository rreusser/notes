

// TypeScript declarations for @stdlib/lapack/base/zlatdf

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes a contribution to the reciprocal Dif-estimate using the LU factorization computed by zgetc2.
	*/
	(
		ijob: number,
		N: number,
		Z: Float64Array,
		strideZ1: number,
		strideZ2: number,
		offsetZ: number,
		RHS: Float64Array,
		strideRHS: number,
		offsetRHS: number,
		rdsum: number,
		rdscal: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		JPIV: Int32Array,
		strideJPIV: number,
		offsetJPIV: number
	): Float64Array;
}

/**
* Computes a contribution to the reciprocal Dif-estimate using the LU factorization computed by zgetc2.
*/
declare var zlatdf: Routine;

export = zlatdf;
